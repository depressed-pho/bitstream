{-# LANGUAGE
    BangPatterns
  , EmptyDataDecls
  , FlexibleContexts
  , FlexibleInstances
  , UnicodeSyntax
  #-}
-- | For internal use only.
module Data.Bitstream.Packet
    ( Left
    , Right

    , Packet

    , full

    , fromOctet
    , toOctet

    , packetLToR
    , packetRToL
    )
    where
import Data.Bitstream.Generic
import Data.Bits
import qualified Data.List as L
import Data.Ord
import qualified Data.Vector.Fusion.Stream as S
import Data.Vector.Fusion.Stream.Monadic (Stream(..), Step(..))
import Data.Vector.Fusion.Stream.Size
import Data.Vector.Fusion.Util
import Data.Word
import Foreign.Storable
import Prelude ( Bool(..), Eq(..), Int, Integral, Ord(..), Maybe(..)
               , Monad(..), Num(..), Show(..), ($!), error, fromIntegral
               , otherwise
               )
import Prelude.Unicode

-- | 'Left' bitstreams interpret an octet as a vector of bits whose
-- LSB comes first and MSB comes last e.g.
--
--   * 11110000 => [False, False, False, False, True, True , True , True]
--
--   * 10010100 => [False, False, True , False, True, False, False, True]
--
-- 'Bits' operations (like 'toBits') treat a 'Left' bitstream as a
-- little-endian integer.
data Left

-- | 'Right' bitstreams interpret an octet as a vector of bits whose
-- MSB comes first and LSB comes last e.g.
--
--   * 11110000 => [True, True , True , True, False, False, False, False]
--
--   * 10010100 => [True, False, False, True, False, True , False, False]
--
-- 'Bits' operations (like 'toBits') treat a 'Right' bitstream as a
-- big-endian integer.
data Right

-- | 'Packet's are strict 'Bitstream's having at most 8 bits.
data Packet d = Packet {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Word8
    deriving (Eq)

instance Storable (Packet d) where
    sizeOf _  = 2
    alignment = sizeOf
    {-# INLINE peek #-}
    peek p
        = do n ← peekByteOff p 0
             o ← peekByteOff p 1
             return $! Packet (fromIntegral (n ∷ Word8)) o
    {-# INLINE poke #-}
    poke p (Packet n o)
        = do pokeByteOff p 0 (fromIntegral n ∷ Word8)
             pokeByteOff p 1 o

instance Show (Packet Left) where
    {-# INLINEABLE show #-}
    show (Packet n0 o0)
        = L.concat
          [ "["
          , L.unfoldr go (n0, o0)
          , "←]"
          ]
        where
          {-# INLINE go #-}
          go (0, _) = Nothing
          go (n, o)
              | o `testBit` (n-1) = Just ('1', (n-1, o))
              | otherwise         = Just ('0', (n-1, o))

instance Show (Packet Right) where
    {-# INLINEABLE show #-}
    show (Packet n0 o0)
        = L.concat
          [ "[→"
          , L.unfoldr go (n0, o0)
          , "]"
          ]
        where
          {-# INLINE δ #-}
          δ ∷ Int
          δ = 7 - n0
          {-# INLINE go #-}
          go (0, _) = Nothing
          go (n, o)
              | o `testBit` (n+δ) = Just ('1', (n-1, o))
              | otherwise         = Just ('0', (n-1, o))

instance Ord (Packet Left) where
    {-# INLINE compare #-}
    px `compare` py
        = comparing packetLToR px py

instance Ord (Packet Right) where
    {-# INLINE compare #-}
    (Packet nx ox) `compare` (Packet ny oy)
        = compare
          (ox `shiftR` (8-nx))
          (oy `shiftR` (8-ny))

instance Bitstream (Packet Left) where
    {-# INLINE basicStream #-}
    basicStream (Packet n o)
        = {-# CORE "Packet Left stream" #-}
          Stream step 0 (Exact n)
        where
          {-# INLINE step #-}
          step !i
              | i ≥ n     = return Done
              | otherwise = return $! Yield (o `testBit` i) (i+1)

    {-# INLINE basicUnstream #-}
    basicUnstream (Stream step s0 sz)
        = {-# CORE "Packet Left unstream" #-}
          case upperBound sz of
            Just n
                | n ≤ 8     → unId (unsafeConsume s0 0 0)
                | otherwise → packetOverflow
            Nothing         → unId (safeConsume   s0 0 0)
        where
          {-# INLINE unsafeConsume #-}
          unsafeConsume s !i !o
              = do r ← step s
                   case r of
                     Yield True  s' → unsafeConsume s' (i+1) (o `setBit` i)
                     Yield False s' → unsafeConsume s' (i+1)  o
                     Skip        s' → unsafeConsume s'  i     o
                     Done           → return $! Packet i o
          {-# INLINE safeConsume #-}
          safeConsume s !i !o
              = do r ← step s
                   case r of
                     Yield b s'
                         | i < 8     → safeConsume s' (i+1) (if b
                                                             then o `setBit` i
                                                             else o)
                         | otherwise → packetOverflow
                     Skip    s'      → safeConsume s' i o
                     Done            → return $! Packet i o

    {-# INLINE basicCons #-}
    basicCons b p
        | full p    = packetOverflow
        | otherwise = b `unsafeConsL` p

    {-# INLINE basicSnoc #-}
    basicSnoc p b
        | full p    = packetOverflow
        | otherwise = p `unsafeSnocL` b

    {-# INLINE basicAppend #-}
    basicAppend (Packet nx ox) (Packet ny oy)
        | nx + ny > 8 = packetOverflow
        | otherwise   = Packet (nx + ny) (ox .|. (oy `shiftL` nx))

    {-# INLINE basicTail #-}
    basicTail (Packet 0 _) = emptyNotAllowed
    basicTail (Packet n o) = Packet (n-1) (o `shiftR` 1)

    {-# INLINE basicInit #-}
    basicInit (Packet 0 _) = emptyNotAllowed
    basicInit (Packet n o) = Packet (n-1) o

    {-# INLINE basicMap #-}
    basicMap f (Packet n o0) = Packet n (go 0 o0)
        where
          {-# INLINE go #-}
          go i o
              | i ≥ n             = o
              | f (o `testBit` i) = go (i+1) (o `setBit`   i)
              | otherwise         = go (i+1) (o `clearBit` i)

    {-# INLINE basicReverse #-}
    basicReverse (Packet n o)
        = Packet n (reverseBits o `shiftR` (8-n))

    {-# INLINE basicScanl #-}
    basicScanl = scanlPacket

    {-# INLINE basicTake #-}
    basicTake l (Packet n o)
        | l ≤ 0      = (∅)
        | otherwise
            = let n' = fromIntegral (min (fromIntegral n) l)
                  o' = (0xFF `shiftR` (8-n')) .&. o
              in
                Packet n' o'

    {-# INLINE basicDrop #-}
    basicDrop l (Packet n o)
        | l ≤ 0      = Packet n o
        | otherwise
            = let d  = fromIntegral (min (fromIntegral n) l)
                  n' = n-d
                  o' = o `shiftR` d
              in
                Packet n' o'

    {-# INLINE basicTakeWhile #-}
    basicTakeWhile = takeWhilePacket

    {-# INLINE basicDropWhile #-}
    basicDropWhile = dropWhilePacket

    {-# INLINE basicFilter #-}
    basicFilter = filterPacket

    {-# INLINEABLE basicFromNBits #-}
    basicFromNBits n β
        | n < 0     = (⊥)
        | n > 8     = packetOverflow
        | n ≡ 8     = Packet (fromIntegral n) (fromIntegral β)
        | otherwise = let n' ∷ Int
                          n' = fromIntegral n
                          o  ∷ Word8
                          o  = fromIntegral (β .&. ((1 `shiftL` n') - 1))
                      in Packet n' o

    {-# INLINE basicToBits #-}
    basicToBits = fromIntegral ∘ toOctet


instance Bitstream (Packet Right) where
    {-# INLINE basicStream #-}
    basicStream (Packet n o)
        = {-# CORE "Packet Right stream" #-}
          Stream step 0 (Exact n)
        where
          {-# INLINE step #-}
          step !i
              | i ≥ n     = return Done
              | otherwise = return $! Yield (o `testBit` (7-i)) (i+1)

    {-# INLINE basicUnstream #-}
    basicUnstream (Stream step s0 sz)
        = {-# CORE "Packet Right unstream" #-}
          case upperBound sz of
            Just n
                | n ≤ 8     → unId (unsafeConsume s0 0 0)
                | otherwise → packetOverflow
            Nothing         → unId (safeConsume   s0 0 0)
        where
          {-# INLINE unsafeConsume #-}
          unsafeConsume s i o
              = do r ← step s
                   case r of
                     Yield True  s' → unsafeConsume s' (i+1) (o `setBit` (7-i))
                     Yield False s' → unsafeConsume s' (i+1)  o
                     Skip        s' → unsafeConsume s'  i     o
                     Done           → return $! Packet i o
          {-# INLINE safeConsume #-}
          safeConsume s i o
              = do r ← step s
                   case r of
                     Yield b s'
                         | i < 8     → safeConsume s' (i+1) (if b
                                                             then o `setBit` (7-i)
                                                             else o)
                         | otherwise → packetOverflow
                     Skip    s'      → safeConsume s' i o
                     Done            → return $! Packet i o

    {-# INLINE basicCons #-}
    basicCons b p
        | full p    = packetOverflow
        | otherwise = b `unsafeConsR` p

    {-# INLINE basicSnoc #-}
    basicSnoc p b
        | full p    = packetOverflow
        | otherwise = p `unsafeSnocR` b

    {-# INLINE basicAppend #-}
    basicAppend (Packet nx ox) (Packet ny oy)
        | nx + ny > 8 = packetOverflow
        | otherwise   = Packet (nx + ny) (ox .|. (oy `shiftR` nx))

    {-# INLINE basicTail #-}
    basicTail (Packet 0 _) = emptyNotAllowed
    basicTail (Packet n o) = Packet (n-1) (o `shiftL` 1)

    {-# INLINE basicInit #-}
    basicInit (Packet 0 _) = emptyNotAllowed
    basicInit (Packet n o) = Packet (n-1) o

    {-# INLINE basicMap #-}
    basicMap f (Packet n o0) = Packet n (go 0 o0)
        where
          {-# INLINE go #-}
          go i o
              | i ≥ n                 = o
              | f (o `testBit` (7-i)) = go (i+1) (o `setBit`   (7-i))
              | otherwise             = go (i+1) (o `clearBit` (7-i))

    {-# INLINE basicReverse #-}
    basicReverse (Packet n o)
        = Packet n (reverseBits o `shiftL` (8-n))

    {-# INLINE basicScanl #-}
    basicScanl = scanlPacket

    {-# INLINE basicTake #-}
    basicTake l (Packet n o)
        | l ≤ 0      = (∅)
        | otherwise
            = let n' = fromIntegral (min (fromIntegral n) l)
                  o' = (0xFF `shiftL` (8-n')) .&. o
              in
                Packet n' o'

    {-# INLINE basicDrop #-}
    basicDrop l (Packet n o)
        | l ≤ 0      = Packet n o
        | otherwise
            = let d  = fromIntegral (min (fromIntegral n) l)
                  n' = n-d
                  o' = o `shiftL` d
              in
                Packet n' o'

    {-# INLINE basicTakeWhile #-}
    basicTakeWhile = takeWhilePacket

    {-# INLINE basicDropWhile #-}
    basicDropWhile = dropWhilePacket

    {-# INLINE basicFilter #-}
    basicFilter = filterPacket

    {-# INLINEABLE basicFromNBits #-}
    basicFromNBits n β
        | n < 0     = (⊥)
        | n > 8     = packetOverflow
        | n ≡ 8     = Packet (fromIntegral n) (fromIntegral β)
        | otherwise = let n' ∷ Int
                          n' = fromIntegral n
                          o  ∷ Word8
                          o  = fromIntegral ( (β .&. ((1 `shiftL` n') - 1))
                                              `shiftL`
                                              (8-n')
                                            )
                      in Packet n' o

    {-# INLINE basicToBits #-}
    basicToBits = fromIntegral ∘ toOctet -- THINKME: Is this correct?

packetHeadL ∷ Packet Left → Bool
{-# RULES "head → packetHeadL" [1] head = packetHeadL #-}
{-# INLINE packetHeadL #-}
packetHeadL (Packet 0 _) = emptyNotAllowed
packetHeadL (Packet _ o) = o `testBit` 0

packetHeadR ∷ Packet Right → Bool
{-# RULES "head → packetHeadR" [1] head = packetHeadR #-}
{-# INLINE packetHeadR #-}
packetHeadR (Packet 0 _) = emptyNotAllowed
packetHeadR (Packet _ o) = o `testBit` 7

packetLastL ∷ Packet Left → Bool
{-# RULES "last → packetLastL" [1] last = packetLastL #-}
{-# INLINE packetLastL #-}
packetLastL (Packet 0 _) = emptyNotAllowed
packetLastL (Packet n o) = o `testBit` (n-1)

packetLastR ∷ Packet Right → Bool
{-# RULES "head → packetLastR" [1] last = packetLastR #-}
{-# INLINE packetLastR #-}
packetLastR (Packet 0 _) = emptyNotAllowed
packetLastR (Packet n o) = o `testBit` (8-n)

packetAndL ∷ Packet Left → Bool
{-# RULES "and → packetAndL" [1] and = packetAndL #-}
{-# INLINE packetAndL #-}
packetAndL (Packet n o) = (0xFF `shiftR` (8-n)) ≡ o

packetAndR ∷ Packet Right → Bool
{-# RULES "and → packetAndR" [1] and = packetAndR #-}
{-# INLINE packetAndR #-}
packetAndR (Packet n o) = (0xFF `shiftL` (8-n)) ≡ o

packetIndexL ∷ Integral n ⇒ Packet Left → n → Bool
{-# RULES "(!!) → packetIndexL" [1] (!!) = packetIndexL #-}
{-# INLINE packetIndexL #-}
packetIndexL p i
    | i < 0 ∨ i ≥ length p = indexOutOfRange i
    | otherwise            = unsafePacketIndexL p i

packetIndexR ∷ Integral n ⇒ Packet Right → n → Bool
{-# RULES "(!!) → packetIndexR" [1] (!!) = packetIndexR #-}
{-# INLINE packetIndexR #-}
packetIndexR p i
    | i < 0 ∨ i ≥ length p = indexOutOfRange i
    | otherwise            = unsafePacketIndexR p i

unsafePacketIndexL ∷ Integral n ⇒ Packet Left → n → Bool
{-# INLINE unsafePacketIndexL #-}
unsafePacketIndexL (Packet _ o) i
    = o `testBit` fromIntegral i

unsafePacketIndexR ∷ Integral n ⇒ Packet Right → n → Bool
{-# INLINE unsafePacketIndexR #-}
unsafePacketIndexR (Packet _ o) i
    = o `testBit` (7 - fromIntegral i)

packetNull ∷ Packet d → Bool
{-# RULES "null → packetNull" [1] null = packetNull #-}
{-# INLINE packetNull #-}
packetNull (Packet 0 _) = True
packetNull _            = False

packetLength ∷ Num n ⇒ Packet d → n
{-# RULES "length → packetLength" [1] length = packetLength #-}
{-# INLINE packetLength #-}
packetLength (Packet n _) = fromIntegral n

packetOr ∷ Packet d → Bool
{-# RULES "or → packetOr" [1] or = packetOr #-}
{-# INLINE packetOr #-}
packetOr (Packet _ o) = o ≢ 0

{-# INLINE emptyNotAllowed #-}
emptyNotAllowed ∷ α
emptyNotAllowed = error "Data.Bitstream.Packet: packet is empty"

{-# INLINE packetOverflow #-}
packetOverflow ∷ α
packetOverflow = error "Data.Bitstream.Packet: packet size overflow"

{-# INLINE indexOutOfRange #-}
indexOutOfRange ∷ Integral n ⇒ n → α
indexOutOfRange n = error ("Data.Bitstream.Packet: index out of range: " L.++ show n)

-- | /O(1)/ @'full' p == 'True'@ iff @'length' p == 8@, otherwise it
-- returns 'False'.
full ∷ Packet d → Bool
{-# INLINE full #-}
full (Packet 8 _) = True
full _            = False

-- | /O(1)/ Convert an octet to 'Packet'.
fromOctet ∷ Word8 → Packet d
{-# INLINE fromOctet #-}
fromOctet = Packet 8

-- | /O(1)/ 'toOctet' @p@ converts a 'Packet' @p@ to an octet, padding
-- with zeroes if @'length' p < 8@.
toOctet ∷ Packet d → Word8
{-# INLINE toOctet #-}
toOctet (Packet _ o) = o

{-# INLINE unsafeConsL #-}
unsafeConsL ∷ Bool → Packet Left → Packet Left
unsafeConsL True  (Packet n o) = Packet (n+1) ((o `shiftL` 1) .|. 1)
unsafeConsL False (Packet n o) = Packet (n+1)  (o `shiftL` 1)

{-# INLINE unsafeConsR #-}
unsafeConsR ∷ Bool → Packet Right → Packet Right
unsafeConsR True  (Packet n o) = Packet (n+1) ((o `shiftR` 1) .|. 0x80)
unsafeConsR False (Packet n o) = Packet (n+1)  (o `shiftR` 1)

{-# INLINE unsafeSnocL #-}
unsafeSnocL ∷ Packet Left → Bool → Packet Left
unsafeSnocL (Packet n o) True  = Packet (n+1) (o `setBit` n)
unsafeSnocL (Packet n o) False = Packet (n+1)  o

{-# INLINE unsafeSnocR #-}
unsafeSnocR ∷ Packet Right → Bool → Packet Right
unsafeSnocR (Packet n o) True  = Packet (n+1) (o `setBit` (7-n))
unsafeSnocR (Packet n o) False = Packet (n+1)  o

-- | /O(1)/ Change the direction of 'Packet' from 'Left' to
-- 'Right'. Bit directions only affect octet-based operations such as
-- 'toOctet'.
packetLToR ∷ Packet Left → Packet Right
{-# INLINE packetLToR #-}
packetLToR (Packet n o) = Packet n (reverseBits o)

-- | /O(1)/ Change the direction of 'Packet' from 'Right' to
-- 'Left'. Bit directions only affect octet-based operations such as
-- 'toOctet'.
packetRToL ∷ Packet Right → Packet Left
{-# INLINE packetRToL #-}
packetRToL (Packet n o) = Packet n (reverseBits o)

{-# INLINE reverseBits #-}
reverseBits ∷ Word8 → Word8
reverseBits x
    = ((x .&. 0x01) `shiftL` 7) .|.
      ((x .&. 0x02) `shiftL` 5) .|.
      ((x .&. 0x04) `shiftL` 3) .|.
      ((x .&. 0x08) `shiftL` 1) .|.
      ((x .&. 0x10) `shiftR` 1) .|.
      ((x .&. 0x20) `shiftR` 3) .|.
      ((x .&. 0x40) `shiftR` 5) .|.
      ((x .&. 0x80) `shiftR` 7)

{-# INLINE scanlPacket #-}
scanlPacket ∷ Bitstream (Packet d) ⇒ (Bool → Bool → Bool) → Bool → Packet d → Packet d
scanlPacket f b
    = unstream ∘ S.scanl f b ∘ stream

{-# INLINEABLE takeWhilePacket #-}
takeWhilePacket ∷ Bitstream (Packet d) ⇒ (Bool → Bool) → Packet d → Packet d
takeWhilePacket f α = take (go 0 ∷ Int) α
    where
      {-# INLINE go #-}
      go i | i ≥ length α = i
           | f (α !! i)   = go (i+1)
           | otherwise    = i

{-# INLINEABLE dropWhilePacket #-}
dropWhilePacket ∷ Bitstream (Packet d) ⇒ (Bool → Bool) → Packet d → Packet d
dropWhilePacket f α = drop (go 0 ∷ Int) α
    where
      {-# INLINE go #-}
      go i | i ≥ length α = i
           | f (α !! i)   = go (i+1)
           | otherwise    = i

filterPacket ∷ Bitstream (Packet d) ⇒ (Bool → Bool) → Packet d → Packet d
{-# INLINE filterPacket #-}
filterPacket f = unstream ∘ S.filter f ∘ stream
