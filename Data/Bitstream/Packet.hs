{-# LANGUAGE
    BangPatterns
  , EmptyDataDecls
  , FlexibleContexts
  , FlexibleInstances
  , RankNTypes
  , UnboxedTuples
  , UnicodeSyntax
  #-}
module Data.Bitstream.Packet
    ( Left
    , Right

    , Packet

    , full

    , fromOctet
    , toOctet
    )
    where
import Data.Bitstream.Generic
import Data.Bits
import qualified Data.List.Stream as L
import qualified Data.Stream as S
import Data.Word
import Foreign.Storable
import Prelude hiding (length, null)
import Prelude.Unicode

data Left
data Right

data Packet d = Packet {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Word8
    deriving (Eq, Show)

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

instance Bitstream (Packet Left) where
    {-# INLINE [0] pack #-}
    pack xs0 = case consume (-1) 0 xs0 of
                (# p, o #) → Packet (p+1) o
        where
          {-# INLINE consume #-}
          consume !p !o []      = (# p, o #)
          consume !p !o !(x:xs)
              | p < 8     = if x
                            then consume (p+1) (o `setBit` p) xs
                            else consume (p+1)  o             xs
              | otherwise = error "packet overflow"

    {-# INLINE [0] unpack #-}
    unpack (Packet n o) = L.unfoldr produce 0
        where
          {-# INLINE produce #-}
          produce ∷ Int → Maybe (Bool, Int)
          produce !p
              | p < n     = Just (o `testBit` p, p+1)
              | otherwise = Nothing

    {-# INLINE [0] stream #-}
    stream (Packet n o) = S.unfoldr produce 0
        where
          {-# INLINE produce #-}
          produce ∷ Int → Maybe (Bool, Int)
          produce !p
              | p < n     = Just (o `testBit` p, p+1)
              | otherwise = Nothing

    {-# INLINE [0] unstream #-}
    unstream (S.Stream next s0) = case consume (-1) 0 s0 of
                                    (# p, o #) → Packet (p+1) o
        where
          {-# INLINE consume #-}
          consume !p !o !s
              = case next s of
                  S.Yield x s'
                      | p < 8     → if x
                                     then consume (p+1) (o `setBit` p) s'
                                     else consume (p+1)  o             s'
                      | otherwise → error "packet overflow"
                  S.Skip s' → consume p o s'
                  S.Done    → (# p, o #)

    {-# INLINE empty #-}
    empty = Packet 0 0

    {-# INLINE singleton #-}
    singleton True  = Packet 1 1
    singleton False = Packet 1 0

    {-# INLINE cons #-}
    cons b p
        | full p    = packetOverflow
        | otherwise = b `unsafeConsL` p

    {-# INLINE snoc #-}
    snoc p b
        | full p    = packetOverflow
        | otherwise = p `unsafeSnocL` b

    {-# INLINE head #-}
    head (Packet 0 _) = packetEmpty
    head (Packet _ o) = o `testBit` 0

    {-# INLINE uncons #-}
    uncons (Packet 0 _) = Nothing
    uncons (Packet n o) = Just ( o `testBit` 0
                               , Packet (n-1) (o `shiftR` 1) )

    {-# INLINE last #-}
    last (Packet 0 _) = packetEmpty
    last (Packet n o) = o `testBit` (n-1)

    {-# INLINE tail #-}
    tail (Packet 0 _) = packetEmpty
    tail (Packet n o) = Packet (n-1) (o `shiftR` 1)

    {-# INLINE init #-}
    init (Packet 0 _) = packetEmpty
    init (Packet n o) = Packet (n-1) o

    {-# INLINE null #-}
    null (Packet 0 _) = True
    null _            = False

    {-# SPECIALISE length ∷ Packet Left → Int #-}
    length (Packet n _) = fromIntegral n
    {-# INLINE length #-}

    {-# INLINE and #-}
    and (Packet n o) = (0xff `shiftR` (8-n)) ≡ o

    {-# INLINE or #-}
    or (Packet _ o) = o ≢ 0

    {-# SPECIALISE replicate ∷ Int → Bool → Packet Left #-}
    replicate n b
        | n > 8     = packetOverflow
        | otherwise = let o = if b
                              then 0xFF `shiftR` (8 - fromIntegral n)
                              else 0
                      in
                        Packet (fromIntegral n) o
    {-# INLINE replicate #-}

    {-# SPECIALISE
        unfoldrN ∷ Int → (β → Maybe (Bool, β)) → β → (Packet Left, Maybe β) #-}
    unfoldrN n0 f β0
        | n0 > 8    = packetOverflow
        | otherwise = loop_unfoldrN n0 β0 (∅)
        where
          {-# INLINE loop_unfoldrN #-}
          loop_unfoldrN 0 β α = (α, Just β)
          loop_unfoldrN n β α
              = case f β of
                  Nothing      → (α, Nothing)
                  Just (a, β') → loop_unfoldrN (n-1) β' (α `unsafeSnocL` a)
    {-# INLINEABLE unfoldrN #-}

    {-# SPECIALISE take ∷ Int → Packet Left → Packet Left #-}
    take l (Packet _ o)
        = let n' = fromIntegral (min 8 l)
              o' = (0xFF `shiftR` (8-n')) .&. o
          in
            Packet n' o'
    {-# INLINE take #-}

    {-# SPECIALISE drop ∷ Int → Packet Left → Packet Left #-}
    drop l (Packet n o)
        = let d  = fromIntegral (min (fromIntegral n) l)
              n' = n-d
              o' = o `shiftR` d
          in
            Packet n' o'
    {-# INLINE drop #-}

instance Bitstream (Packet Right) where
    {-# INLINE [0] pack #-}
    pack xs0 = case consume 7 0 xs0 of
                 (# p, o #) → Packet (7-p) o
        where
          {-# INLINE consume #-}
          consume !p !o []      = (# p, o #)
          consume !p !o !(x:xs)
              | p > 0     = if x
                            then consume (p-1) (o `setBit` p) xs
                            else consume (p-1)  o             xs
              | otherwise = error "packet overflow"

    {-# INLINE [0] unpack #-}
    unpack (Packet n b) = L.unfoldr produce (n-1)
        where
          {-# INLINE produce #-}
          produce ∷ Int → Maybe (Bool, Int)
          produce !p
              | p > 0     = Just (b `testBit` p, p-1)
              | otherwise = Nothing

    {-# INLINE [0] stream #-}
    stream (Packet n b) = {-# CORE "Packet Right 'stream'" #-}
                          S.unfoldr produce (n-1)
        where
          {-# INLINE produce #-}
          produce ∷ Int → Maybe (Bool, Int)
          produce !p
              | p > 0     = Just (b `testBit` p, p-1)
              | otherwise = Nothing

    {-# INLINE [0] unstream #-}
    unstream (S.Stream next s0) = {-# CORE "Packet Right 'unstream'" #-}
                                  case consume 7 0 s0 of
                                    (# p, o #) → Packet (7-p) o
        where
          {-# INLINE consume #-}
          consume !p !o !s
              = case next s of
                  S.Done       → (# p, o #)
                  S.Skip    s' → consume p o s'
                  S.Yield x s'
                      | p > 0     → if x
                                     then consume (p-1) (o `setBit` p) s'
                                     else consume (p-1)  o             s'
                      | otherwise → error "packet overflow"

    {-# INLINE empty #-}
    empty = Packet 0 0

    {-# INLINE singleton #-}
    singleton True  = Packet 1 0x80
    singleton False = Packet 1 0x00

    {-# INLINE cons #-}
    cons b p
        | full p    = packetOverflow
        | otherwise = b `unsafeConsR` p

    {-# INLINE snoc #-}
    snoc p b
        | full p    = packetOverflow
        | otherwise = p `unsafeSnocR` b

    {-# INLINE head #-}
    head (Packet 0 _) = packetEmpty
    head (Packet _ o) = o `testBit` 7

    {-# INLINE uncons #-}
    uncons (Packet 0 _) = Nothing
    uncons (Packet n o) = Just ( o `testBit` 0x80
                               , Packet (n-1) (o `shiftL` 1) )

    {-# INLINE last #-}
    last (Packet 0 _) = packetEmpty
    last (Packet n o) = o `testBit` (8-n)

    {-# INLINE tail #-}
    tail (Packet 0 _) = packetEmpty
    tail (Packet n o) = Packet (n-1) (o `shiftL` 1)

    {-# INLINE init #-}
    init (Packet 0 _) = packetEmpty
    init (Packet n o) = Packet (n-1) o

    {-# INLINE null #-}
    null (Packet 0 _) = True
    null _            = False

    {-# SPECIALISE length ∷ Packet Right → Int #-}
    length (Packet n _) = fromIntegral n
    {-# INLINE length #-}

    {-# INLINE and #-}
    and (Packet n o) = (0xff `shiftL` (8-n)) ≡ o

    {-# INLINE or #-}
    or (Packet _ o) = o ≢ 0

    {-# SPECIALISE replicate ∷ Int → Bool → Packet Right #-}
    replicate n b
        | n > 8     = packetOverflow
        | otherwise = let o = if b
                              then 0xFF `shiftL` (8 - fromIntegral n)
                              else 0
                      in
                        Packet (fromIntegral n) o
    {-# INLINE replicate #-}

    {-# SPECIALISE
        unfoldrN ∷ Int → (β → Maybe (Bool, β)) → β → (Packet Right, Maybe β) #-}
    unfoldrN n0 f β0
        | n0 > 8    = packetOverflow
        | otherwise = loop_unfoldrN n0 β0 (∅)
        where
          {-# INLINE loop_unfoldrN #-}
          loop_unfoldrN 0 β α = (α, Just β)
          loop_unfoldrN n β α
              = case f β of
                  Nothing      → (α, Nothing)
                  Just (a, β') → loop_unfoldrN (n-1) β' (α `unsafeSnocR` a)
    {-# INLINEABLE unfoldrN #-}

    {-# SPECIALISE take ∷ Int → Packet Right → Packet Right #-}
    take n (Packet _ o)
        = let n' = fromIntegral (min 8 n)
              o' = (0xFF `shiftL` (8-n')) .&. o
          in
            Packet n' o'
    {-# INLINE take #-}

    {-# SPECIALISE drop ∷ Int → Packet Right → Packet Right #-}
    drop l (Packet n o)
        = let d  = fromIntegral (min (fromIntegral n) l)
              n' = n-d
              o' = o `shiftL` d
          in
            Packet n' o'
    {-# INLINE drop #-}

packetEmpty ∷ α
packetEmpty = error "Data.Bitstream.Packet: packet is empty"
{-# INLINE packetEmpty #-}

packetOverflow ∷ α
packetOverflow = error "Data.Bitstream.Packet: packet size overflow"
{-# INLINE packetOverflow #-}

full ∷ Packet d → Bool
full (Packet 8 _) = True
full _            = False
{-# INLINE full #-}

fromOctet ∷ Word8 → Packet d
fromOctet = Packet 8
{-# INLINE fromOctet #-}

toOctet ∷ Packet d → Word8
toOctet (Packet _ o) = o
{-# INLINE toOctet #-}

unsafeConsL ∷ Bool → Packet Left → Packet Left
unsafeConsL True  (Packet n o) = Packet (n+1) ((o `shiftL` 1) .|. 1)
unsafeConsL False (Packet n o) = Packet (n+1)  (o `shiftL` 1)
{-# INLINE unsafeConsL #-}

unsafeConsR ∷ Bool → Packet Right → Packet Right
unsafeConsR True  (Packet n o) = Packet (n+1) ((o `shiftR` 1) .|. 0x80)
unsafeConsR False (Packet n o) = Packet (n+1)  (o `shiftR` 1)
{-# INLINE unsafeConsR #-}

unsafeSnocL ∷ Packet Left → Bool → Packet Left
unsafeSnocL (Packet n o) True  = Packet (n+1) (o `setBit` n)
unsafeSnocL (Packet n o) False = Packet (n+1)  o
{-# INLINE unsafeSnocL #-}

unsafeSnocR ∷ Packet Right → Bool → Packet Right
unsafeSnocR (Packet n o) True  = Packet (n+1) (o `setBit` (7-n))
unsafeSnocR (Packet n o) False = Packet (n+1)  o
{-# INLINE unsafeSnocR #-}
