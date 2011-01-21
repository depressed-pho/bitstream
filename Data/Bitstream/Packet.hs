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

    , fromOctet
    , toOctet
    )
    where
import Data.Bitstream.Generic
import Data.Bits
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

    {-# NOINLINE [1] cons #-}
    cons b p@(Packet n _)
        | n ≥ 8     = packetOverflow
        | otherwise = b `unsafeConsL` p

    {-# NOINLINE [1] snoc #-}
    snoc p@(Packet n _) b
        | n ≥ 8     = packetOverflow
        | otherwise = p `unsafeSnocL` b

    {-# NOINLINE [1] head #-}
    head (Packet 0 _) = packetEmpty
    head (Packet _ o) = o `testBit` 0

    {-# INLINE uncons #-}
    uncons (Packet 0 _) = Nothing
    uncons (Packet n o) = Just ( o `testBit` 0
                               , Packet (n-1) (o `shiftR` 1) )

    {-# SPECIALISE length ∷ Packet Left → Int #-}
    length (Packet n _) = fromIntegral n
    {-# NOINLINE [1] length #-}

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
                  Just (a, β') → loop_unfoldrN (n-1) β' (a `unsafeConsL` α)
    {-# INLINE unfoldrN #-}

instance Bitstream (Packet Right) where
    {-# INLINE [0] stream #-}
    stream (Packet n b) = S.unfoldr produce (n-1)
        where
          {-# INLINE produce #-}
          produce ∷ Int → Maybe (Bool, Int)
          produce !p
              | p > 0     = Just (b `testBit` p, p-1)
              | otherwise = Nothing

    {-# INLINE [0] unstream #-}
    unstream (S.Stream next s0) = case consume 7 0 s0 of
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

    {-# NOINLINE [1] cons #-}
    cons b p@(Packet n _)
        | n ≥ 8     = packetOverflow
        | otherwise = b `unsafeConsR` p

    {-# INLINE uncons #-}
    uncons (Packet 0 _) = Nothing
    uncons (Packet n o) = Just ( o `testBit` 0x80
                               , Packet (n-1) (o `shiftL` 1) )

    {-# NOINLINE [1] snoc #-}
    snoc p@(Packet n _) b
        | n ≥ 8     = packetOverflow
        | otherwise = p `unsafeSnocR` b

    {-# NOINLINE [1] head #-}
    head (Packet 0 _) = packetEmpty
    head (Packet _ o) = o `testBit` 7

    {-# SPECIALISE length ∷ Packet Right → Int #-}
    length (Packet n _) = fromIntegral n
    {-# NOINLINE [1] length #-}

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
                  Just (a, β') → loop_unfoldrN (n-1) β' (a `unsafeConsR` α)
    {-# INLINE unfoldrN #-}

packetEmpty ∷ α
packetEmpty = error "Data.Bitstream.Packet: packet is empty"
{-# INLINE packetEmpty #-}

packetOverflow ∷ α
packetOverflow = error "Data.Bitstream.Packet: packet size overflow"
{-# INLINE packetOverflow #-}

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
