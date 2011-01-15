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
                  S.Done       → (# p, o #)
                  S.Skip    s' → consume p o s'
                  S.Yield x s'
                      | p < 8     → if x
                                     then consume (p+1) (o `setBit` p) s'
                                     else consume (p+1)  o             s'
                      | otherwise → error "packet overflow"

    {-# NOINLINE [1] empty #-}
    empty = Packet 0 0

    {-# NOINLINE [1] length #-}
    length (Packet n _) = fromIntegral n

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

    {-# NOINLINE [1] empty #-}
    empty = Packet 0 0

    {-# NOINLINE [1] length #-}
    length (Packet n _) = fromIntegral n

{-# RULES

"empty → fusible" [~1]
    empty = unstream (S.stream [])
"empty → unfused" [ 1]
    unstream (S.stream []) = empty

"length → fusible" [~1]
    ∀c. length c = S.genericLength (stream c)
"length → unfused" [ 1]
    ∀c. S.genericLength (stream c) = length c

  #-}

fromOctet ∷ Word8 → Packet d
fromOctet = Packet 8
{-# INLINE fromOctet #-}

toOctet ∷ Packet d → Word8
toOctet (Packet _ o) = o
{-# INLINE toOctet #-}
