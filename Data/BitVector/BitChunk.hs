{-# LANGUAGE
    EmptyDataDecls
  , FlexibleInstances
  , UnboxedTuples
  , UnicodeSyntax
  #-}
module Data.BitVector.BitChunk
    ( Chunk(..)
    , Left
    , Right

    , (∅)
    , singleton
    , fromOctet
    , toOctet

    , unsafeCons
    , unsafeSnoc
    , unsafeAppend
    , append
    , unsafeHead
    , unsafeLast
    , unsafeTail
    , unsafeInit
    , null
    , length

    , map
    )
    where
import Data.Bits
import Data.Word
import Prelude hiding (length, map, null)

data Chunk d = Chunk {-# UNPACK #-} !Int 
                     {-# UNPACK #-} !Word8
    deriving (Eq, Show)

data Left
data Right

class BitChunk α where
    singleton ∷ Bool → α

    unsafeCons   ∷ Bool → α → α
    unsafeSnoc   ∷ α → Bool → α
    unsafeAppend ∷ α → α → α
    unsafeHead   ∷ α → Bool
    unsafeLast   ∷ α → Bool
    unsafeTail   ∷ α → α

    map ∷ (Bool → Bool) → α → α

(∅) ∷ Chunk d
(∅) = Chunk 0 0

fromOctet ∷ Word8 → Chunk d
fromOctet = Chunk 8

toOctet ∷ Chunk d → Word8
toOctet (Chunk _ b) = b

instance BitChunk (Chunk Left) where
    singleton True  = Chunk 1 1
    singleton False = Chunk 1 0

    unsafeCons True  (Chunk n b) = Chunk (n+1) (setBit (b `shiftL` 1) 0)
    unsafeCons False (Chunk n b) = Chunk (n+1) (b `shiftL` 1)

    unsafeSnoc (Chunk n b) True  = Chunk (n+1) (setBit b n)
    unsafeSnoc (Chunk n b) False = Chunk (n+1) b

    unsafeAppend (Chunk n b) (Chunk n' b')
        = Chunk (n + n') ((b `shiftL` n') .|. b')

    unsafeHead (Chunk _ b)
        = testBit b 0

    unsafeLast (Chunk n b)
        = testBit b (n-1)

    unsafeTail (Chunk n b)
        = Chunk (n-1) (b `shiftR` 1)

    map f (Chunk n b) = Chunk n (map' b n)
        where
          map' ∷ Word8 → Int → Word8
          map' b' 0
              | f (testBit b' 0) = setBit   b' 0
              | otherwise        = clearBit b' 0
          map' b' n'
              | f (testBit b' n') = map' (setBit   b' n') (n'-1)
              | otherwise         = map' (clearBit b' n') (n'-1)

instance BitChunk (Chunk Right) where
    singleton True  = Chunk 1 0x80
    singleton False = Chunk 1 0

    unsafeCons True  (Chunk n b) = Chunk (n+1) (setBit (b `shiftR` 1) 7)
    unsafeCons False (Chunk n b) = Chunk (n+1) (b `shiftR` 1)

    unsafeSnoc (Chunk n b) True  = Chunk (n+1) (setBit b (7-n))
    unsafeSnoc (Chunk n b) False = Chunk (n+1) b

    unsafeAppend (Chunk n b) (Chunk n' b')
        = Chunk (n + n') (b .|. (b' `shiftR` n))

    unsafeHead (Chunk _ b)
        = testBit b 7

    unsafeLast (Chunk n b)
        = testBit b (7-n+1)

    unsafeTail (Chunk n b)
        = Chunk (n-1) (b `shiftL` 1)

    map f (Chunk n b) = Chunk n (map' b n)
        where
          map' ∷ Word8 → Int → Word8
          map' b' 0
              | f (testBit b' 7) = setBit   b' 7
              | otherwise        = clearBit b' 7
          map' b' n'
              | f (testBit b' (7-n')) = map' (setBit   b' (7-n')) (n'-1)
              | otherwise             = map' (clearBit b' (7-n')) (n'-1)

unsafeInit ∷ Chunk d → Chunk d
unsafeInit (Chunk n b) = Chunk (n-1) b

null ∷ Chunk d → Bool
null (Chunk 0 _) = True
null _           = False

{-# SPECIALISE length ∷ Chunk d → Int #-}
length ∷ Integral n ⇒ Chunk d → n
length (Chunk n _) = fromIntegral n

{-# SPECIALISE append ∷ Chunk Left  → Chunk Left  → (# Chunk Left , Maybe (Chunk Left ) #) #-}
{-# SPECIALISE append ∷ Chunk Right → Chunk Right → (# Chunk Right, Maybe (Chunk Right) #) #-}
append ∷ BitChunk α ⇒ α → α → (# α, Maybe α #)
append = error "FIXME"
