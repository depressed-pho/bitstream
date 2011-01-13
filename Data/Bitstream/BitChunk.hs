{-# LANGUAGE
    BangPatterns
  , EmptyDataDecls
  , FlexibleContexts
  , FlexibleInstances
  , RankNTypes
  , UnboxedTuples
  , UnicodeSyntax
  #-}
module Data.Bitstream.BitChunk
    ( Chunk(..)
    , Left
    , Right

    , stream
    , unstream

    , fromOctet
    , toOctet

    , length
    , null
    )
    where
import Data.Bits
import qualified Data.Stream as S
import Data.Word
import Prelude hiding (length, null)

data Chunk d = Chunk {-# UNPACK #-} !Int 
                     {-# UNPACK #-} !Word8
    deriving (Eq, Show)

data Left
data Right

class BitChunk α where
    stream   ∷ α → S.Stream Bool
    unstream ∷ S.Stream Bool → α
{-# RULES
"BitChunk stream/unstream fusion"
    ∀s. stream (unstream s) = s
  #-}

instance BitChunk (Chunk Left) where
    {-# INLINE [0] stream #-}
    stream (Chunk n o) = S.unfoldr produce 0
        where
          {-# INLINE produce #-}
          produce ∷ Int → Maybe (Bool, Int)
          produce !p
              | p < n     = Just (o `testBit` p, p+1)
              | otherwise = Nothing

    {-# INLINE [0] unstream #-}
    unstream (S.Stream next s0) = case consume (-1) 0 s0 of
                                    (# p, o #) → Chunk (p+1) o
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
                      | otherwise → error "bitchunk overflow"

instance BitChunk (Chunk Right) where
    {-# INLINE [0] stream #-}
    stream (Chunk n b) = S.unfoldr produce (n-1)
        where
          {-# INLINE produce #-}
          produce ∷ Int → Maybe (Bool, Int)
          produce !p
              | p > 0     = Just (b `testBit` p, p-1)
              | otherwise = Nothing

    {-# INLINE [0] unstream #-}
    unstream (S.Stream next s0) = case consume 7 0 s0 of
                                    (# p, o #) → Chunk (7-p) o
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
                      | otherwise → error "bitchunk overflow"

fromOctet ∷ Word8 → Chunk d
fromOctet = Chunk 8
{-# INLINE fromOctet #-}

toOctet ∷ Chunk d → Word8
toOctet (Chunk _ o) = o
{-# INLINE toOctet #-}

length ∷ BitChunk (Chunk d) ⇒ Chunk d → Int
length (Chunk n _) = n
{-# NOINLINE [1] length #-}
{-# RULES
"length → fusible" [~1]
    ∀c. length c = S.length (stream c)
"length → unfused" [ 1]
    ∀c. S.length (stream c) = length c
  #-}

null ∷ BitChunk (Chunk d) ⇒ Chunk d → Bool
null (Chunk 0 _) = True
null _           = False
{-# NOINLINE [1] null #-}
{-# RULES
"null → fusible" [~1]
    ∀c. null c = S.null (stream c)
"null → unfused" [ 1]
    ∀c. S.null (stream c) = null c
  #-}
