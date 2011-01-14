{-# LANGUAGE
    RankNTypes
  , UnicodeSyntax
  #-}
module Data.Bitstream.Generic
    ( Bitstream(..)
    )
    where
import qualified Data.Stream as S
import Prelude hiding (length)
import Prelude.Unicode

class Bitstream α where
    stream   ∷ α → S.Stream Bool
    unstream ∷ S.Stream Bool → α

    empty ∷ α
    empty = unstream (S.stream [])
    {-# INLINE empty #-}

    (∅) ∷ α
    (∅) = empty
    {-# INLINE (∅) #-}

    singleton ∷ Bool → α
    singleton = unstream ∘ S.stream ∘ flip (:) []
    {-# INLINE singleton #-}

    pack ∷ [Bool] → α
    pack = unstream ∘ S.stream
    {-# INLINE pack #-}

    unpack ∷ α → [Bool]
    unpack = S.unstream ∘ stream
    {-# INLINE unpack #-}

    cons ∷ Bool → α → α
    cons = (unstream ∘) ∘ (∘ stream) ∘ S.cons
    {-# INLINE cons #-}

    snoc ∷ α → Bool → α
    snoc = (unstream ∘) ∘ S.snoc . stream
    {-# INLINE snoc #-}

    -- | Note: the default implementation of 'length' is unsafe when
    -- the actual length of the stream cannot be represented with
    -- 'Int'.
    length ∷ Integral n ⇒ α → n
    length = fromIntegral ∘ S.length ∘ stream
    {-# INLINE length #-}

{-# RULES
"Bitstream stream/unstream fusion"
    ∀s. stream (unstream s) = s
  #-}
