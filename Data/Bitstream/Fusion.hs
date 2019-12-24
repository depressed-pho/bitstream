{-# LANGUAGE
    CPP
  , UnicodeSyntax
  #-}
-- | Some functions currently missing from
-- "Data.Vector.Fusion.Stream".
module Data.Bitstream.Fusion
    ( genericLength
    , genericTake
    , genericDrop
    , genericIndex
    , genericReplicate
    , genericUnfoldrN
    , genericFindIndex
    , genericIndexed
    )
    where
import qualified Data.Bitstream.Fusion.Monadic as M
#if MIN_VERSION_vector(0,11,0)
import Data.Bifunctor (first)
import Data.Vector.Fusion.Bundle as B
import Data.Vector.Fusion.Bundle.Size
#else
import Data.Vector.Fusion.Stream
import Prelude.Unicode
#endif
import Prelude hiding (replicate)

{-# INLINE genericLength #-}
#if MIN_VERSION_vector(0,11,0)
genericLength ∷ Num n ⇒ Bundle v α → n
genericLength = fromIntegral . B.length
#else
genericLength ∷ Num n ⇒ Stream α → n
genericLength = unId ∘ M.genericLength
#endif

{-# INLINE genericTake #-}
#if MIN_VERSION_vector(0,11,0)
genericTake ∷ Integral n ⇒ n → Bundle v α → Bundle v α
genericTake n = inplace (M.genericTake n) toMax
#else
genericTake ∷ Integral n ⇒ n → Stream α → Stream α
genericTake = M.genericTake
#endif

{-# INLINE genericDrop #-}
#if MIN_VERSION_vector(0,11,0)
genericDrop ∷ Integral n ⇒ n → Bundle v α → Bundle v α
genericDrop n = inplace (M.genericDrop n) toMax
#else
genericDrop ∷ Integral n ⇒ n → Stream α → Stream α
genericDrop = M.genericDrop
#endif

{-# INLINE genericIndex #-}
#if MIN_VERSION_vector(0,11,0)
genericIndex ∷ (Integral n, Show n) ⇒ Bundle v α → n → α
genericIndex s i = s B.!! (fromIntegral i)
#else
genericIndex ∷ (Integral n, Show n) ⇒ Stream α → n → α
genericIndex s = unId ∘ M.genericIndex s
#endif

{-# INLINE genericReplicate #-}
#if MIN_VERSION_vector(0,11,0)
genericReplicate ∷ Integral n ⇒ n → α → Bundle v α
genericReplicate = B.replicate . fromIntegral
#else
genericReplicate ∷ Integral n ⇒ n → α → Stream α
genericReplicate = M.genericReplicate
#endif

{-# INLINE genericUnfoldrN #-}
#if MIN_VERSION_vector(0,11,0)
genericUnfoldrN ∷ Integral n ⇒ n → (β → Maybe (α, β)) → β → Bundle v α
genericUnfoldrN = B.unfoldrN . fromIntegral
#else
genericUnfoldrN ∷ Integral n ⇒ n → (β → Maybe (α, β)) → β → Stream α
genericUnfoldrN = M.genericUnfoldrN
#endif

{-# INLINE genericFindIndex #-}
#if MIN_VERSION_vector(0,11,0)
genericFindIndex ∷ Integral n ⇒ (α → Bool) → Bundle v α → Maybe n
genericFindIndex f = fmap fromIntegral . B.findIndex f
#else
genericFindIndex ∷ Integral n ⇒ (α → Bool) → Stream α → Maybe n
genericFindIndex f = unId ∘ M.genericFindIndex f
#endif

{-# INLINE genericIndexed #-}
#if MIN_VERSION_vector(0,11,0)
genericIndexed ∷ Integral n ⇒ Bundle v α → Bundle v (n, α)
genericIndexed = fmap (first fromIntegral) . B.indexed
#else
genericIndexed ∷ Integral n ⇒ Stream α → Stream (n, α)
genericIndexed = M.genericIndexed
#endif
