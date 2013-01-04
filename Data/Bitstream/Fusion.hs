{-# LANGUAGE
    UnicodeSyntax
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
import Data.Vector.Fusion.Stream
import Data.Vector.Fusion.Util
import Prelude hiding (replicate)
import Prelude.Unicode

genericLength ∷ Num n ⇒ Stream α → n
{-# INLINE genericLength #-}
genericLength = unId ∘ M.genericLength

genericTake ∷ Integral n ⇒ n → Stream α → Stream α
{-# INLINE genericTake #-}
genericTake = M.genericTake

genericDrop ∷ Integral n ⇒ n → Stream α → Stream α
{-# INLINE genericDrop #-}
genericDrop = M.genericDrop

genericIndex ∷ (Integral n, Show n) ⇒ Stream α → n → α
{-# INLINE genericIndex #-}
genericIndex s = unId ∘ M.genericIndex s

genericReplicate ∷ Integral n ⇒ n → α → Stream α
{-# INLINE genericReplicate #-}
genericReplicate = M.genericReplicate

genericUnfoldrN ∷ Integral n ⇒ n → (β → Maybe (α, β)) → β → Stream α
{-# INLINE genericUnfoldrN #-}
genericUnfoldrN = M.genericUnfoldrN

genericFindIndex ∷ Integral n ⇒ (α → Bool) → Stream α → Maybe n
{-# INLINE genericFindIndex #-}
genericFindIndex f = unId ∘ M.genericFindIndex f

genericIndexed ∷ Integral n ⇒ Stream α → Stream (n, α)
{-# INLINE genericIndexed #-}
genericIndexed = M.genericIndexed
