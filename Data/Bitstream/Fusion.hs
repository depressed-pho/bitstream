{-# LANGUAGE
    UnicodeSyntax
  #-}
module Data.Bitstream.Fusion
    ( genericLength
    , genericTake
    , genericDrop
    , genericIndex
    , genericReplicate
    , genericUnfoldrN
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

genericIndex ∷ Integral n ⇒ Stream α → n → α
{-# INLINE genericIndex #-}
genericIndex s = unId ∘ M.genericIndex s

genericReplicate ∷ Integral n ⇒ n → α → Stream α
{-# INLINE genericReplicate #-}
genericReplicate = M.genericReplicate

genericUnfoldrN ∷ Integral n ⇒ n → (β → Maybe (α, β)) → β → Stream α
{-# INLINE genericUnfoldrN #-}
genericUnfoldrN = M.genericUnfoldrN
