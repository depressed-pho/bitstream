{-# LANGUAGE
    UnicodeSyntax
  #-}
module Data.Bitstream.Fusion
    ( genericLength
    , genericTake
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

genericReplicate ∷ Integral n ⇒ n → α → Stream α
{-# INLINE genericReplicate #-}
genericReplicate = M.genericReplicate

genericUnfoldrN ∷ Integral n ⇒ n → (β → Maybe (α, β)) → β → Stream α
{-# INLINE genericUnfoldrN #-}
genericUnfoldrN = M.genericUnfoldrN
