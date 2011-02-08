{-# LANGUAGE
    RankNTypes
  , UnicodeSyntax
  #-}
module Data.Bitstream.Fusion
    ( genericLength
    , genericReplicate
    , genericUnfoldrN
    )
    where
import Data.Vector.Fusion.Stream
import Prelude hiding (replicate)
import Prelude.Unicode

genericLength ∷ Num n ⇒ Stream α → n
{-# INLINE genericLength #-}
genericLength s = foldl' (\n _ → n+1) 0 s

genericReplicate ∷ Integral n ⇒ n → α → Stream α
{-# INLINE genericReplicate #-}
genericReplicate n0 α = unfoldr go n0
    where
      {-# INLINE go #-}
      go n | n ≤ 0     = Nothing
           | otherwise = Just (α, n-1)

{-# RULES
"genericReplicate @ Int"
    ∀n α. genericReplicate n α = replicate n α
  #-}

genericUnfoldrN ∷ Integral n ⇒ n → (β → Maybe (α, β)) → β → Stream α
{-# INLINE genericUnfoldrN #-}
genericUnfoldrN n0 f β0 = unfoldr go (n0, β0)
    where
      {-# INLINE go #-}
      go (n, β)
          | n ≤ 0     = Nothing
          | otherwise = do (α, β') ← f β
                           return (α, (n-1, β'))

{-# RULES
"genericUnfoldrN @ Int"
    ∀n f β. genericUnfoldrN n f β = unfoldrN n f β
  #-}
