{-# LANGUAGE
    RankNTypes
  , UnicodeSyntax
  #-}
module Data.Bitstream.Fusion
    ( genericLength
    , genericReplicate
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
genericReplicate n0 α
    | n0 ≤ 0    = empty
    | otherwise = unfoldr go n0
    where
      {-# INLINE go #-}
      go 0 = Nothing
      go n = Just (α, n-1)

{-# RULES
"genericReplicate @ Int"
    ∀n α. genericReplicate n α = replicate n α
  #-}
