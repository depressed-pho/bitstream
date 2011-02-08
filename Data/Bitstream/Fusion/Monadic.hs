{-# LANGUAGE
    UnicodeSyntax
  #-}
module Data.Bitstream.Fusion.Monadic
    ( genericLength
    , genericTake
    , genericReplicate
    , genericReplicateM
    , genericUnfoldrN
    , genericUnfoldrNM
    )
    where
import Data.Vector.Fusion.Stream.Monadic
import Data.Vector.Fusion.Stream.Size
import Prelude hiding (replicate, take)
import Prelude.Unicode

genericLength ∷ (Monad m, Num n) ⇒ Stream m α → m n
{-# INLINE genericLength #-}
genericLength s = foldl' (\n _ → n+1) 0 s

genericTake ∷ (Monad m, Integral n) ⇒ n → Stream m α → Stream m α
{-# INLINE [0] genericTake #-}
genericTake n (Stream step s0 sz) = Stream step' (s0, 0) (toMax sz)
    where
      {-# INLINE step' #-}
      step' (s, i)
          | i < n
              = do r ← step s
                   case r of
                     Yield α s' → return $ Yield α (s', i+1)
                     Skip    s' → return $ Skip    (s', i  )
                     Done       → return Done
          | otherwise
              = return Done
{-# RULES
"genericTake @ Int"
    genericTake = take
  #-}

genericReplicate ∷ (Monad m, Integral n) ⇒ n → α → Stream m α
{-# INLINE genericReplicate #-}
genericReplicate n = genericReplicateM n ∘ return

genericReplicateM ∷ (Monad m, Integral n) ⇒ n → m α → Stream m α
{-# INLINE [0] genericReplicateM #-}
genericReplicateM n0 mα = unfoldrM go n0
    where
      {-# INLINE go #-}
      go n | n ≤ 0     = return Nothing
           | otherwise = do α ← mα
                            return $ Just (α, n-1)
{-# RULES
"genericReplicateM @ Int"
    genericReplicateM = replicateM
  #-}

genericUnfoldrN ∷ (Monad m, Integral n) ⇒ n → (β → Maybe (α, β)) → β → Stream m α
{-# INLINE genericUnfoldrN #-}
genericUnfoldrN n f = genericUnfoldrNM n (return ∘ f)

genericUnfoldrNM ∷ (Monad m, Integral n) ⇒ n → (β → m (Maybe (α, β))) → β → Stream m α
{-# INLINE [0] genericUnfoldrNM #-}
genericUnfoldrNM n0 f β0 = unfoldrM go (n0, β0)
    where
      {-# INLINE go #-}
      go (n, β)
          | n ≤ 0     = return Nothing
          | otherwise = do r ← f β
                           return $ do (α, β') ← r
                                       return (α, (n-1, β'))
{-# RULES
"genericUnfoldrNM @ Int"
    genericUnfoldrNM = unfoldrNM
  #-}
