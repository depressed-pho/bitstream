{-# LANGUAGE
    UnicodeSyntax
  #-}
module Data.Bitstream.Fusion.Monadic
    ( genericLength
    , genericTake
    , genericDrop
    , genericIndex
    , genericReplicate
    , genericReplicateM
    , genericUnfoldrN
    , genericUnfoldrNM
    , genericFindIndex
    , genericFindIndexM
    , genericIndexed
    )
    where
import Data.Vector.Fusion.Stream.Monadic
import Data.Vector.Fusion.Stream.Size
import Prelude hiding ((!!), drop, replicate, take)
import Prelude.Unicode

genericLength ∷ (Monad m, Num n) ⇒ Stream m α → m n
{-# INLINE genericLength #-}
genericLength s = foldl' (\n _ → n+1) 0 s

genericTake ∷ (Monad m, Integral n) ⇒ n → Stream m α → Stream m α
{-# INLINE [0] genericTake #-}
{-# RULES "genericTake → take" genericTake = take #-}
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

genericDrop ∷ (Monad m, Integral n) ⇒ n → Stream m α → Stream m α
{-# INLINE [0] genericDrop #-}
{-# RULES "genericDrop → drop" genericDrop = drop #-}
genericDrop n0 (Stream step s0 sz) = Stream step' (s0, Just n0) (toMax sz)
    where
      {-# INLINE step' #-}
      step' (s, Just n)
          | n > 0
              = do r ← step s
                   case r of
                     Yield _ s' → return $ Skip (s', Just (n-1))
                     Skip    s' → return $ Skip (s', Just n)
                     Done       → return $ Done
          | otherwise
              = return $ Skip (s, Nothing)

      step' (s, Nothing)
          = do r ← step s
               case r of
                 Yield α s' → return $ Yield α (s', Nothing)
                 Skip    s' → return $ Skip    (s', Nothing)
                 Done       → return Done

genericIndex ∷ (Monad m, Integral n) ⇒ Stream m α → n → m α
{-# INLINE [0] genericIndex #-}
{-# RULES "genericIndex → (!!)" genericIndex = (!!) #-}
genericIndex (Stream step s0 _) i0
    | i0 < 0    = fail ("genericIndex: out of range: " ⧺ show i0)
    | otherwise = index_loop s0 0
    where
      {-# INLINE index_loop #-}
      index_loop s i
          = do r ← step s
               case r of
                 Yield α s'
                     | i ≡ i0    → return α
                     | otherwise → index_loop s' (i+1)
                 Skip    s'      → index_loop s' i
                 Done            → fail ("genericIndex: out of range: " ⧺ show i)

genericReplicate ∷ (Monad m, Integral n) ⇒ n → α → Stream m α
{-# INLINE genericReplicate #-}
genericReplicate n = genericReplicateM n ∘ return

genericReplicateM ∷ (Monad m, Integral n) ⇒ n → m α → Stream m α
{-# INLINE [0] genericReplicateM #-}
{-# RULES "genericReplicateM → replicateM" genericReplicateM = replicateM #-}
genericReplicateM n0 mα = unfoldrM go n0
    where
      {-# INLINE go #-}
      go n | n ≤ 0     = return Nothing
           | otherwise = do α ← mα
                            return $ Just (α, n-1)

genericUnfoldrN ∷ (Monad m, Integral n) ⇒ n → (β → Maybe (α, β)) → β → Stream m α
{-# INLINE genericUnfoldrN #-}
genericUnfoldrN n f = genericUnfoldrNM n (return ∘ f)

genericUnfoldrNM ∷ (Monad m, Integral n) ⇒ n → (β → m (Maybe (α, β))) → β → Stream m α
{-# INLINE [0] genericUnfoldrNM #-}
{-# RULES "genericUnfoldrNM → unfoldrNM" genericUnfoldrNM = unfoldrNM #-}
genericUnfoldrNM n0 f β0 = unfoldrM go (n0, β0)
    where
      {-# INLINE go #-}
      go (n, β)
          | n ≤ 0     = return Nothing
          | otherwise = do r ← f β
                           return $ do (α, β') ← r
                                       return (α, (n-1, β'))

genericFindIndex ∷ (Monad m, Integral n) ⇒ (α → Bool) → Stream m α → m (Maybe n)
{-# INLINE genericFindIndex #-}
genericFindIndex f = genericFindIndexM (return ∘ f)

genericFindIndexM ∷ (Monad m, Integral n) ⇒ (α → m Bool) → Stream m α → m (Maybe n)
{-# INLINE [0] genericFindIndexM #-}
{-# RULES "genericFindIndexM → findIndexM" genericFindIndexM = findIndexM #-}
genericFindIndexM f (Stream step s0 _) = findIndex_loop s0 0
    where
      {-# INLINE findIndex_loop #-}
      findIndex_loop s i
          = do r ← step s
               case r of
                 Yield α s' → do b ← f α
                                 if b then return $ Just i
                                      else findIndex_loop s' (i+1)
                 Skip    s' → findIndex_loop s' i
                 Done       → return Nothing

genericIndexed ∷ (Monad m, Integral n) ⇒ Stream m α → Stream m (n, α)
{-# INLINE [0] genericIndexed #-}
{-# RULES "genericIndexed → indexed" genericIndexed = indexed #-}
genericIndexed (Stream step s0 sz) = Stream step' (s0, 0) sz
    where
      {-# INLINE step' #-}
      step' (s, i)
          = do r ← step s
               case r of
                 Yield α s' → return $ Yield (i, α) (s', i+1)
                 Skip    s' → return $ Skip         (s', i  )
                 Done       → return $ Done
