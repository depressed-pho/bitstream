{-# LANGUAGE
    FlexibleContexts
  , ScopedTypeVariables
  , UndecidableInstances
  , UnicodeSyntax
  #-}
-- | Fast, packed, strict bit vectors.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions. e.g.
--
-- > import qualified Data.BitStream as S
--
-- FIXME: explain about directions
module Data.Bitstream
    ( -- * Types
      Bitstream
    , Left
    , Right

      -- * Introducing and eliminating 'Bitstream's
    , empty
    , singleton
    , pack
    , unpack

      -- * Basic interface
    , cons
    , snoc
    , append
    , head
    , uncons
    , last
    , tail
    , init
    , null
    , length

      -- * Transforming 'Bitstream's
    , map
    , reverse
    , intersperse
    , intercalate
    , transpose

      -- * Reducing 'Bitstream's
    , foldl
    , foldl'
    , foldl1
    , foldl1'
    , foldr
    , foldr1

      -- ** Special folds
    , concat
    , concatMap
    , and
    , or
    , any
    , all

      -- * Building lists
      -- ** Scans
    , scanl
    , scanl1
    , scanr
    , scanr1

      -- ** Accumulating maps
    , mapAccumL
    , mapAccumR

      -- ** Replication
    , replicate

      -- ** Unfolding
    , unfoldr
    , unfoldrN

      -- * Substreams
    , take
    , drop
    , splitAt
    )
    where
import Data.Bitstream.Generic hiding (Bitstream)
import qualified Data.Bitstream.Generic as G
import Data.Bitstream.Internal
import Data.Bitstream.Packet (Left, Right, Packet, full)
import qualified Data.List.Stream as L
import qualified Data.StorableVector as SV
import qualified Data.Stream as S
import Prelude ( Bool(..), Eq(..), Int, Maybe(..), Monad(..), Num(..), Ord(..)
               , Show(..), ($), div, error, fromIntegral, fst, otherwise
               )
import Prelude.Unicode

newtype Bitstream d
    = Bitstream (SV.Vector (Packet d))
    deriving (Show)

instance G.Bitstream (Packet d) ⇒ Eq (Bitstream d) where
    x == y = unpack x ≡ unpack y

instance G.Bitstream (Packet d) ⇒ G.Bitstream (Bitstream d) where
    {-# SPECIALISE instance G.Bitstream (Bitstream Left ) #-}
    {-# SPECIALISE instance G.Bitstream (Bitstream Right) #-}

    {-# INLINEABLE [0] pack #-}
    pack xs0 = Bitstream (fst $ SV.unfoldrN l f xs0)
        where
          {-# INLINE l #-}
          l ∷ Int
          l = (L.length xs0 + 7) `div` 8
          {-# INLINE f #-}
          f xs = case L.splitAt 8 xs of
                   (hd, tl)
                       | L.null hd → Nothing
                       | otherwise → Just (pack hd, tl)

    {-# INLINEABLE [0] unpack #-}
    unpack (Bitstream v) = L.concatMap unpack (SV.unpack v)

    {-# INLINE [0] stream #-}
    stream (Bitstream v)
        = S.concatMap stream (streamSV v)

    {-# INLINE [0] unstream #-}
    unstream
        = Bitstream ∘ unstreamSV ∘ packStream

    {-# INLINE empty #-}
    empty = Bitstream SV.empty

    {-# INLINE singleton #-}
    singleton b
        = Bitstream (SV.singleton (singleton b))

    {-# INLINEABLE cons #-}
    cons b (Bitstream v)
        = case SV.viewL v of
            Just (p, v')
                | length p < (8 ∷ Int)
                      → Bitstream (SV.cons (cons    b p) v')
                | otherwise
                      → Bitstream (SV.cons (singleton b) v )
            Nothing   → Bitstream (SV.cons (singleton b) v )

    {-# INLINEABLE snoc #-}
    snoc (Bitstream v) b
        = case SV.viewR v of
            Just (v', p)
                | length p < (8 ∷ Int)
                      → Bitstream (SV.snoc v' (snoc    p b))
                | otherwise
                      → Bitstream (SV.snoc v  (singleton b))
            Nothing   → Bitstream (SV.snoc v  (singleton b))

    {-# INLINE append #-}
    append (Bitstream x) (Bitstream y)
        = Bitstream (SV.append x y)

    {-# INLINE head #-}
    head (Bitstream v)
        = head (SV.head v)

    {-# INLINEABLE uncons #-}
    uncons (Bitstream v)
        = do (p, v') ← SV.viewL v
             case uncons p of
               Just (b, p')
                   | null p'   → return (b, Bitstream v')
                   | otherwise → return (b, Bitstream (SV.cons p' v'))
               Nothing         → inconsistentState

    {-# INLINE last #-}
    last (Bitstream v)
        = last (SV.last v)

    {-# INLINEABLE tail #-}
    tail (Bitstream v)
        = case SV.viewL v of
            Just (p, v')
                → case tail p of
                     p' | null p'   → Bitstream v'
                        | otherwise → Bitstream (SV.cons p' v')
            Nothing
                → emptyStream

    {-# INLINEABLE init #-}
    init (Bitstream v)
        = case SV.viewR v of
            Just (v', p)
                → case init p of
                     p' | null p'   → Bitstream v'
                        | otherwise → Bitstream (SV.snoc v' p')
            Nothing
                → emptyStream

    {-# INLINE null #-}
    null (Bitstream v)
        = SV.null v

    {-# SPECIALISE length ∷ Bitstream Left  → Int #-}
    {-# SPECIALISE length ∷ Bitstream Right → Int #-}
    length (Bitstream v)
        = SV.foldl' (\n p → n + length p) 0 v
    {-# INLINE length #-}

    {-# INLINE map #-}
    map f (Bitstream v)
        = Bitstream (SV.map (map f) v)

    {-# INLINE reverse #-}
    reverse (Bitstream v)
        = Bitstream (SV.reverse (SV.map reverse v))

    {-# INLINE concat #-}
    concat = Bitstream ∘ SV.concat ∘ L.map g
        where
          {-# INLINE g #-}
          g (Bitstream v) = v

    {-# INLINEABLE concatMap #-}
    concatMap f (Bitstream v)
        = Bitstream (SV.concatMap g v)
        where
          {-# INLINE g #-}
          g = SV.concatMap h ∘ SV.pack ∘ unpack
          {-# INLINE h #-}
          h = i ∘ f
          {-# INLINE i #-}
          i (Bitstream v') = v'

    {-# INLINE and #-}
    and (Bitstream v) = SV.all and v

    {-# INLINE or #-}
    or (Bitstream v) = SV.any or v

    {-# INLINE any #-}
    any f (Bitstream v) = SV.any (any f) v

    {-# INLINE all #-}
    all f (Bitstream v) = SV.all (all f) v

    {-# SPECIALISE replicate ∷ Int → Bool → Bitstream Left  #-}
    {-# SPECIALISE replicate ∷ Int → Bool → Bitstream Right #-}
    replicate n0 = Bitstream ∘ SV.unfoldr g ∘ ((,) n0)
        where
          {-# INLINE g #-}
          g (0, _) = Nothing
          g (n, b) = let n' = min 8 n
                         p  = replicate n' b
                     in
                       Just (p, (n-n', b))
    {-# INLINEABLE replicate #-}

    {-# INLINEABLE unfoldr #-}
    unfoldr f = Bitstream ∘ SV.unfoldr g ∘ Just
        where
          {-# INLINE g #-}
          g Nothing  = Nothing
          g (Just β) = case unfoldrN (8 ∷ Int) f β of
                          (p, β')
                              | null p    → Nothing
                              | otherwise → Just (p, β')

    {-# SPECIALISE unfoldrN ∷ Int → (β → Maybe (Bool, β)) → β → (Bitstream Left , Maybe β) #-}
    {-# SPECIALISE unfoldrN ∷ Int → (β → Maybe (Bool, β)) → β → (Bitstream Right, Maybe β) #-}
    unfoldrN n0 f β0 = case SV.unfoldrN l g (n0, Just β0) of
                          (v, Just (_, mβ1)) → (Bitstream v, mβ1    )
                          (v, _            ) → (Bitstream v, Nothing)
        where
          {-# INLINE l #-}
          l ∷ Int
          l = fromIntegral ((n0 + 7) `div` 8)
          {-# INLINE g #-}
          g (n, mβ) = case consume8 n mβ (∅) of
                         (p, mβ')
                             | null p    → Nothing
                             | otherwise → Just (p, (n, mβ'))
          {-# INLINE consume8 #-}
          consume8 0 _        p = (p, Nothing)
          consume8 _ Nothing  p = (p, Nothing)
          consume8 n (Just β) p
              | full p    = (p, Just β)
              | otherwise = case f β of
                              Nothing
                                  → (p, Nothing)
                              Just (b, β')
                                  → consume8 (n-1) (Just β') (p `snoc` b)
    {-# INLINEABLE unfoldrN #-}

    {-# SPECIALISE take ∷ Int → Bitstream Left  → Bitstream Left  #-}
    {-# SPECIALISE take ∷ Int → Bitstream Right → Bitstream Right #-}
    take n0 (Bitstream v0) = Bitstream (SV.unfoldr g (n0, v0))
        where
          {-# INLINE g #-}
          g (0, _) = Nothing
          g (n, v) = do (p, v') ← SV.viewL v
                        let p' = take n p
                            n' = n - length p'
                        return (p', (n', v'))
    {-# INLINEABLE take #-}

    {-# SPECIALISE drop ∷ Int → Bitstream Left  → Bitstream Left  #-}
    {-# SPECIALISE drop ∷ Int → Bitstream Right → Bitstream Right #-}
    drop n0 (Bitstream v0) = Bitstream (g n0 v0)
        where
          {-# INLINE g #-}
          g 0 v = v
          g n v = case SV.viewL v of
                    Just (p, v')
                        | n ≥ length p → g (n - length p) v'
                        | otherwise    → drop n p `SV.cons` v'
                    Nothing            → v
    {-# INLINEABLE drop #-}

--    {-# SPECIALISE splitAt ∷ Int → Bitstream Left  → (Bitstream Left , Bitstream Left ) #-}
--    {-# SPECIALISE splitAt ∷ Int → Bitstream Left  → (Bitstream Right, Bitstream Right) #-}
--    splitAt n0 (Bitstream

inconsistentState ∷ α
inconsistentState
    = error "Data.Bitstream: internal error: inconsistent state"

emptyStream ∷ α
emptyStream
    = error "Data.Bitstream: empty stream"
