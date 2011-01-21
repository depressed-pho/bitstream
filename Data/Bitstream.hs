{-# LANGUAGE
    FlexibleContexts
  , ScopedTypeVariables
  , UndecidableInstances
  , UnicodeSyntax
  #-}
-- | Fast, packed, strict bit vectors using stream fusion.
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
    )
    where
import Data.Bitstream.Internal
import Data.Bitstream.Generic hiding (Bitstream)
import qualified Data.Bitstream.Generic as G
import Data.Bitstream.Packet (Left, Right, Packet)
import qualified Data.List.Stream as L
import qualified Data.StorableVector as SV
import qualified Data.Stream as S
import Prelude ( Eq(..), Int, Maybe(..), Monad(..), Num(..), Ord(..), Show(..)
               , error, otherwise
               )
import Prelude.Unicode

newtype Bitstream d
    = Bitstream (SV.Vector (Packet d))
    deriving (Eq, Show)

instance G.Bitstream (Packet d) ⇒ G.Bitstream (Bitstream d) where
--    {-# SPECIALISE instance G.Bitstream (Bitstream Left ) #-}
--    {-# SPECIALISE instance G.Bitstream (Bitstream Right) #-}

    {-# INLINE [0] stream #-}
    stream (Bitstream v)
        = {-# CORE "strict bitstream 'stream'" #-}
          S.concatMap stream (streamSV v)

    {-# INLINE [0] unstream #-}
    unstream
        = {-# CORE "strict bitstream 'unstream'" #-}
          Bitstream ∘ unstreamSV ∘ packStream

    {-# INLINE empty #-}
    empty = Bitstream SV.empty

    {-# INLINE singleton #-}
    singleton b
        = Bitstream (SV.singleton (singleton b))

    {-# NOINLINE [1] cons #-}
    cons b (Bitstream v)
        = case SV.viewL v of
            Just (p, v')
                | length p < (8 ∷ Int)
                      → Bitstream (SV.cons (cons    b p) v')
                | otherwise
                      → Bitstream (SV.cons (singleton b) v )
            Nothing   → Bitstream (SV.cons (singleton b) v )

    {-# NOINLINE [1] snoc #-}
    snoc (Bitstream v) b
        = case SV.viewR v of
            Just (v', p)
                | length p < (8 ∷ Int)
                      → Bitstream (SV.snoc v' (snoc    p b))
                | otherwise
                      → Bitstream (SV.snoc v  (singleton b))
            Nothing   → Bitstream (SV.snoc v  (singleton b))

    {-# INLINE [1] append #-}
    append (Bitstream x) (Bitstream y)
        = Bitstream (SV.append x y)

    {-# INLINE [1] head #-}
    head (Bitstream v)
        = head (SV.head v)

    {-# NOINLINE uncons #-}
    uncons (Bitstream v)
        = do (p, v') ← SV.viewL v
             case uncons p of
               Just (b, p')
                   | null p'   → return (b, Bitstream v')
                   | otherwise → return (b, Bitstream (SV.cons p' v'))
               Nothing         → inconsistentState

    {-# INLINE [1] last #-}
    last (Bitstream v)
        = last (SV.last v)

    {-# NOINLINE [1] tail #-}
    tail (Bitstream v)
        = case SV.viewL v of
            Just (p, v')
                → case tail p of
                     p' | null p'   → Bitstream v'
                        | otherwise → Bitstream (SV.cons p' v')
            Nothing
                → emptyStream

    {-# NOINLINE [1] init #-}
    init (Bitstream v)
        = case SV.viewR v of
            Just (v', p)
                → case init p of
                     p' | null p'   → Bitstream v'
                        | otherwise → Bitstream (SV.snoc v' p')
            Nothing
                → emptyStream

    {-# INLINE [1] null #-}
    null (Bitstream v)
        = SV.null v

    {-# SPECIALISE length ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Int #-}
    length (Bitstream v)
        = SV.foldl' (\n p → n + length p) 0 v
    {-# NOINLINE [1] length #-}

    {-# INLINE [1] map #-}
    map f (Bitstream v)
        = Bitstream (SV.map (map f) v)

    {-# INLINE reverse #-}
    reverse (Bitstream v)
        = Bitstream (SV.reverse (SV.map reverse v))

    {-# INLINE [1] concat #-}
    concat = Bitstream ∘ SV.concat ∘ L.map (\(Bitstream v) → v)

inconsistentState ∷ α
inconsistentState
    = error "Data.Bitstream: internal error: inconsistent state"

emptyStream ∷ α
emptyStream
    = error "Data.Bitstream: empty stream"
