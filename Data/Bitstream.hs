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
    ( Bitstream
    , Left
    , Right
    )
    where
import qualified Data.Bitstream.Generic as G
import Data.Bitstream.Packet (Left, Right, Packet)
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SV
import qualified Data.Stream as S
import Foreign.Storable
import Prelude.Unicode

newtype Bitstream d
    = Bitstream (SV.Vector (Packet d))
    deriving (Eq, Show)

{-# INLINE streamSV #-}
streamSV ∷ ∀α. Storable α ⇒ SV.Vector α → S.Stream α
streamSV xs = S.unfoldr produce 0
    where
      {-# INLINE produce #-}
      produce ∷ Int → Maybe (α, Int)
      produce i
          | i < SV.length xs = Just (SV.unsafeIndex xs i, i+1)
          | otherwise        = Nothing

{-# INLINE unstreamSV #-}
unstreamSV ∷ ∀α. Storable α ⇒ S.Stream α → SV.Vector α
unstreamSV (S.Stream f s0) = SV.unfoldr consume s0
    where
      {-# INLINE consume #-}
      consume s = case f s of
                    S.Yield α s' → Just (α, s')
                    S.Skip    s' → consume s'
                    S.Done       → Nothing

{-
{-# INLINE unsafeUnstreamSV #-}
unsafeUnstreamSV ∷ ∀α. Storable α ⇒ Int → S.Stream α → SV.Vector α
unsafeUnstreamSV n (S.Stream f s0) = fst $! SV.unfoldrN n consume s0
    where
      {-# INLINE consume #-}
      consume s = case f s of
                    S.Yield α s' → Just (α, s')
                    S.Skip    s' → consume s'
                    S.Done       → Nothing
-}

{-# INLINE packStream #-}
packStream ∷ ∀d. G.Bitstream (Packet d) ⇒ S.Stream Bool → S.Stream (Packet d)
packStream (S.Stream f s0) = S.unfoldr pack8 (Just s0)
    where
      {-# INLINE pack8 #-}
      pack8 Nothing  = Nothing
      pack8 (Just s) = case G.unfoldrN (8 ∷ Int) consume s of
                         (p, s')
                             | G.null p  → Nothing
                             | otherwise → Just (p, s')
      {-# INLINE consume #-}
      consume s = case f s of
                    S.Yield b s' → Just (b, s')
                    S.Skip    s' → consume s'
                    S.Done       → Nothing

instance G.Bitstream (Packet d) ⇒ G.Bitstream (Bitstream d) where
    {-# INLINE [0] stream #-}
    stream (Bitstream v)
        = {-# CORE "strict bitstream 'stream'" #-}
          S.concatMap G.stream (streamSV v)

    {-# INLINE [0] unstream #-}
    unstream
        = {-# CORE "strict bitstream 'unstream'" #-}
          Bitstream ∘ unstreamSV ∘ packStream
