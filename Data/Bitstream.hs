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

{-# INLINE unstreamSV' #-}
unstreamSV' ∷ ∀α. Storable α ⇒ Int → S.Stream α → SV.Vector α
unstreamSV' n (S.Stream f s0) = fst $ SV.unfoldrN n consume s0
    where
      {-# INLINE consume #-}
      consume s = case f s of
                    S.Yield α s' → Just (α, s')
                    S.Skip    s' → consume s'
                    S.Done       → Nothing

instance G.Bitstream (Packet d) ⇒ G.Bitstream (Bitstream d) where
    {-# INLINE [0] stream #-}
    stream (Bitstream v)
        = S.concatMap G.stream (streamSV v)
