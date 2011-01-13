{-# LANGUAGE
    UnicodeSyntax
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
import Data.Bitstream.BitChunk (Left, Right, Chunk)
--import qualified Data.Bitstream.BitChunk as C
import qualified Data.StorableVector as S

newtype Bitstream d
    = BitStream (S.Vector (Chunk d))
