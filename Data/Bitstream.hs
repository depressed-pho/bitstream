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
import Data.Bitstream.Internal
import Data.Bitstream.Generic hiding (Bitstream)
import qualified Data.Bitstream.Generic as G
import Data.Bitstream.Packet (Left, Right, Packet)
import qualified Data.StorableVector as SV
import qualified Data.Stream as S
import Prelude hiding (length)
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

    {-# NOINLINE [1] length #-}
    length (Bitstream v)
        = SV.foldl' (\n p → n + length p) 0 v
