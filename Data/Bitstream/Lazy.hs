{-# LANGUAGE
    FlexibleContexts
  , ScopedTypeVariables
  , UndecidableInstances
  , UnicodeSyntax
  #-}
module Data.Bitstream.Lazy
    ( {-Bitstream
    , Left
    , Right-}
    )
    where
{-
import qualified Data.Bitstream.Generic as G
import Data.Bitstream.Internal
import Data.Bitstream.Packet (Left, Right, Packet)
import qualified Data.StorableVector.Lazy as LV
import Prelude.Unicode

-- 32 KiB * sizeOf (Packet d) == 64 KiB
chunkSize ∷ Int
chunkSize = 32 ⋅ 1024

newtype Bitstream d
    = Bitstream (LV.Vector (Packet d))
    deriving (Eq, Show)

instance G.Bitstream (Packet d) ⇒ G.Bitstream (Bitstream d) where
    {-# INLINE [0] stream #-}
    stream (Bitstream v)
        = {-# CORE "lazy bitstream 'stream'" #-}
          S.concatMap G.stream (streamLV v)

    {-# INLINE [0] unstream #-}
    unstream
        = {-# CORE "lazy bitstream 'unstream'" #-}
          Bitstream ∘ unstreamLV chunkSize ∘ packStream
-}
