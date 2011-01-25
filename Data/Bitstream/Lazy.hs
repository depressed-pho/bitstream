{-# LANGUAGE
    FlexibleContexts
  , ScopedTypeVariables
  , UndecidableInstances
  , UnicodeSyntax
  #-}
module Data.Bitstream.Lazy
    ( -- * Types
      Bitstream
    , Left
    , Right

      -- * Introducing and eliminating 'Bitstream's
    , empty
    , (∅)
    , singleton
    , pack
    , unpack

      -- ** Converting from\/to lazy 'BS.ByteString's
    , fromByteString
    , toByteString
    )
    where
import Data.Bitstream.Generic hiding (Bitstream)
import qualified Data.Bitstream.Generic as G
import Data.Bitstream.Internal
import Data.Bitstream.Packet (Left, Right, Packet)
import qualified Data.ByteString.Lazy as LS
import qualified Data.List.Stream as L
import qualified Data.StorableVector.Lazy as LV
import qualified Data.Stream as S
import Prelude.Unicode

-- 32 KiB * sizeOf (Packet d) == 64 KiB
chunkSize ∷ LV.ChunkSize
chunkSize = LV.ChunkSize (32 ⋅ 1024)

newtype Bitstream d
    = Bitstream (LV.Vector (Packet d))
    deriving (Eq, Show)

instance G.Bitstream (Packet d) ⇒ G.Bitstream (Bitstream d) where
    {-# INLINE [0] pack #-}
    pack xs0 = Bitstream (LV.unfoldr chunkSize f xs0)
        where
          {-# INLINE f #-}
          f xs = case L.splitAt 8 xs of
                   (hd, tl)
                       | L.null hd → Nothing
                       | otherwise → Just (pack hd, tl)

    {-# INLINE [0] unpack #-}
    unpack (Bitstream v) = L.concatMap unpack (LV.unpack v)

    {-# INLINE [0] stream #-}
    stream (Bitstream v)
        = {-# CORE "lazy bitstream 'stream'" #-}
          S.concatMap G.stream (streamLV v)

    {-# INLINE [0] unstream #-}
    unstream
        = {-# CORE "lazy bitstream 'unstream'" #-}
          Bitstream ∘ unstreamLV chunkSize ∘ packStream

    {-# INLINE empty #-}
    empty = Bitstream LV.empty

    {-# INLINE singleton #-}
    singleton b
        = Bitstream (LV.singleton (singleton b))

{-# INLINE fromByteString #-}
fromByteString ∷ LS.ByteString → Bitstream d
fromByteString = Bitstream ∘ fromLBS

{-# INLINE toByteString #-}
toByteString ∷ G.Bitstream (Packet d) ⇒ Bitstream d → LS.ByteString
toByteString (Bitstream v) = toLBS v
