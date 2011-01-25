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
    , fromChunks
    , toChunks

      -- ** Converting from\/to lazy 'BS.ByteString's
    , fromByteString
    , toByteString

      -- ** Converting from\/to 'S.Stream's
    , stream
    , unstream

      -- * Changing bit order in octets
    , directionLToR
    , directionRToL

      -- * Basic interface
    , cons
    , cons'
    , snoc
    , append
    , (⧺)
    )
    where
import qualified Data.Bitstream as Strict
import Data.Bitstream.Generic hiding (Bitstream)
import qualified Data.Bitstream.Generic as G
import Data.Bitstream.Internal
import Data.Bitstream.Packet
import qualified Data.ByteString.Lazy as LS
import qualified Data.List.Stream as L
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Lazy as LV
import qualified Data.Stream as S
import Prelude ( Bool(..), Eq(..), Int, Integral, Maybe(..), Monad(..), Num(..)
               , Ord(..), Ordering(..), Show(..), ($), div, error, fmap
               , fromIntegral, fst, otherwise
               )
import Prelude.Unicode hiding ((⧺), (∈), (∉))

-- 32 KiB * sizeOf (Packet d) == 64 KiB
chunkSize ∷ Num α ⇒ α
chunkSize = 32 ⋅ 1024
{-# INLINE chunkSize #-}

newtype Bitstream d
    = Bitstream (LV.Vector (Packet d))
    deriving (Show)

instance Ord (Bitstream d) ⇒ Eq (Bitstream d) where
    {-# INLINE (==) #-}
    x == y = case x `compare` y of
               EQ → True
               _  → False

-- | 'Bitstream's are compared as two binary integers:
--
-- @
-- let x = 'pack' ['True' , 'False', 'False']
--     y = 'pack' ['False', 'True' , 'False']
-- in
--   'compare' x y -- 'GT'
-- @
instance G.Bitstream (Packet d) ⇒ Ord (Bitstream d) where
    {-# INLINEABLE compare #-}
    (Bitstream x0) `compare` (Bitstream y0) = go ((∅), x0) ((∅), y0)
        where
          {-# INLINE go #-}
          go (px, x) (py, y)
              | null px
                  = case LV.viewL x of
                      Just (px', x')
                          → go (px', x') (py, y)
                      Nothing
                          → if null py then
                                 case LV.viewL y of
                                   Just _  → LT
                                   Nothing → EQ
                            else
                                LT
              | null py
                  = case LV.viewL y of
                      Just (py', y')
                          → go (px, x) (py', y')
                      Nothing
                          → GT
              | otherwise
                  = let len ∷ Int
                        len = min (length px) (length py)
                        pxH, pxT, pyH, pyT ∷ Packet d
                        (pxH, pxT) = splitAt len px
                        (pyH, pyT) = splitAt len py
                    in
                      case pxH `compare` pyH of
                        LT → LT
                        GT → GT
                        EQ → go (pxT, x) (pyT, y)

instance G.Bitstream (Packet d) ⇒ G.Bitstream (Bitstream d) where
    {-# SPECIALISE instance G.Bitstream (Bitstream Left ) #-}
    {-# SPECIALISE instance G.Bitstream (Bitstream Right) #-}

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
        = S.concatMap G.stream (streamLV v)

    {-# INLINE [0] unstream #-}
    unstream
        = Bitstream ∘ unstreamLV chunkSize ∘ packStream

    {-# INLINE empty #-}
    empty = Bitstream LV.empty

    {-# INLINE singleton #-}
    singleton b
        = Bitstream (LV.singleton (singleton b))

    {-# INLINE cons #-}
    cons b (Bitstream v)
        = Bitstream (LV.cons (singleton b) v)

    {-# INLINE snoc #-}
    snoc = snoc'

    {-# INLINE append #-}
    append (Bitstream x) (Bitstream y)
        = Bitstream (LV.append x y)

{-# INLINE fromChunks #-}
fromChunks ∷ [Strict.Bitstream d] → Bitstream d
fromChunks = Bitstream ∘ LV.fromChunks ∘ L.map Strict.toPackets

{-# INLINE toChunks #-}
toChunks ∷ Bitstream d → [Strict.Bitstream d]
toChunks (Bitstream v) = L.map Strict.fromPackets (LV.chunks v)

{-# INLINE fromByteString #-}
fromByteString ∷ LS.ByteString → Bitstream d
fromByteString = Bitstream ∘ fromLBS

{-# INLINE toByteString #-}
toByteString ∷ G.Bitstream (Packet d) ⇒ Bitstream d → LS.ByteString
toByteString (Bitstream v) = toLBS v

{-# INLINE directionLToR #-}
directionLToR ∷ Bitstream Left → Bitstream Right
directionLToR (Bitstream v) = Bitstream (LV.map packetLToR v)

{-# INLINE directionRToL #-}
directionRToL ∷ Bitstream Right → Bitstream Left
directionRToL (Bitstream v) = Bitstream (LV.map packetRToL v)

{-# INLINEABLE cons' #-}
{-# SPECIALISE cons' ∷ Bool → Bitstream Left  → Bitstream Left  #-}
{-# SPECIALISE cons' ∷ Bool → Bitstream Right → Bitstream Right #-}
cons' ∷ G.Bitstream (Packet d) ⇒ Bool → Bitstream d → Bitstream d
cons' b (Bitstream v) = Bitstream (LV.fromChunks (go (LV.chunks v)))
    where
      {-# INLINE go #-}
      go []     = [SV.singleton (singleton b)]
      go (x:xs) = case SV.viewL x of
                    Just (p, ps)
                        | length p < (8 ∷ Int)
                              → SV.cons (cons b p) ps : xs
                        | SV.length x < chunkSize
                              → SV.cons (singleton b) ps : xs
                        | otherwise
                              → SV.singleton (singleton b) : x : xs
                    Nothing   → SV.singleton (singleton b) : xs

{-# INLINEABLE snoc' #-}
{-# SPECIALISE snoc' ∷ Bitstream Left  → Bool → Bitstream Left  #-}
{-# SPECIALISE snoc' ∷ Bitstream Right → Bool → Bitstream Right #-}
snoc' ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bool → Bitstream d
snoc' (Bitstream v) b = Bitstream (LV.fromChunks (go (LV.chunks v)))
    where
      {-# INLINE go #-}
      go []     = [SV.singleton (singleton b)]
      go (x:[]) = case SV.viewR x of
                    Just (ps, p)
                        | length p < (8 ∷ Int)
                              → [SV.snoc ps (snoc p b)]
                        | SV.length x < chunkSize
                              → [SV.snoc x (singleton b)]
                        | otherwise
                              → [x, SV.singleton (singleton b)]
                    Nothing   → [SV.singleton (singleton b)]
      go (x:xs) = x : go xs
