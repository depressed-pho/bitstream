{-# LANGUAGE
    FlexibleContexts
  , ScopedTypeVariables
  , UnicodeSyntax
  #-}
module Data.Bitstream.Internal
    ( packStream

    , streamSV
    , unstreamSV

    , streamLV
    , unstreamLV

    , fromBS
    , toBS
    )
    where
import qualified Data.Bitstream.Generic as G
import Data.Bitstream.Packet
import qualified Data.ByteString as BS
import qualified Data.Stream as S
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SV
import qualified Data.StorableVector.Lazy as LV
import Foreign.Storable
import Prelude.Unicode

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

{-# INLINE streamLV #-}
streamLV ∷ ∀α. Storable α ⇒ LV.Vector α → S.Stream α
streamLV = S.concatMap streamSV ∘ S.stream ∘ LV.chunks

{-# INLINE unstreamLV #-}
unstreamLV ∷ ∀α. Storable α ⇒ LV.ChunkSize → S.Stream α → LV.Vector α
unstreamLV n (S.Stream f s0) = LV.unfoldr n consume s0
    where
      {-# INLINE consume #-}
      consume s = case f s of
                    S.Yield α s' → Just (α, s')
                    S.Skip    s' → consume s'
                    S.Done       → Nothing

{-# INLINEABLE fromBS #-}
fromBS ∷ BS.ByteString → SV.Vector (Packet d)
fromBS bs0 = fst $ SV.unfoldrN len go bs0
    where
      {-# INLINE len #-}
      len ∷ Int
      len = BS.length bs0
      {-# INLINE go #-}
      go bs = do (o, bs') ← BS.uncons bs
                 return (fromOctet o, bs')

{-# INLINEABLE toBS #-}
toBS ∷ G.Bitstream (Packet d) ⇒ SV.Vector (Packet d) → BS.ByteString
toBS v0 = fst $ BS.unfoldrN len go ((G.∅), (G.∅), v0)
    where
      {-# INLINE len #-}
      len ∷ Int
      len = SV.length v0
      {-# INLINE go #-}
      go (p, r, v)
          | full p
              = Just (toOctet p, ((G.∅), r, v))
          | G.null r
              = case SV.viewL v of
                  Just (r', v')
                      → go (p, r', v')
                  Nothing
                      | G.null p  → Nothing
                      | otherwise → Just (toOctet p, ((G.∅), (G.∅), SV.empty))
          | otherwise
              = let lenR     ∷ Int
                    lenR     = 8 - G.length p
                    (rH, rT) = G.splitAt lenR r
                in
                  go (p G.⧺ rH, rT, v)
