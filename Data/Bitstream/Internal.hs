{-# LANGUAGE
    BangPatterns
  , CPP
  , FlexibleContexts
  , UnicodeSyntax
  #-}
module Data.Bitstream.Internal
    ( packPackets
    , packPacketsSize

    , lePacketsFromNBits
    , bePacketsFromNBits
    , packetsFromNBitsSize

    , lePacketsToBits
    , bePacketsToBits
    )
    where
import Data.Bits
import Data.Bitstream.Generic
import Data.Bitstream.Packet
import Data.Vector.Fusion.Stream.Monadic (Stream(..), Step(..))
#if MIN_VERSION_vector(0,11,0)
import qualified Data.Vector.Fusion.Bundle.Monadic as B
import Data.Vector.Fusion.Bundle.Monadic (Bundle)
import Data.Vector.Fusion.Bundle.Size
#else
import Data.Vector.Fusion.Stream.Size
#endif
import Prelude hiding (length, null)
import Prelude.Unicode

packPacketsSize ∷ Size -> Size
{-# INLINE packPacketsSize #-}
packPacketsSize sz = case sz of
    Exact n → Exact ((n+7) `div` 8)
    Max   n → Max   ((n+7) `div` 8)
    Unknown → Unknown

#if MIN_VERSION_vector(0,11,0)
inplace :: Monad m => (Stream m a -> Stream m b)
        -> (Size -> Size) -> Bundle m v a -> Bundle m v b
inplace f g b = b `seq` B.fromStream (f (B.elements b)) (g (B.size b))
#endif

{-# INLINEABLE packPackets #-}
#if MIN_VERSION_vector(0,11,0)
packPackets ∷ (Bitstream (Packet d), Monad m) ⇒ Bundle m v Bool → Bundle m v (Packet d)
packPackets = inplace (\(Stream s s0) -> Stream (step' s) ((∅), Just s0)) packPacketsSize
#else
packPackets ∷ (Bitstream (Packet d), Monad m) ⇒ Stream m Bool → Stream m (Packet d)
packPackets (Stream s s0 sz) = Stream (step' s) ((∅), Just s0) (packPacketsSize sz)
#endif
    where
      {-# INLINE step' #-}
      step' step (p, Just s)
          = do r ← step s
               case r of
                 Yield b s'
                     | full p    → return $ Yield p (singleton b, Just s')
                     | otherwise → return $ Skip    (p `snoc` b , Just s')
                 Skip    s'      → return $ Skip    (p          , Just s')
                 Done
                     | null p    → return Done
                     | otherwise → return $ Yield p ((⊥)       , Nothing)
      step' _ (_, Nothing)
          = return Done

nOctets ∷ Integral n ⇒ n → Int
{-# INLINE nOctets #-}
nOctets nBits
    = (fromIntegral nBits + 7) `div` 8

packetsFromNBitsSize ∷ Integral n ⇒ n → Size
packetsFromNBitsSize = Exact . nOctets

{-# INLINEABLE lePacketsFromNBits #-}
#if MIN_VERSION_vector(0,11,0)
lePacketsFromNBits ∷ ( Integral n
                     , Integral β
                     , Bits β
                     , Monad m
                     )
                   ⇒ n
                   → β
                   → Bundle m v (Packet Left)
lePacketsFromNBits n0 β0 = B.fromStream (Stream step (n0, β0)) (packetsFromNBitsSize n0)
#else
lePacketsFromNBits ∷ ( Integral n
                     , Integral β
                     , Bits β
                     , Monad m
                     )
                   ⇒ n
                   → β
                   → Stream m (Packet Left)
lePacketsFromNBits n0 β0 = Stream step (n0, β0) (packetsFromNBitsSize n0)
#endif
    where
      {-# INLINE step #-}
      step (n, β)
          | n > 0
              = let !n'  = min 8 n
                    !n'' = n - n'
                    !p   = fromNBits n' β
                    !β'  = β `shiftR` 8
                in
                  return $ Yield p (n'', β')
          | otherwise
              = return Done

{-# INLINEABLE bePacketsFromNBits #-}
#if MIN_VERSION_vector(0,11,0)
bePacketsFromNBits ∷ ( Integral n
                   , Integral β
                   , Bits β
                   , Monad m
                   )
                 ⇒ n
                 → β
                 → Bundle m v (Packet Right)
bePacketsFromNBits n0 β = B.fromStream (Stream step (n0, nOctets n0 ⋅ 8)) (packetsFromNBitsSize n0)
#else
bePacketsFromNBits ∷ ( Integral n
                     , Integral β
                     , Bits β
                     , Monad m
                     )
                   ⇒ n
                   → β
                   → Stream m (Packet Right)
bePacketsFromNBits n0 β = Stream step (n0, nOctets n0 ⋅ 8) (bePacketsFromNBitsSize n0)
#endif
    where
      {-# INLINE step #-}
      step (n, r)
          | n > 0
              = let !r'  = r - 8
                    !n'  = n - fromIntegral r'
                    !n'' = n - n'
                    !p   = fromNBits n' (β `shiftR` r')
                in
                  return $ Yield p (n'', r')
          | otherwise
              = return Done

{-# INLINEABLE lePacketsToBits #-}
#if MIN_VERSION_vector(0,11,0)
lePacketsToBits ∷ (Monad m, Integral β, Bits β) ⇒ Bundle m v (Packet Left) → m β
lePacketsToBits (B.Bundle (Stream step s0) _ _ _) = go (s0, 0, 0)
#else
lePacketsToBits ∷ (Monad m, Integral β, Bits β) ⇒ Stream m (Packet Left) → m β
lePacketsToBits (Stream step s0 _) = go (s0, 0, 0)
#endif
    where
      {-# INLINE go #-}
      go (s, o, n)
          = do r ← step s
               case r of
                 Yield p s' → let !n' = (toBits p `shiftL` o) .|. n
                                  !o' = o + length p
                              in
                                go (s', o', n')
                 Skip    s' → go (s', o, n)
                 Done       → return n

{-# INLINEABLE bePacketsToBits #-}
#if MIN_VERSION_vector(0,11,0)
bePacketsToBits ∷ (Monad m, Integral β, Bits β) ⇒ Bundle m v (Packet Right) → m β
bePacketsToBits (B.Bundle (Stream step s0) _ _ _) = go (s0, 0)
#else
bePacketsToBits ∷ (Monad m, Integral β, Bits β) ⇒ Stream m (Packet Right) → m β
bePacketsToBits (Stream step s0 _) = go (s0, 0)
#endif
    where
      {-# INLINE go #-}
      go (s, n)
          = do r ← step s
               case r of
                 Yield p s' → let !o  = length p
                                  !n' = (n `shiftL` o) .|. toBits p
                              in
                                go (s', n')
                 Skip    s' → go (s', n)
                 Done       → return n
