{-# LANGUAGE
    FlexibleContexts
  , UnicodeSyntax
  #-}
module Data.Bitstream.Internal
    ( packPackets

    , lePacketsFromNBits
    , bePacketsFromNBits

    , lePacketsToBits
    , bePacketsToBits
    )
    where
import Data.Bits
import Data.Bitstream.Generic
import Data.Bitstream.Packet
import Data.Vector.Fusion.Stream.Monadic (Stream(..), Step(..))
import Data.Vector.Fusion.Stream.Size
import Prelude hiding (length, null)
import Prelude.Unicode

packPackets ∷ (Bitstream (Packet d), Monad m) ⇒ Stream m Bool → Stream m (Packet d)
{-# INLINEABLE packPackets #-}
packPackets (Stream step s0 sz) = Stream step' ((∅), Just s0) sz'
    where
      sz' ∷ Size
      {-# INLINE sz' #-}
      sz' = case sz of
              Exact n → Exact ((n+7) `div` 8)
              Max   n → Max   ((n+7) `div` 8)
              Unknown → Unknown
      {-# INLINE step' #-}
      step' (p, Just s)
          = do r ← step s
               case r of
                 Yield b s'
                     | full p    → return $ Yield p (singleton b, Just s')
                     | otherwise → return $ Skip    (p `snoc` b , Just s')
                 Skip    s'      → return $ Skip    (p          , Just s')
                 Done
                     | null p    → return Done
                     | otherwise → return $ Yield p ((⊥)       , Nothing)
      step' (_, Nothing)
          = return Done

lePacketsFromNBits ∷ ( Integral n
                     , Integral β
                     , Bits β
                     , Monad m
                     )
                   ⇒ n
                   → β
                   → Stream m (Packet Left)
{-# INLINEABLE lePacketsFromNBits #-}
lePacketsFromNBits n0 β0 = Stream step (n0, β0) (Exact nOctets)
    where
      nOctets ∷ Int
      {-# INLINE nOctets #-}
      nOctets = (fromIntegral n0 + 7) `div` 8
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

bePacketsFromNBits ∷ ( Integral n
                     , Integral β
                     , Bits β
                     , Monad m
                     )
                   ⇒ n
                   → β
                   → Stream m (Packet Right)
{-# INLINEABLE bePacketsFromNBits #-}
bePacketsFromNBits n0 β = Stream step (n0, nOctets ⋅ 8) (Exact nOctets)
    where
      nOctets ∷ Int
      {-# INLINE nOctets #-}
      nOctets = (fromIntegral n0 + 7) `div` 8
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

lePacketsToBits ∷ (Monad m, Bits β) ⇒ Stream m (Packet Left) → m β
{-# INLINEABLE lePacketsToBits #-}
lePacketsToBits (Stream step s0 _) = go (s0, 0, 0)
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
                 Done       → return $ n

bePacketsToBits ∷ (Monad m, Bits β) ⇒ Stream m (Packet Right) → m β
{-# INLINEABLE bePacketsToBits #-}
bePacketsToBits (Stream step s0 _) = go (s0, 0)
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
                 Done       → return $ n
