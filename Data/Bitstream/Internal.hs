{-# LANGUAGE
    FlexibleContexts
  , UnicodeSyntax
  #-}
module Data.Bitstream.Internal
    ( packPackets
    )
    where
import Data.Bitstream.Generic
import Data.Bitstream.Packet
import Data.Vector.Fusion.Stream.Monadic (Stream(..), Step(..))
import Data.Vector.Fusion.Stream.Size
import Prelude hiding (null)
import Prelude.Unicode

packPackets ∷ (Bitstream (Packet d), Monad m) ⇒ Stream m Bool → Stream m (Packet d)
{-# INLINE packPackets #-}
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
