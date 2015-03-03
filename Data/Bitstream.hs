{-# LANGUAGE
    BangPatterns
  , FlexibleContexts
  , FlexibleInstances
  , ScopedTypeVariables
  , UnboxedTuples
  , UndecidableInstances
  , UnicodeSyntax
  #-}
-- | Fast, packed, strict bit streams (i.e. list of 'Bool's) with
-- semi-automatic stream fusion.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions. e.g.
--
-- > import qualified Data.Bitstream as BS
--
-- Strict 'Bitstream's are made of strict 'SV.Vector' of 'Packet's,
-- and each 'Packet's have at least 1 bit.
module Data.Bitstream
    ( -- * Data types
      Bitstream
    , Left
    , Right

      -- * Introducing and eliminating 'Bitstream's
    , empty
    , (∅)
    , singleton
    , pack
    , unpack
    , fromPackets
    , unsafeFromPackets
    , toPackets

      -- ** Converting from\/to strict 'BS.ByteString's
    , fromByteString
    , toByteString

    -- ** Converting from\/to 'Bits''
    , fromBits
    , fromNBits
    , toBits

      -- ** Converting from\/to 'S.Stream's
    , stream
    , unstream
    , streamPackets
    , unstreamPackets

      -- * Changing bit order in octets
    , directionLToR
    , directionRToL

      -- * Basic interface
    , cons
    , snoc
    , append
    , (⧺)
    , head
    , last
    , tail
    , init
    , null
    , length

      -- * Transforming 'Bitstream's
    , map
    , reverse

      -- * Reducing 'Bitstream's
    , foldl
    , foldl'
    , foldl1
    , foldl1'
    , foldr
    , foldr1

      -- ** Special folds
    , concat
    , concatMap
    , and
    , or
    , any
    , all

      -- * Building 'Bitstream's
      -- ** Scans
    , scanl
    , scanl1
    , scanr
    , scanr1

      -- ** Replication
    , replicate

      -- ** Unfolding
    , unfoldr
    , unfoldrN

      -- * Substreams
    , take
    , drop
    , takeWhile
    , dropWhile
    , span
    , break

      -- * Searching streams
      -- ** Searching by equality
    , elem
    , (∈)
    , (∋)
    , notElem
    , (∉)
    , (∌)

      -- ** Searching with a predicate
    , find
    , filter
    , partition

      -- ** Indexing streams
    , (!!)
    , elemIndex
    , elemIndices
    , findIndex
    , findIndices

      -- * Zipping and unzipping streams
    , zip
    , zip3
    , zip4
    , zip5
    , zip6
    , zipWith
    , zipWith3
    , zipWith4
    , zipWith5
    , zipWith6
    , unzip
    , unzip3
    , unzip4
    , unzip5
    , unzip6

    -- * I/O with 'Bitstream's
    -- ** Standard input and output
    , getContents
    , putBits
    , interact

    -- ** Files
    , readFile
    , writeFile
    , appendFile

    -- ** I/O with 'Handle's
    , hGetContents
    , hGet
    , hGetSome
    , hGetNonBlocking
    , hPut
    )
    where
import Data.Bitstream.Generic hiding (Bitstream)
import qualified Data.Bitstream.Generic as G
import Data.Bitstream.Internal
import Data.Bitstream.Packet
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Monoid
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.New as New
import qualified Data.Vector.Generic.Mutable as MVector
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Fusion.Stream as S
import Data.Vector.Fusion.Stream.Monadic (Stream(..), Step(..))
import Data.Vector.Fusion.Stream.Size
import Data.Vector.Fusion.Util
import Prelude ( Bool(..), Eq(..), Int, Integral, Maybe(..), Monad(..), Num(..)
               , Ord(..), Show(..), ($), error, fmap, fromIntegral, fst
               , otherwise
               )
import Prelude.Unicode hiding ((⧺), (∈), (∉))
import System.IO (FilePath, Handle, IO)

-- | A space-efficient representation of a 'Bool' vector, supporting
-- many efficient operations. 'Bitstream's have an idea of
-- /directions/ controlling how octets are interpreted as bits. There
-- are two types of concrete 'Bitstream's: @'Bitstream' 'Left'@ and
-- @'Bitstream' 'Right'@.
data Bitstream d
    = Bitstream {-# UNPACK #-} !Int -- bit length
                {-# UNPACK #-} !(SV.Vector (Packet d))
-- THINKME: The bit length should only be a hint, just like stream
-- size.

instance Show (Packet d) ⇒ Show (Bitstream d) where
    {-# INLINEABLE show #-}
    show (Bitstream _ v0)
        = L.concat
          [ "(S"
          , L.concat (L.unfoldr go v0)
          , ")"
          ]
        where
          {-# INLINE go #-}
          go v | SV.null v = Nothing
               | otherwise = Just (show (SV.head v), SV.tail v)

instance G.Bitstream (Bitstream d) ⇒ Eq (Bitstream d) where
    {-# INLINE (==) #-}
    x == y = stream x ≡ stream y

-- | 'Bitstream's are lexicographically ordered.
--
-- @
-- let x = 'pack' ['True' , 'False', 'False']
--     y = 'pack' ['False', 'True' , 'False']
--     z = 'pack' ['False']
-- in
--   [ 'compare' x y -- 'GT'
--   , 'compare' z y -- 'LT'
--   ]
-- @
instance G.Bitstream (Bitstream d) ⇒ Ord (Bitstream d) where
    {-# INLINE compare #-}
    x `compare` y = stream x `compare` stream y

-- | 'Bitstream' forms 'Monoid' in the same way as ordinary lists:
--
-- @
-- 'mempty'  = 'empty'
-- 'mappend' = 'append'
-- 'mconcat' = 'concat'
-- @
instance G.Bitstream (Bitstream d) ⇒ Monoid (Bitstream d) where
    mempty  = (∅)
    mappend = (⧺)
    mconcat = concat

instance G.Bitstream (Bitstream Left) where
    {-# INLINE basicStream #-}
    basicStream = strictStream

    {-# INLINE basicUnstream #-}
    basicUnstream = strictUnstream

    {-# INLINE basicCons #-}
    basicCons = strictCons

    {-# INLINE basicSnoc #-}
    basicSnoc = strictSnoc

    {-# INLINE basicAppend #-}
    basicAppend = strictAppend

    {-# INLINE basicTail #-}
    basicTail = strictTail

    {-# INLINE basicInit #-}
    basicInit = strictInit

    {-# INLINE basicMap #-}
    basicMap = strictMap

    {-# INLINE basicReverse #-}
    basicReverse = strictReverse

    {-# INLINE basicConcat #-}
    basicConcat = strictConcat

    {-# INLINE basicScanl #-}
    basicScanl = strictScanl

    {-# INLINE basicTake #-}
    basicTake = strictTake

    {-# INLINE basicDrop #-}
    basicDrop = strictDrop

    {-# INLINE basicTakeWhile #-}
    basicTakeWhile = strictTakeWhile

    {-# INLINE basicDropWhile #-}
    basicDropWhile = strictDropWhile

    {-# INLINE basicFilter #-}
    basicFilter = strictFilter

    {-# INLINE basicFromNBits #-}
    basicFromNBits = (unstreamPackets ∘) ∘ lePacketsFromNBits

    {-# INLINE basicToBits #-}
    basicToBits = unId ∘ lePacketsToBits ∘ streamPackets

instance G.Bitstream (Bitstream Right) where
    {-# INLINE basicStream #-}
    basicStream = strictStream

    {-# INLINE basicUnstream #-}
    basicUnstream = strictUnstream

    {-# INLINE basicCons #-}
    basicCons = strictCons

    {-# INLINE basicSnoc #-}
    basicSnoc = strictSnoc

    {-# INLINE basicAppend #-}
    basicAppend = strictAppend

    {-# INLINE basicTail #-}
    basicTail = strictTail

    {-# INLINE basicInit #-}
    basicInit = strictInit

    {-# INLINE basicMap #-}
    basicMap = strictMap

    {-# INLINE basicReverse #-}
    basicReverse = strictReverse

    {-# INLINE basicConcat #-}
    basicConcat = strictConcat

    {-# INLINE basicScanl #-}
    basicScanl = strictScanl

    {-# INLINE basicTake #-}
    basicTake = strictTake

    {-# INLINE basicDrop #-}
    basicDrop = strictDrop

    {-# INLINE basicTakeWhile #-}
    basicTakeWhile = strictTakeWhile

    {-# INLINE basicDropWhile #-}
    basicDropWhile = strictDropWhile

    {-# INLINE basicFilter #-}
    basicFilter = strictFilter

    {-# INLINEABLE basicFromNBits #-}
    basicFromNBits = (unstreamPackets ∘) ∘ bePacketsFromNBits

    {-# INLINEABLE basicToBits #-}
    basicToBits = unId ∘ bePacketsToBits ∘ streamPackets

strictStream ∷ G.Bitstream (Packet d) ⇒ Bitstream d → S.Stream Bool
{-# INLINE strictStream #-}
strictStream (Bitstream l v)
    = {-# CORE "Strict Bitstream stream" #-}
      S.concatMap stream (GV.stream v)
      `S.sized`
      Exact l

strictUnstream ∷ G.Bitstream (Packet d) ⇒ S.Stream Bool → Bitstream d
{-# INLINE strictUnstream #-}
strictUnstream
    = {-# CORE "Strict Bitstream unstream" #-}
      unstreamPackets ∘ packPackets

strictCons ∷ G.Bitstream (Packet d) ⇒ Bool → Bitstream d → Bitstream d
{-# INLINEABLE strictCons #-}
strictCons b (Bitstream 0 _) = Bitstream 1 (SV.singleton (singleton b))
strictCons b (Bitstream l v)
    = case SV.head v of
        p | length p < (8 ∷ Int)
                → Bitstream (l+1) ((b `cons` p) `SV.cons` SV.tail v)
          | otherwise
                → Bitstream (l+1) (singleton b `SV.cons` v)

strictSnoc ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bool → Bitstream d
{-# INLINEABLE strictSnoc #-}
strictSnoc (Bitstream 0 _) b = Bitstream 1 (SV.singleton (singleton b))
strictSnoc (Bitstream l v) b
    = case SV.last v of
        p | length p < (8 ∷ Int)
                → Bitstream (l+1) (SV.init v `SV.snoc` (p `snoc` b))
          | otherwise
                → Bitstream (l+1) (v `SV.snoc` singleton b)

strictAppend ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bitstream d → Bitstream d
{-# INLINE strictAppend #-}
strictAppend (Bitstream lx x) (Bitstream ly y)
    = Bitstream (lx + ly) (x SV.++ y)

strictTail ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bitstream d
{-# INLINEABLE strictTail #-}
strictTail (Bitstream 0 _) = emptyStream
strictTail (Bitstream l v)
    = case tail (SV.head v) of
        p' | null p'   → Bitstream (l-1) (SV.tail v)
           | otherwise → Bitstream (l-1) (p' `SV.cons` SV.tail v)

strictInit ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bitstream d
{-# INLINEABLE strictInit #-}
strictInit (Bitstream 0 _) = emptyStream
strictInit (Bitstream l v)
    = case init (SV.last v) of
        p' | null p'   → Bitstream (l-1) (SV.init v)
           | otherwise → Bitstream (l-1) (SV.init v `SV.snoc` p')

strictMap ∷ G.Bitstream (Packet d) ⇒ (Bool → Bool) → Bitstream d → Bitstream d
{-# INLINE strictMap #-}
strictMap f (Bitstream l v)
    = Bitstream l (SV.map (map f) v)

strictReverse ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bitstream d
{-# INLINE strictReverse #-}
strictReverse (Bitstream l v)
    = Bitstream l (SV.reverse (SV.map reverse v))

strictConcat ∷ G.Bitstream (Bitstream d) ⇒ [Bitstream d] → Bitstream d
{-# INLINEABLE strictConcat #-}
strictConcat xs
    = let (!l, !vs) = L.mapAccumL (\n x → (n + length x, toPackets x)) 0 xs
          !v        = SV.concat vs
      in
        Bitstream l v

strictScanl ∷ G.Bitstream (Bitstream d) ⇒ (Bool → Bool → Bool) → Bool → Bitstream d → Bitstream d
{-# INLINE strictScanl #-}
strictScanl f b
    = unstream ∘ S.scanl f b ∘ stream

strictTake ∷ ( Integral n
             , G.Bitstream (Bitstream d)
             , G.Bitstream (Packet d)
             )
           ⇒ n
           → Bitstream d
           → Bitstream d
{-# INLINEABLE strictTake #-}
strictTake n0 (Bitstream l0 v0)
    | l0 ≡ 0    = (∅)
    | n0 ≤ 0    = (∅)
    | otherwise = let !e = New.create (MVector.new (SV.length v0))
                  in
                    case go n0 v0 0 0 e of
                      (# l, np, mv #)
                          → let !mv' = New.apply (MVector.take np) mv
                                !v   = GV.new mv'
                            in
                              Bitstream l v
    where
      {-# INLINE go #-}
      go 0 _ l np mv  = (# l, np, mv #)
      go n v l np mv
          | SV.null v = (# l, np, mv #)
          | otherwise = let !p   = SV.head v
                            !p'  = take n p
                            !n'  = n - length p'
                            !v'  = SV.tail v
                            !l'  = l + length p'
                            !np' = np + 1
                            !mv' = New.modify (\x → MVector.write x np p') mv
                        in
                          go n' v' l' np' mv'

strictDrop ∷ (Integral n, G.Bitstream (Packet d)) ⇒ n → Bitstream d → Bitstream d
{-# INLINEABLE strictDrop #-}
strictDrop n0 (Bitstream l0 v0)
    | n0 ≤ 0    = Bitstream l0 v0
    | otherwise = case go n0 l0 v0 of
                    (# l, v #) → Bitstream l v
    where
      {-# INLINE go #-}
      go 0 l v = (# l, v #)
      go _ 0 v = (# 0, v #)
      go n l v = let !p = SV.head v
                 in
                   case drop n p of
                     p' | null p'   → go (n - length p) (l - length p) (SV.tail v)
                        | otherwise → (# l - length p + length p'
                                       , p' `SV.cons` SV.tail v #)

strictTakeWhile ∷ G.Bitstream (Packet d) ⇒ (Bool → Bool) → Bitstream d → Bitstream d
{-# INLINEABLE strictTakeWhile #-}
strictTakeWhile f
    = unstreamPackets ∘ takeWhilePS ∘ streamPackets
    where
      {-# INLINE takeWhilePS #-}
      takeWhilePS (Stream step s0 sz) = Stream step' (Just s0) (toMax sz)
          where
            {-# INLINE step' #-}
            step' Nothing  = return Done
            step' (Just s)
                = do r ← step s
                     case r of
                       Yield p s'
                           → case takeWhile f p of
                                p' | p ≡ p'    → return $ Yield p' (Just s')
                                   | otherwise → return $ Yield p' Nothing
                       Skip    s'
                           → return $ Skip (Just s')
                       Done
                           → return Done

strictDropWhile ∷ G.Bitstream (Packet d) ⇒ (Bool → Bool) → Bitstream d → Bitstream d
{-# INLINEABLE strictDropWhile #-}
strictDropWhile _ (Bitstream 0  v0) = Bitstream 0 v0
strictDropWhile f (Bitstream l0 v0) = case go l0 v0 of
                                        (# l, v #) → Bitstream l v
    where
      {-# INLINE go #-}
      go 0 v = (# 0, v #)
      go l v = let !p    = SV.head v
                   !pLen = length p
               in
                 case dropWhile f p of
                   p' | null p'   → go (l - pLen) (SV.tail v)
                      | otherwise → (# l - pLen + length p'
                                     , p' `SV.cons` SV.tail v #)

strictFilter ∷ G.Bitstream (Packet d) ⇒ (Bool → Bool) → Bitstream d → Bitstream d
{-# INLINEABLE strictFilter #-}
strictFilter f
    = unstreamPackets ∘ filterPS ∘ streamPackets
    where
      {-# INLINE filterPS #-}
      filterPS (Stream step s0 sz) = Stream step' s0 (toMax sz)
          where
            {-# INLINE step' #-}
            step' s
                = do r ← step s
                     case r of
                       Yield p s' → case filter f p of
                                       p' | null p'   → return $ Skip s'
                                          | otherwise → return $ Yield p' s'
                       Skip    s' → return $ Skip s'
                       Done       → return Done

strictHead ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bool
{-# RULES "head → strictHead" [1]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d).
    head v = strictHead v #-}
{-# INLINE strictHead #-}
strictHead (Bitstream _ v) = head (SV.head v)

strictLast ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bool
{-# RULES "last → strictLast" [1]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d).
    last v = strictLast v #-}
{-# INLINE strictLast #-}
strictLast (Bitstream _ v) = last (SV.last v)

strictNull ∷ Bitstream d → Bool
{-# RULES "null → strictNull" [1] null = strictNull #-}
{-# INLINE strictNull #-}
strictNull (Bitstream 0 _) = True
strictNull _               = False

strictLength ∷ Num n ⇒ Bitstream d → n
{-# RULES "length → strictLength" [1] length = strictLength #-}
{-# INLINE strictLength #-}
strictLength (Bitstream len _) = fromIntegral len

strictAnd ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bool
{-# RULES "and → strictAnd" [1]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d).
    and v = strictAnd v #-}
{-# INLINE strictAnd #-}
strictAnd (Bitstream _ v)
    = SV.all and v

strictOr ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bool
{-# RULES "or → strictOr" [1]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d).
    or v = strictOr v #-}
{-# INLINE strictOr #-}
strictOr (Bitstream _ v)
    = SV.any or v

strictIndex ∷ (G.Bitstream (Packet d), Integral n, Show n) ⇒ Bitstream d → n → Bool
{-# RULES "(!!) → strictIndex" [1]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d) n.
    v !! n = strictIndex v n #-}
{-# INLINEABLE strictIndex #-}
strictIndex (Bitstream _ v0) i0
    | i0 < 0    = indexOutOfRange i0
    | otherwise = go v0 i0
    where
      {-# INLINE go #-}
      go v i
          | SV.null v = indexOutOfRange i
          | otherwise = case SV.head v of
                          p | i < length p → p !! i
                            | otherwise    → go (SV.tail v) (i - length p)

emptyStream ∷ α
emptyStream
    = error "Data.Bitstream: empty stream"

{-# INLINE indexOutOfRange #-}
indexOutOfRange ∷ (Integral n, Show n) ⇒ n → α
indexOutOfRange n = error ("Data.Bitstream: index out of range: " L.++ show n)

-- | /O(n)/ Convert a strict 'BS.ByteString' into a strict
-- 'Bitstream'.
{-# INLINE fromByteString #-}
fromByteString ∷ BS.ByteString → Bitstream d
fromByteString bs0
    = Bitstream (nOctets ⋅ 8) (SV.unfoldrN nOctets go bs0)
    where
      nOctets ∷ Int
      {-# INLINE nOctets #-}
      nOctets = BS.length bs0
      {-# INLINE go #-}
      go bs = do (o, bs') ← BS.uncons bs
                 return (fromOctet o, bs')

-- | /O(n)/ @'toByteString' bits@ converts a strict 'Bitstream' @bits@
-- into a strict 'BS.ByteString'. The resulting octets will be padded
-- with zeroes if the 'length' of @bs@ is not multiple of 8.
{-# INLINEABLE toByteString #-}
toByteString ∷ ∀d. ( G.Bitstream (Bitstream d)
                   , G.Bitstream (Packet d)
                                     ) ⇒ Bitstream d → BS.ByteString
toByteString = unstreamBS
             ∘ (packPackets ∷ Stream Id Bool → Stream Id (Packet d))
             ∘ stream

unstreamBS ∷ Stream Id (Packet d) → BS.ByteString
{-# INLINE unstreamBS #-}
unstreamBS (Stream step s0 sz)
    = case upperBound sz of
        Just n  → fst $ BS.unfoldrN n (unId ∘ go) s0
        Nothing → BS.unfoldr (unId ∘ go) s0
      where
        {-# INLINE go #-}
        go s = do r ← step s
                  case r of
                    Yield p s' → return $ Just (toOctet p, s')
                    Skip    s' → go s'
                    Done       → return Nothing

-- WARNING: countBits is rather slow.
countBits ∷ (G.Bitstream (Packet d), Num n) ⇒ SV.Vector (Packet d) → n
{-# INLINE countBits #-}
countBits = SV.foldl' (\n p → n + length p) 0

-- | /O(n)/ Convert a 'SV.Vector' of 'Packet's into a 'Bitstream'.
fromPackets ∷ G.Bitstream (Packet d) ⇒ SV.Vector (Packet d) → Bitstream d
{-# INLINE fromPackets #-}
fromPackets v = Bitstream (countBits v) v

-- | /O(1)/ Convert a 'SV.Vector' of 'Packet's into a 'Bitstream',
-- with provided overall bit length. The correctness of the bit length
-- isn't checked, so you MUST be sure your bit length is absolutely
-- correct.
unsafeFromPackets ∷ G.Bitstream (Packet d) ⇒ Int → SV.Vector (Packet d) → Bitstream d
{-# INLINE unsafeFromPackets #-}
unsafeFromPackets = Bitstream

-- | /O(1)/ Convert a 'Bitstream' into a 'SV.Vector' of 'Packet's.
toPackets ∷ Bitstream d → SV.Vector (Packet d)
{-# INLINE toPackets #-}
toPackets (Bitstream _ d) = d

-- | /O(1)/ Convert a 'Bitstream' into a 'S.Stream' of 'Packet's.
streamPackets ∷ Bitstream d → S.Stream (Packet d)
{-# NOINLINE streamPackets #-}
streamPackets (Bitstream _ v) = GV.stream v

-- | /O(n)/ Convert a 'S.Stream' of 'Packet's into 'Bitstream'.
unstreamPackets ∷ G.Bitstream (Packet d) ⇒ S.Stream (Packet d) → Bitstream d
{-# NOINLINE unstreamPackets #-}
unstreamPackets s
    = let !v = GV.unstream s
          !l = countBits v
      in
        Bitstream l v

{-# RULES
"Strict Bitstream streamPackets/unstreamPackets fusion"
    ∀s. streamPackets (unstreamPackets s) = s

"Strict Bitstream unstreamPackets/streamPackets fusion"
    ∀v. unstreamPackets (streamPackets v) = v
  #-}

-- | /O(n)/ Convert a @'Bitstream' 'Left'@ into a @'Bitstream'
-- 'Right'@. Bit directions only affect octet-based operations such as
-- 'toByteString'.
directionLToR ∷ Bitstream Left → Bitstream Right
{-# INLINE directionLToR #-}
directionLToR (Bitstream l v) = Bitstream l (SV.map packetLToR v)

-- | /O(n)/ Convert a @'Bitstream' 'Right'@ into a @'Bitstream'
-- 'Left'@. Bit directions only affect octet-based operations such as
-- 'toByteString'.
directionRToL ∷ Bitstream Right → Bitstream Left
{-# INLINE directionRToL #-}
directionRToL (Bitstream l v) = Bitstream l (SV.map packetRToL v)

-- | /O(n)/ Read a 'Bitstream' from the stdin strictly, equivalent to
-- 'hGetContents' @stdin@. The 'Handle' is closed after the contents
-- have been read.
getContents ∷ G.Bitstream (Packet d) ⇒ IO (Bitstream d)
{-# INLINE getContents #-}
getContents = fmap fromByteString BS.getContents

-- | /O(n)/ Write a 'Bitstream' to the stdout, equivalent to 'hPut'
-- @stdout@.
putBits ∷ ( G.Bitstream (Bitstream d)
          , G.Bitstream (Packet d)
          )
        ⇒ Bitstream d
        → IO ()
{-# INLINE putBits #-}
putBits = BS.putStr ∘ toByteString

-- | The 'interact' function takes a function of type @'Bitstream' d
-- -> 'Bitstream' d@ as its argument. The entire input from the stdin
-- is passed to this function as its argument, and the resulting
-- 'Bitstream' is output on the stdout.
interact ∷ ( G.Bitstream (Bitstream d)
           , G.Bitstream (Packet d)
           )
         ⇒ (Bitstream d → Bitstream d)
         → IO ()
{-# INLINE interact #-}
interact = BS.interact ∘ lift'
    where
      {-# INLINE lift' #-}
      lift' f = toByteString ∘ f ∘ fromByteString

-- | /O(n)/ Read an entire file strictly into a 'Bitstream'.
readFile ∷ G.Bitstream (Packet d) ⇒ FilePath → IO (Bitstream d)
{-# INLINE readFile #-}
readFile = fmap fromByteString ∘ BS.readFile

-- | /O(n)/ Write a 'Bitstream' to a file.
writeFile ∷ ( G.Bitstream (Bitstream d)
            , G.Bitstream (Packet d)
            ) 
          ⇒ FilePath
          → Bitstream d
          → IO ()
{-# INLINE writeFile #-}
writeFile = (∘ toByteString) ∘ BS.writeFile

-- | /O(n)/ Append a 'Bitstream' to a file.
appendFile ∷ ( G.Bitstream (Bitstream d)
             , G.Bitstream (Packet d)
             )
           ⇒ FilePath
           → Bitstream d
           → IO ()
{-# INLINE appendFile #-}
appendFile = (∘ toByteString) ∘ BS.appendFile

-- | /O(n)/ Read entire handle contents strictly into a 'Bitstream'.
--
-- This function reads chunks at a time, doubling the chunksize on each
-- read. The final buffer is then realloced to the appropriate size. For
-- files > half of available memory, this may lead to memory exhaustion.
-- Consider using 'readFile' in this case.
--
-- The 'Handle' is closed once the contents have been read, or if an
-- exception is thrown.
hGetContents ∷ G.Bitstream (Packet d) ⇒ Handle → IO (Bitstream d)
{-# INLINE hGetContents #-}
hGetContents = fmap fromByteString ∘ BS.hGetContents

-- | /O(n)/ @'hGet' h n@ reads a 'Bitstream' directly from the
-- specified 'Handle' @h@. First argument @h@ is the 'Handle' to read
-- from, and the second @n@ is the number of /octets/ to read, not
-- /bits/. It returns the octets read, up to @n@, or null if EOF has
-- been reached.
--
-- If the handle is a pipe or socket, and the writing end is closed,
-- 'hGet' will behave as if EOF was reached.
hGet ∷ G.Bitstream (Packet d) ⇒ Handle → Int → IO (Bitstream d)
{-# INLINE hGet #-}
hGet = (fmap fromByteString ∘) ∘ BS.hGet

-- | /O(n)/ Like 'hGet', except that a shorter 'Bitstream' may be
-- returned if there are not enough octets immediately available to
-- satisfy the whole request. 'hGetSome' only blocks if there is no
-- data available, and EOF has not yet been reached.
hGetSome ∷ G.Bitstream (Packet d) ⇒ Handle → Int → IO (Bitstream d)
{-# INLINE hGetSome #-}
hGetSome = (fmap fromByteString ∘) ∘ BS.hGetSome

-- | /O(n)/ 'hGetNonBlocking' is similar to 'hGet', except that it
-- will never block waiting for data to become available. If there is
-- no data available to be read, 'hGetNonBlocking' returns 'empty'.
hGetNonBlocking ∷ G.Bitstream (Packet d) ⇒ Handle → Int → IO (Bitstream d)
{-# INLINE hGetNonBlocking #-}
hGetNonBlocking = (fmap fromByteString ∘) ∘ BS.hGetNonBlocking

-- | /O(n)/ Write a 'Bitstream' to the given 'Handle'.
hPut ∷ ( G.Bitstream (Bitstream d)
       , G.Bitstream (Packet d)
       )
     ⇒ Handle
     → Bitstream d
     → IO ()
{-# INLINE hPut #-}
hPut = (∘ toByteString) ∘ BS.hPut
