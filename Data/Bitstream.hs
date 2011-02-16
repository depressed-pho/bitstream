{-# LANGUAGE
    BangPatterns
  , FlexibleContexts
  , ScopedTypeVariables
  , UndecidableInstances
  , UnicodeSyntax
  #-}
-- | Fast, packed, strict bit streams (i.e. list of 'Bool's) with
-- semi-automatic stream fusion.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions. e.g.
--
-- > import qualified Data.BitStream as BS
--
-- Strict 'Bitstream's are made of strict 'SV.Vector' of 'Packet's,
-- and each 'Packet's have at least 1 bit.
module Data.Bitstream
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
    , fromPackets
    , toPackets

      -- ** Converting from\/to strict 'BS.ByteString's
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

      -- * Building lists
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
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Fusion.Stream as S
import Data.Vector.Fusion.Stream.Monadic (Stream(..), Step(..))
import Data.Vector.Fusion.Stream.Size
import Data.Vector.Fusion.Util
import Prelude ( Bool(..), Eq(..), Int, Integral, Maybe(..), Monad(..), Num(..)
               , Ord(..), Show(..), ($), div, error, fmap
               , fromIntegral, fst, mod, otherwise
               )
import Prelude.Unicode hiding ((⧺), (∈), (∉))
import System.IO (FilePath, Handle, IO)

-- THINKME: Use vector instead of storablevector.

-- | A space-efficient representation of a 'Bool' vector, supporting
-- many efficient operations. 'Bitstream's have an idea of
-- /directions/ controlling how octets are interpreted as bits. There
-- are two types of concrete 'Bitstream's: @'Bitstream' 'Left'@ and
-- @'Bitstream' 'Right'@.
newtype Bitstream d
    = Bitstream (SV.Vector (Packet d))

instance Show (Packet d) ⇒ Show (Bitstream d) where
    {-# INLINEABLE show #-}
    show (Bitstream v0)
        = L.concat
          [ "(S"
          , L.concat (L.unfoldr go v0)
          , ")"
          ]
        where
          {-# INLINE go #-}
          go v | SV.null v = Nothing
               | otherwise = Just (show (SV.head v), SV.tail v)

instance G.Bitstream (Packet d) ⇒ Eq (Bitstream d) where
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
instance G.Bitstream (Packet d) ⇒ Ord (Bitstream d) where
    {-# INLINE compare #-}
    x `compare` y = stream x `compare` stream y

-- | 'Bitstream' forms 'Monoid' in the same way as ordinary lists:
--
-- @
-- 'mempty'  = 'empty'
-- 'mappend' = 'append'
-- 'mconcat' = 'concat'
-- @
instance G.Bitstream (Packet d) ⇒ Monoid (Bitstream d) where
    mempty  = (∅)
    mappend = (⧺)
    mconcat = concat

instance G.Bitstream (Packet d) ⇒ G.Bitstream (Bitstream d) where
    {-# INLINE [0] stream #-}
    stream (Bitstream v)
        = {-# CORE "Bitstream stream" #-}
          S.concatMap stream (GV.stream v)
          `S.sized`
          Exact (length (Bitstream v))

    {-# INLINE [0] unstream #-}
    unstream
        = {-# CORE "Bitstream unstream" #-}
          Bitstream ∘ GV.unstream ∘ packPackets

    {-# INLINEABLE [2] cons #-}
    cons b (Bitstream v)
        | SV.null v = Bitstream (SV.singleton (singleton b))
        | otherwise = case SV.head v of
                        p | length p < (8 ∷ Int)
                                → Bitstream ((b `cons` p) `SV.cons` SV.tail v)
                          | otherwise
                                → Bitstream (singleton b `SV.cons` v)

    {-# INLINEABLE [2] snoc #-}
    snoc (Bitstream v) b
        | SV.null v = Bitstream (SV.singleton (singleton b))
        | otherwise = case SV.last v of
                        p | length p < (8 ∷ Int)
                                → Bitstream (SV.init v `SV.snoc` (p `snoc` b))
                          | otherwise
                                → Bitstream (v `SV.snoc` singleton b)

    {-# INLINE [2] append #-}
    append (Bitstream x) (Bitstream y)
        = Bitstream (x SV.++ y)

    {-# INLINEABLE [2] tail #-}
    tail (Bitstream v)
        | SV.null v = emptyStream
        | otherwise = case tail (SV.head v) of
                        p' | null p'   → Bitstream (SV.tail v)
                           | otherwise → Bitstream (p' `SV.cons` SV.tail v)

    {-# INLINEABLE [2] init #-}
    init (Bitstream v)
        | SV.null v = emptyStream
        | otherwise = case init (SV.last v) of
                        p' | null p'   → Bitstream (SV.init v)
                           | otherwise → Bitstream (SV.init v `SV.snoc` p')

    {-# INLINE [2] map #-}
    map f (Bitstream v)
        = Bitstream (SV.map (map f) v)

    {-# INLINE [2] reverse #-}
    reverse (Bitstream v)
        = Bitstream (SV.reverse (SV.map reverse v))

    {-# INLINE [1] scanl #-}
    scanl f b
        = unstream ∘ S.scanl f b ∘ stream

    {-# INLINE [2] concat #-}
    concat = Bitstream ∘ SV.concat ∘ L.map toPackets

    {-# INLINEABLE replicate #-}
    replicate n0 b
        | n0 ≤ 0         = (∅)
        | n0 `mod` 8 ≡ 0 = Bitstream anterior
        | otherwise      = Bitstream (anterior `SV.snoc` posterior)
        where
          {-# INLINE anterior #-}
          anterior = SV.replicate n p
              where
                n ∷ Int
                {-# INLINE n #-}
                n = fromIntegral (n0 `div` 8)
                {-# INLINE p #-}
                p = replicate (8 ∷ Int) b

          {-# INLINE posterior #-}
          posterior = replicate n b
              where
                n ∷ Int
                {-# INLINE n #-}
                n = fromIntegral (n0 `mod` 8)

    {-# INLINEABLE [2] take #-}
    take n0 (Bitstream v0)
        | n0 ≤ 0    = (∅)
        | otherwise = Bitstream (SV.unfoldrN nOctets go (n0, v0))
        where
          {-# INLINE nOctets #-}
          nOctets ∷ Int
          nOctets = fromIntegral (min n0 (fromIntegral (SV.length v0)))
          {-# INLINE go #-}
          go (0, _) = Nothing
          go (n, v)
              | SV.null v = Nothing
              | otherwise = let p  = SV.head v
                                v' = SV.tail v
                                p' = take n p
                                n' = n - length p'
                            in
                              return (p', (n', v'))

    {-# INLINEABLE [2] drop #-}
    drop n0 (Bitstream v0)
        | n0 ≤ 0    = Bitstream v0
        | otherwise = Bitstream (go n0 v0)
        where
          {-# INLINE go #-}
          go 0 v = v
          go n v
              | SV.null v = v
              | otherwise = case SV.head v of
                              p | n ≥ length p → go (n - length p) (SV.tail v)
                                | otherwise    → drop n p `SV.cons` (SV.tail v)

    {-# INLINEABLE [2] takeWhile #-}
    takeWhile f (Bitstream v0)
        = Bitstream (GV.unstream (takeWhilePS (GV.stream v0)))
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

    {-# INLINEABLE [2] dropWhile #-}
    dropWhile f (Bitstream v0) = Bitstream (go v0)
        where
          {-# INLINE go #-}
          go v | SV.null v = v
               | otherwise = case dropWhile f (SV.head v) of
                               p' | null p'   → go (SV.tail v)
                                  | otherwise → p' `SV.cons` SV.tail v

    {-# INLINEABLE [2] filter #-}
    filter f (Bitstream v0)
        = Bitstream (GV.unstream (filterPS (GV.stream v0)))
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
{-# RULES "head → strictHead" [2]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d).
    head v = strictHead v #-}
{-# INLINE strictHead #-}
strictHead (Bitstream v) = head (SV.head v)

strictLast ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bool
{-# RULES "last → strictLast" [2]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d).
    last v = strictLast v #-}
{-# INLINE strictLast #-}
strictLast (Bitstream v) = last (SV.last v)

strictNull ∷ Bitstream d → Bool
{-# RULES "null → strictNull" [2] null = strictNull #-}
{-# INLINE strictNull #-}
strictNull (Bitstream v) = SV.null v

strictLength ∷ (G.Bitstream (Packet d), Num n) ⇒ Bitstream d → n
{-# RULES "length → strictLength" [2]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d).
    length v = strictLength v #-}
{-# INLINEABLE strictLength #-}
strictLength (Bitstream v)
    = SV.foldl' (\n p → n + length p) 0 v

strictAnd ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bool
{-# RULES "and → strictAnd" [2]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d).
    and v = strictAnd v #-}
{-# INLINE strictAnd #-}
strictAnd (Bitstream v)
    = SV.all and v

strictOr ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bool
{-# RULES "or → strictOr" [2]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d).
    or v = strictOr v #-}
{-# INLINE strictOr #-}
strictOr (Bitstream v)
    = SV.any or v

strictIndex ∷ (G.Bitstream (Packet d), Integral n) ⇒ Bitstream d → n → Bool
{-# RULES "(!!) → strictIndex" [2]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d) n.
    v !! n = strictIndex v n #-}
{-# INLINEABLE strictIndex #-}
strictIndex (Bitstream v0) i0
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
indexOutOfRange ∷ Integral n ⇒ n → α
indexOutOfRange n = error ("Data.Bitstream: index out of range: " L.++ show n)

-- | /O(n)/ Convert a 'BS.ByteString' into a 'Bitstream'.
{-# INLINE fromByteString #-}
fromByteString ∷ BS.ByteString → Bitstream d
fromByteString bs0 = Bitstream (SV.unfoldrN nOctets go bs0)
    where
      {-# INLINE nOctets #-}
      nOctets ∷ Int
      nOctets = BS.length bs0
      {-# INLINE go #-}
      go bs = do (o, bs') ← BS.uncons bs
                 return (fromOctet o, bs')

-- | /O(n)/ @'toByteString' bs@ converts a 'Bitstream' @bits@ into a
-- 'BS.ByteString'. The resulting octets will be padded with zeroes if
-- the 'length' of @bs@ is not multiple of 8.
{-# INLINEABLE toByteString #-}
toByteString ∷ ∀d. G.Bitstream (Packet d) ⇒ Bitstream d → BS.ByteString
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

-- | /O(1)/ Convert a 'SV.Vector' of 'Packet's into a 'Bitstream'.
fromPackets ∷ SV.Vector (Packet d) → Bitstream d
{-# INLINE fromPackets #-}
fromPackets = Bitstream

-- | /O(1)/ Convert a 'Bitstream' into a 'SV.Vector' of 'Packet's.
toPackets ∷ Bitstream d → SV.Vector (Packet d)
{-# INLINE toPackets #-}
toPackets (Bitstream d) = d

-- | /O(n)/ Convert a @'Bitstream' 'Left'@ into a @'Bitstream'
-- 'Right'@. Bit directions only affect octet-based operations like
-- 'toByteString'.
directionLToR ∷ Bitstream Left → Bitstream Right
{-# INLINE directionLToR #-}
directionLToR (Bitstream v) = Bitstream (SV.map packetLToR v)

-- | /O(n)/ Convert a @'Bitstream' 'Right'@ into a @'Bitstream'
-- 'Left'@. Bit directions only affect octet-based operations like
-- 'toByteString'.
directionRToL ∷ Bitstream Right → Bitstream Left
{-# INLINE directionRToL #-}
directionRToL (Bitstream v) = Bitstream (SV.map packetRToL v)

getContents ∷ G.Bitstream (Packet d) ⇒ IO (Bitstream d)
{-# INLINE getContents #-}
getContents = fmap fromByteString BS.getContents

putBits ∷ G.Bitstream (Packet d) ⇒ Bitstream d → IO ()
{-# INLINE putBits #-}
putBits = BS.putStr ∘ toByteString

interact ∷ G.Bitstream (Packet d) ⇒ (Bitstream d → Bitstream d) → IO ()
{-# INLINE interact #-}
interact = BS.interact ∘ lift'
    where
      {-# INLINE lift' #-}
      lift' f = toByteString ∘ f ∘ fromByteString

readFile ∷ G.Bitstream (Packet d) ⇒ FilePath → IO (Bitstream d)
{-# INLINE readFile #-}
readFile = fmap fromByteString ∘ BS.readFile

writeFile ∷ G.Bitstream (Packet d) ⇒ FilePath → Bitstream d → IO ()
{-# INLINE writeFile #-}
writeFile = (∘ toByteString) ∘ BS.writeFile

appendFile ∷ G.Bitstream (Packet d) ⇒ FilePath → Bitstream d → IO ()
{-# INLINE appendFile #-}
appendFile = (∘ toByteString) ∘ BS.appendFile

hGetContents ∷ G.Bitstream (Packet d) ⇒ Handle → IO (Bitstream d)
{-# INLINE hGetContents #-}
hGetContents = fmap fromByteString ∘ BS.hGetContents

-- |@'hGet' h n@ reads a 'Bitstream' directly from the specified
-- 'Handle' @h@. First argument @h@ is the 'Handle' to read from, and
-- the second @n@ is the number of /octets/ to read, not /bits/. It
-- returns the octets read, up to @n@, or null if EOF has been
-- reached.
--
-- If the handle is a pipe or socket, and the writing end is closed,
-- 'hGet' will behave as if EOF was reached.
--
hGet ∷ G.Bitstream (Packet d) ⇒ Handle → Int → IO (Bitstream d)
{-# INLINE hGet #-}
hGet = (fmap fromByteString ∘) ∘ BS.hGet

hGetSome ∷ G.Bitstream (Packet d) ⇒ Handle → Int → IO (Bitstream d)
{-# INLINE hGetSome #-}
hGetSome = (fmap fromByteString ∘) ∘ BS.hGetSome

hGetNonBlocking ∷ G.Bitstream (Packet d) ⇒ Handle → Int → IO (Bitstream d)
{-# INLINE hGetNonBlocking #-}
hGetNonBlocking = (fmap fromByteString ∘) ∘ BS.hGetNonBlocking

hPut ∷ G.Bitstream (Packet d) ⇒ Handle → Bitstream d → IO ()
{-# INLINE hPut #-}
hPut = (∘ toByteString) ∘ BS.hPut
