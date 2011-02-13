{-# LANGUAGE
    FlexibleContexts
  , ScopedTypeVariables
  , UndecidableInstances
  , UnicodeSyntax
  #-}
-- | Fast, packed, strict bit streams with optional stream fusion.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions. e.g.
--
-- > import qualified Data.BitStream as BS
--
-- Strict 'Bitstream's are made of strict 'SV.Vector' of 'Packet's,
-- and each 'Packet's have at least 1 bit.
--
-- Note that stream fusion does NOT automatically occurs as there are
-- possibilities that stream fusion produces a slower code for this
-- data structure. See 'unstream' for more details.
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
--import Data.Bitstream.Internal
import Data.Bitstream.Packet
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Monoid
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Fusion.Stream as S
import Data.Vector.Fusion.Stream.Monadic (Stream(..), Step(..))
import Data.Vector.Fusion.Stream.Size
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
    stream (Bitstream v) = S.concatMap stream (GV.stream v)

    {-# INLINE [0] unstream #-}
    unstream (Stream step s0 sz)
        = Bitstream (GV.unstream (Stream packPackets ((∅), Just s0) sz'))
        where
          sz' ∷ Size
          {-# INLINE sz' #-}
          sz' = case sz of
                  Exact n → Exact (n+7 `div` 8)
                  Max   n → Max   (n+7 `div` 8)
                  Unknown → Unknown
          {-# INLINE packPackets #-}
          packPackets (p, Just s)
              = do r ← step s
                   case r of
                     Yield b s'
                         | full p    → return $ Yield p (singleton b, Just s')
                         | otherwise → return $ Skip    (p `snoc` b , Just s')
                     Skip    s'      → return $ Skip    (p          , Just s')
                     Done
                         | null p    → return Done
                         | otherwise → return $ Yield p ((⊥)       , Nothing)
          packPackets (_, Nothing)
              = return Done

    {-# INLINEABLE cons #-}
    cons b (Bitstream v)
        | SV.null v = Bitstream (SV.singleton (singleton b))
        | otherwise = case SV.head v of
                        p | length p < (8 ∷ Int)
                                → Bitstream ((b `cons` p) `SV.cons` SV.tail v)
                          | otherwise
                                → Bitstream (singleton b `SV.cons` v)

    {-# INLINEABLE snoc #-}
    snoc (Bitstream v) b
        | SV.null v = Bitstream (SV.singleton (singleton b))
        | otherwise = case SV.last v of
                        p | length p < (8 ∷ Int)
                                → Bitstream (SV.init v `SV.snoc` (p `snoc` b))
                          | otherwise
                                → Bitstream (v `SV.snoc` singleton b)

    {-# INLINE append #-}
    append (Bitstream x) (Bitstream y)
        = Bitstream (x SV.++ y)

    {-# INLINEABLE tail #-}
    tail (Bitstream v)
        | SV.null v = emptyStream
        | otherwise = case tail (SV.head v) of
                        p' | null p'   → Bitstream (SV.tail v)
                           | otherwise → Bitstream (p' `SV.cons` SV.tail v)

    {-# INLINEABLE init #-}
    init (Bitstream v)
        | SV.null v = emptyStream
        | otherwise = case init (SV.last v) of
                        p' | null p'   → Bitstream (SV.init v)
                           | otherwise → Bitstream (SV.init v `SV.snoc` p')

    {-# INLINE map #-}
    map f (Bitstream v)
        = Bitstream (SV.map (map f) v)

    {-# INLINE reverse #-}
    reverse (Bitstream v)
        = Bitstream (SV.reverse (SV.map reverse v))

    {-# INLINE scanl #-}
    scanl f b
        = unstream ∘ S.scanl f b ∘ stream

    {-# INLINE concat #-}
    concat = Bitstream ∘ SV.concat ∘ L.map toPackets

    {-# INLINEABLE replicate #-}
    replicate n0 b
        | n0 ≤ 0    = (∅)
        | otherwise = Bitstream (anterior `SV.snoc` posterior)
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

    {-# INLINEABLE take #-}
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

    {-# INLINEABLE drop #-}
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

    {-# INLINEABLE takeWhile #-}
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

    {-# INLINEABLE dropWhile #-}
    dropWhile f (Bitstream v0) = Bitstream (go v0)
        where
          {-# INLINE go #-}
          go v | SV.null v = v
               | otherwise = case dropWhile f (SV.head v) of
                               p' | null p'   → go (SV.tail v)
                                  | otherwise → p' `SV.cons` SV.tail v

    {-# INLINEABLE filter #-}
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
{-# INLINE strictLength #-}
strictLength (Bitstream v)
    = SV.foldl' (\n p → n + length p) 0 v

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
{-# INLINE toByteString #-}
toByteString ∷ G.Bitstream (Packet d) ⇒ Bitstream d → BS.ByteString
toByteString (Bitstream v0)
    = fst $ BS.unfoldrN nOctets go ((∅), (∅), v0)
    where
      {-# INLINE nOctets #-}
      nOctets ∷ Int
      nOctets = SV.length v0
      {-# INLINE go #-}
      go (p, r, v)
          | full p
              = Just (toOctet p, ((∅), r, v))
          | null r
              = case SV.null v of
                  False           → go (p, SV.head v, SV.tail v)
                  True
                      | null p    → Nothing
                      | otherwise → Just (toOctet p, ((∅), (∅), SV.empty))
          | otherwise
              = let lenR ∷ Int
                    lenR = 8 - length p
                    rH   = take lenR r
                    rT   = drop lenR r
                in
                  go (p ⧺ rH, rT, v)

-- | /O(1)/ Convert a 'SV.Vector' of 'Packet's into a 'Bitstream'.
{-# INLINE fromPackets #-}
fromPackets ∷ SV.Vector (Packet d) → Bitstream d
fromPackets = Bitstream

-- | /O(1)/ Convert a 'Bitstream' into a 'SV.Vector' of 'Packet's.
{-# INLINE toPackets #-}
toPackets ∷ Bitstream d → SV.Vector (Packet d)
toPackets (Bitstream d) = d

-- | /O(n)/ Convert a @'Bitstream' 'Left'@ into a @'Bitstream'
-- 'Right'@. Bit directions only affect octet-based operations like
-- 'toByteString'.
{-# INLINE directionLToR #-}
directionLToR ∷ Bitstream Left → Bitstream Right
directionLToR (Bitstream v) = Bitstream (SV.map packetLToR v)

-- | /O(n)/ Convert a @'Bitstream' 'Right'@ into a @'Bitstream'
-- 'Left'@. Bit directions only affect octet-based operations like
-- 'toByteString'.
{-# INLINE directionRToL #-}
directionRToL ∷ Bitstream Right → Bitstream Left
directionRToL (Bitstream v) = Bitstream (SV.map packetRToL v)

{-# INLINE getContents #-}
getContents ∷ G.Bitstream (Packet d) ⇒ IO (Bitstream d)
getContents = fmap fromByteString BS.getContents

{-# INLINE putBits #-}
putBits ∷ G.Bitstream (Packet d) ⇒ Bitstream d → IO ()
putBits = BS.putStr ∘ toByteString

{-# INLINE interact #-}
interact ∷ G.Bitstream (Packet d) ⇒ (Bitstream d → Bitstream d) → IO ()
interact = BS.interact ∘ lift'
    where
      {-# INLINE lift' #-}
      lift' f = toByteString ∘ f ∘ fromByteString

{-# INLINE readFile #-}
readFile ∷ G.Bitstream (Packet d) ⇒ FilePath → IO (Bitstream d)
readFile = fmap fromByteString ∘ BS.readFile

{-# INLINE writeFile #-}
writeFile ∷ G.Bitstream (Packet d) ⇒ FilePath → Bitstream d → IO ()
writeFile = (∘ toByteString) ∘ BS.writeFile

{-# INLINE appendFile #-}
appendFile ∷ G.Bitstream (Packet d) ⇒ FilePath → Bitstream d → IO ()
appendFile = (∘ toByteString) ∘ BS.appendFile

{-# INLINE hGetContents #-}
hGetContents ∷ G.Bitstream (Packet d) ⇒ Handle → IO (Bitstream d)
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
{-# INLINE hGet #-}
hGet ∷ G.Bitstream (Packet d) ⇒ Handle → Int → IO (Bitstream d)
hGet = (fmap fromByteString ∘) ∘ BS.hGet

{-# INLINE hGetSome #-}
hGetSome ∷ G.Bitstream (Packet d) ⇒ Handle → Int → IO (Bitstream d)
hGetSome = (fmap fromByteString ∘) ∘ BS.hGetSome

{-# INLINE hGetNonBlocking #-}
hGetNonBlocking ∷ G.Bitstream (Packet d) ⇒ Handle → Int → IO (Bitstream d)
hGetNonBlocking = (fmap fromByteString ∘) ∘ BS.hGetNonBlocking

{-# INLINE hPut #-}
hPut ∷ G.Bitstream (Packet d) ⇒ Handle → Bitstream d → IO ()
hPut = (∘ toByteString) ∘ BS.hPut
