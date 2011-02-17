{-# LANGUAGE
    BangPatterns
  , FlexibleContexts
  , ScopedTypeVariables
  , UndecidableInstances
  , UnicodeSyntax
  #-}
-- | Fast, packed, lazy bit streams (i.e. list of 'Bool's) with
-- semi-automatic stream fusion.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions. e.g.
--
-- > import qualified Data.BitStream.Lazy as LS
--
-- Lazy 'Bitstream's are made of possibly infinite list of strict
-- 'SB.Bitstream's as chunks, and each chunks have at least 1 bit.
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

      -- ** Converting from\/to lazy 'LS.ByteString's
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

      -- ** Replications
    , iterate
    , repeat
    , replicate
    , cycle

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
    , hGetNonBlocking
    , hPut
    )
    where
import qualified Data.Bitstream as SB
import Data.Bitstream.Generic hiding (Bitstream)
import qualified Data.Bitstream.Generic as G
import Data.Bitstream.Internal
import Data.Bitstream.Packet
import qualified Data.ByteString.Lazy as LS
import qualified Data.List as L
import Data.Monoid
import qualified Data.Vector.Fusion.Stream as S
import Data.Vector.Fusion.Stream.Monadic (Stream(..), Step(..))
import Data.Vector.Fusion.Stream.Size
import Data.Vector.Fusion.Util
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.New as New
import qualified Data.Vector.Generic.Mutable as MVector
import qualified Data.Vector.Storable as SV
import Prelude ( Bool(..), Eq(..), Int, Integral, Maybe(..)
               , Monad(..), Num(..), Ord(..), Show(..)
               , ($), div, error, fmap, otherwise
               )
import Prelude.Unicode hiding ((⧺), (∈), (∉))
import System.IO (FilePath, Handle, IO)

-- 32 KiB * sizeOf (Packet d) == 64 KiB
chunkSize ∷ Num α ⇒ α
chunkSize = fromInteger (32 ⋅ 1024)
{-# INLINE chunkSize #-}

chunkBits ∷ Num α ⇒ α
chunkBits = chunkSize ⋅ 8

-- | A space-efficient representation of a 'Bool' vector, supporting
-- many efficient operations. 'Bitstream's have an idea of
-- /directions/ controlling how octets are interpreted as bits. There
-- are two types of concrete 'Bitstream's: @'Bitstream' 'Left'@ and
-- @'Bitstream' 'Right'@.
data Bitstream d
    = Empty
    | Chunk {-# UNPACK #-} !(SB.Bitstream d) (Bitstream d)

instance Show (Packet d) ⇒ Show (Bitstream d) where
    {-# INLINEABLE show #-}
    show ch
        = L.concat
          [ "[L: "
          , L.concat (L.intersperse " " (L.map show (toChunks ch)))
          , " ]"
          ]

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
    stream
        = {-# CORE "Lazy Bitstream stream" #-}
          S.concatMap stream ∘ streamChunks

    {-# INLINE [0] unstream #-}
    unstream
        = {-# CORE "Lazy Bitstream unstream" #-}
          unId ∘ unstreamChunks ∘ packChunks ∘ packPackets

    {-# INLINE [2] cons #-}
    cons b = Chunk (singleton b)

    {-# INLINEABLE [2] cons' #-}
    cons' b Empty
        = Chunk (SB.singleton b) Empty
    cons' b (Chunk x xs)
        | length x < (chunkBits ∷ Int)
            = Chunk (b `cons` x) xs
        | otherwise
            = Chunk (singleton b) (Chunk x xs)

    {-# INLINEABLE [2] snoc #-}
    snoc Empty b
        = Chunk (SB.singleton b) Empty
    snoc (Chunk x Empty) b
        | length x < (chunkBits ∷ Int)
            = Chunk (x `snoc` b) Empty
        | otherwise
            = Chunk x (Chunk (singleton b) Empty)
    snoc (Chunk x xs) b
        = Chunk x (xs `snoc` b)

    {-# INLINE [2] append #-}
    append Empty ch        = ch
    append (Chunk x xs) ch = Chunk x (append xs ch)

    {-# INLINEABLE [2] tail #-}
    tail Empty        = emptyStream
    tail (Chunk x xs) = case tail x of
                          x' | null x'   → xs
                             | otherwise → Chunk x' xs

    {-# INLINEABLE [2] init #-}
    init Empty           = emptyStream
    init (Chunk x Empty) = case init x of
                             x' | null x'   → Empty
                                | otherwise → Chunk x' Empty
    init (Chunk x xs   ) = Chunk x (init xs)

    {-# INLINE [2] map #-}
    map _ Empty        = Empty
    map f (Chunk x xs) = Chunk (map f x) (map f xs)

    {-# INLINEABLE [2] reverse #-}
    reverse ch0 = go ch0 Empty
        where
          {-# INLINE go #-}
          go Empty        ch = ch
          go (Chunk x xs) ch = go xs (Chunk (reverse x) ch)

    {-# INLINE [2] scanl #-}
    scanl f b ch
        = Chunk (singleton b)
                (case ch of
                   Empty      → Empty
                   Chunk x xs → let h   = head x
                                    x'  = scanl f (f b h) (tail x)
                                    l   = last x'
                                    x'' = init x'
                                    xs' = scanl f l xs
                                in
                                  if null x''
                                  then xs'
                                  else Chunk x'' xs')

    {-# INLINE [2] concat #-}
    concat = fromChunks ∘ L.concatMap toChunks

    {-# INLINEABLE replicate #-}
    replicate n b
        | n ≤ 0         = Empty
        | n < chunkBits = Chunk (replicate n b) Empty
        | otherwise     = Chunk x (replicate (n - chunkBits) b)
        where
          x = replicate (chunkBits ∷ Int) b

    {-# INLINEABLE [2] take #-}
    take _ Empty        = Empty
    take n (Chunk x xs)
        | n ≤ 0         = Empty
        | n ≥ length x  = Chunk x (take (n - length x) xs)
        | otherwise     = Chunk (take n x) Empty

    {-# INLINEABLE [2] drop #-}
    drop _ Empty       = Empty
    drop n (Chunk x xs)
        | n ≤ 0        = Chunk x xs
        | n ≥ length x = drop (n - length x) xs
        | otherwise    = Chunk (drop n x) xs

    {-# INLINEABLE [2] takeWhile #-}
    takeWhile _ Empty        = Empty
    takeWhile f (Chunk x xs) = case takeWhile f x of
                                 x' | x ≡ x'    → Chunk x' (takeWhile f xs)
                                    | otherwise → Chunk x' Empty

    {-# INLINEABLE [2] dropWhile #-}
    dropWhile _ Empty        = Empty
    dropWhile f (Chunk x xs) = case dropWhile f x of
                                 x' | null x'   → dropWhile f xs
                                    | otherwise → Chunk x' xs

    {-# INLINEABLE [2] filter #-}
    filter _ Empty        = Empty
    filter f (Chunk x xs) = case filter f x of
                              x' | null x'   → filter f xs
                                 | otherwise → Chunk x' (filter f xs)

lazyHead ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bool
{-# RULES "head → lazyHead" [2]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d).
    head v = lazyHead v #-}
{-# INLINE lazyHead #-}
lazyHead Empty       = emptyStream
lazyHead (Chunk x _) = head x

lazyLast ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bool
{-# RULES "last → lazyLast" [2]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d).
    last v = lazyLast v #-}
{-# INLINE lazyLast #-}
lazyLast Empty           = emptyStream
lazyLast (Chunk x Empty) = last x
lazyLast (Chunk _ xs   ) = lazyLast xs

lazyNull ∷ Bitstream d → Bool
{-# RULES "null → lazyNull" [2] null = lazyNull #-}
{-# INLINE lazyNull #-}
lazyNull Empty = True
lazyNull _     = False

lazyLength ∷ (G.Bitstream (Packet d), Num n) ⇒ Bitstream d → n
{-# RULES "length → lazyLength" [2]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d).
    length v = lazyLength v #-}
{-# INLINE lazyLength #-}
lazyLength = go 0
    where
      {-# INLINE go #-}
      go !soFar Empty        = soFar
      go !soFar (Chunk x xs) = go (soFar + length x) xs

lazyAnd ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bool
{-# RULES "and → lazyAnd" [2]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d).
    and v = lazyAnd v #-}
{-# INLINEABLE lazyAnd #-}
lazyAnd Empty        = False
lazyAnd (Chunk x xs)
    | and x          = lazyAnd xs
    | otherwise      = False

lazyOr ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bool
{-# RULES "or → lazyOr" [2]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d).
    or v = lazyOr v #-}
{-# INLINEABLE lazyOr #-}
lazyOr Empty        = True
lazyOr (Chunk x xs)
    | or x          = True
    | otherwise     = lazyOr xs

lazyIndex ∷ (G.Bitstream (Packet d), Integral n) ⇒ Bitstream d → n → Bool
{-# RULES "(!!) → lazyIndex" [2]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d) n.
    v !! n = lazyIndex v n #-}
{-# INLINEABLE lazyIndex #-}
lazyIndex ch0 i0
    | i0 < 0    = indexOutOfRange i0
    | otherwise = go ch0 i0
    where
      {-# INLINE go #-}
      go Empty        _  = indexOutOfRange i0
      go (Chunk x xs) i
          | i < length x = x !! i
          | otherwise    = go xs (i - length x)

emptyStream ∷ α
emptyStream
    = error "Data.Bitstream.Lazy: empty stream"

{-# INLINE indexOutOfRange #-}
indexOutOfRange ∷ Integral n ⇒ n → α
indexOutOfRange n = error ("Data.Bitstream.Lazy: index out of range: " L.++ show n)

-- | /O(n)/ Convert a list of chunks, strict 'SB.Bitstream's, into a
-- lazy 'Bitstream'.
fromChunks ∷ G.Bitstream (Packet d) ⇒ [SB.Bitstream d] → Bitstream d
{-# INLINE fromChunks #-}
fromChunks []     = Empty
fromChunks (x:xs)
    | null x      = fromChunks xs
    | otherwise   = Chunk x (fromChunks xs)

-- | /O(n)/ Convert a lazy 'Bitstream' into a list of chunks, strict
-- 'SB.Bitstream's.
toChunks ∷ Bitstream d → [SB.Bitstream d]
{-# INLINE toChunks #-}
toChunks Empty        = []
toChunks (Chunk x xs) = x : toChunks xs

-- | /O(n)/ Convert a lazy 'LS.ByteString' into a lazy 'Bitstream'.
fromByteString ∷ G.Bitstream (Packet d) ⇒ LS.ByteString → Bitstream d
{-# INLINE fromByteString #-}
fromByteString = fromChunks ∘ L.map SB.fromByteString ∘ LS.toChunks

-- | /O(n)/ @'toByteString' bits@ converts a lazy 'Bitstream' @bits@
-- into a lazy 'LS.ByteString'. The resulting octets will be padded
-- with zeroes if @bs@ is finite and its 'length' is not multiple of
-- 8.
toByteString ∷ G.Bitstream (Packet d) ⇒ Bitstream d → LS.ByteString
{-# INLINE toByteString #-}
toByteString = LS.fromChunks ∘ L.map SB.toByteString ∘ toChunks

streamChunks ∷ Monad m ⇒ Bitstream d → Stream m (SB.Bitstream d)
{-# INLINE [0] streamChunks #-}
streamChunks ch0 = Stream step ch0 Unknown
    where
      {-# INLINE step #-}
      step Empty        = return Done
      step (Chunk x xs) = return $ Yield x xs

unstreamChunks ∷ (G.Bitstream (Packet d), Monad m)
               ⇒ Stream m (SB.Bitstream d)
               → m (Bitstream d)
{-# INLINE [0] unstreamChunks #-}
unstreamChunks (Stream step s0 _) = go s0
    where
      {-# INLINE go #-}
      go s = do r ← step s
                case r of
                  Yield x s' → do xs ← go s'
                                  if null x
                                     then return xs
                                     else return $ Chunk x xs
                  Skip    s' → go s'
                  Done       → return Empty

{-# RULES
"Lazy Bitstream streamChunks/unstreamChunks fusion"
    ∀s. streamChunks (unId (unstreamChunks s)) = s

"Lazy Bitstream unstreamChunks/streamChunks fusion"
    ∀v. unId (unstreamChunks (streamChunks v)) = v
  #-}

-- Awful implementation to gain speed...
packChunks ∷ ∀m d. Monad m
           ⇒ Stream m (Packet d)
           → Stream m (SB.Bitstream d)
{-# INLINE packChunks #-}
packChunks (Stream step s0 sz)
    = Stream step' (emptyChunk, 0, Just s0) sz'
    where
      emptyChunk ∷ New.New SV.Vector (Packet d)
      {-# INLINE emptyChunk #-}
      emptyChunk
          = New.create (MVector.new chunkSize)

      newChunk ∷ New.New SV.Vector (Packet d)
               → Int
               → SB.Bitstream d
      {-# INLINE newChunk #-}
      newChunk ch len
          = SB.fromPackets
            $ GV.new
            $ New.apply (MVector.take len) ch

      writePacket ∷ New.New SV.Vector (Packet d)
                  → Int
                  → Packet d
                  → New.New SV.Vector (Packet d)
      {-# INLINE writePacket #-}
      writePacket ch len p
          = New.modify (\mv → MVector.write mv len p) ch

      sz' ∷ Size
      {-# INLINE sz' #-}
      sz' = case sz of
              Exact n → Exact (n + chunkSize - 1 `div` chunkSize)
              Max   n → Max   (n + chunkSize - 1 `div` chunkSize)
              Unknown → Unknown

      {-# INLINE step' #-}
      step' (ch, len, Just s)
          = do r ← step s
               case r of
                 Yield p s'
                     | len ≡ chunkSize
                           → return $ Yield (newChunk ch len)
                                            (emptyChunk, 0, Just s')
                     | otherwise
                           → return $ Skip  (writePacket ch len p, len+1, Just s')
                 Skip s'   → return $ Skip  (ch             , len  , Just s')
                 Done
                     | len ≡ 0
                           → return Done
                     | otherwise
                           → return $ Yield (newChunk ch len)
                                            ((⊥), (⊥), Nothing)
      step' (_, _, Nothing)
          = return Done

-- | /O(n)/ Convert a @'Bitstream' 'Left'@ into a @'Bitstream'
-- 'Right'@. Bit directions only affect octet-based operations such as
-- 'toByteString'.
directionLToR ∷ Bitstream Left → Bitstream Right
{-# INLINE directionLToR #-}
directionLToR Empty        = Empty
directionLToR (Chunk x xs) = Chunk (SB.directionLToR x) (directionLToR xs)

-- | /O(n)/ Convert a @'Bitstream' 'Right'@ into a @'Bitstream'
-- 'Left'@. Bit directions only affect octet-based operations such as
-- 'toByteString'.
directionRToL ∷ Bitstream Right → Bitstream Left
{-# INLINE directionRToL #-}
directionRToL Empty        = Empty
directionRToL (Chunk x xs) = Chunk (SB.directionRToL x) (directionRToL xs)

{- There are only 4 functions of the type Bool → Bool.

   * iterate id b            == [b    , b    , b    , b    , ...]
   * iterate (const True ) _ == [True , True , True , True , ...]
   * iterate (const False) _ == [False, False, False, False, ...]
   * iterate not True        == [True , False, True , False, ...]
   * iterate not False       == [False, True , False, True , ...]

   As seen above, all of them are cyclic so we just replicate the
   first 8 bits i.e. a single Packet. Dunno when the given function
   involves unsafeInlineIO and produces random bits.
 -}
-- | /O(n)/ 'iterate' @f x@ returns an infinite 'Bitstream' of
-- repeated applications of @f@ to @x@:
--
-- @
-- 'iterate' f x == [x, f x, f (f x), ...]
-- @
iterate ∷ G.Bitstream (Packet d) ⇒ (Bool → Bool) → Bool → Bitstream d
{-# INLINE iterate #-}
iterate f b = xs
    where
      xs = Chunk x xs
      x  = SB.fromPackets (SV.replicate chunkSize p)
      p  = pack (L.take 8 (L.iterate f b))

-- | /O(n)/ 'repeat' @x@ is an infinite 'Bitstream', with @x@ the
-- value of every bits.
repeat ∷ G.Bitstream (Packet d) ⇒ Bool → Bitstream d
{-# INLINE repeat #-}
repeat b = xs
    where
      xs = Chunk x xs
      x  = replicate (chunkBits ∷ Int) b

-- | /O(n)/ 'cycle' ties a finite 'Bitstream' into a circular one, or
-- equivalently, the infinite repetition of the original 'Bitstream'.
-- It is the identity on infinite 'Bitstream's.
cycle ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bitstream d
{-# INLINE cycle #-}
cycle Empty = emptyStream
cycle ch    = ch ⧺ cycle ch

-- | /O(n)/ 'getContents' is equivalent to 'hGetContents'
-- @stdin@. Will read /lazily/.
getContents ∷ G.Bitstream (Packet d) ⇒ IO (Bitstream d)
{-# INLINE getContents #-}
getContents = fmap fromByteString LS.getContents

-- | /O(n)/ Write a 'Bitstream' to @stdout@, equivalent to 'hPut'
-- @stdout@.
putBits ∷ G.Bitstream (Packet d) ⇒ Bitstream d → IO ()
{-# INLINE putBits #-}
putBits = LS.putStr ∘ toByteString

-- | The 'interact' function takes a function of type @'Bitstream' d
-- -> 'Bitstream' d@ as its argument. The entire input from the stdin
-- is lazily passed to this function as its argument, and the
-- resulting 'Bitstream' is output on the stdout.
interact ∷ G.Bitstream (Packet d) ⇒ (Bitstream d → Bitstream d) → IO ()
{-# INLINE interact #-}
interact = LS.interact ∘ lift'
    where
      {-# INLINE lift' #-}
      lift' f = toByteString ∘ f ∘ fromByteString

-- | /O(n)/ Read an entire file lazily into a 'Bitstream'.
readFile ∷ G.Bitstream (Packet d) ⇒ FilePath → IO (Bitstream d)
{-# INLINE readFile #-}
readFile = fmap fromByteString ∘ LS.readFile

-- | /O(n)/ Write a 'Bitstream' to a file.
writeFile ∷ G.Bitstream (Packet d) ⇒ FilePath → Bitstream d → IO ()
{-# INLINE writeFile #-}
writeFile = (∘ toByteString) ∘ LS.writeFile

-- | /O(n)/ Append a 'Bitstream' to a file.
appendFile ∷ G.Bitstream (Packet d) ⇒ FilePath → Bitstream d → IO ()
{-# INLINE appendFile #-}
appendFile = (∘ toByteString) ∘ LS.appendFile

-- | /O(n)/ Read entire handle contents /lazily/ into a
-- 'Bitstream'. Chunks are read on demand, using the default chunk
-- size.
--
-- Once EOF is encountered, the 'Handle' is closed.
hGetContents ∷ G.Bitstream (Packet d) ⇒ Handle → IO (Bitstream d)
{-# INLINE hGetContents #-}
hGetContents = fmap fromByteString ∘ LS.hGetContents

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
hGet = (fmap fromByteString ∘) ∘ LS.hGet

-- | /O(n)/ 'hGetNonBlocking' is similar to 'hGet', except that it
-- will never block waiting for data to become available, instead it
-- returns only whatever data is available.
{-# INLINE hGetNonBlocking #-}
hGetNonBlocking ∷ G.Bitstream (Packet d) ⇒ Handle → Int → IO (Bitstream d)
hGetNonBlocking = (fmap fromByteString ∘) ∘ LS.hGetNonBlocking

-- | /O(n)/ Write a 'Bitstream' to the given 'Handle'.
hPut ∷ G.Bitstream (Packet d) ⇒ Handle → Bitstream d → IO ()
{-# INLINE hPut #-}
hPut = (∘ toByteString) ∘ LS.hPut
