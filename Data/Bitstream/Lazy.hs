{-# LANGUAGE
    BangPatterns
  , FlexibleContexts
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
    , fromPackets
    , toPackets

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
    , head
    , uncons
    , last
    , tail
    , init
    , null
    , length

      -- * Transforming 'Bitstream's
    , map
    , reverse
    , intersperse
    , intercalate
    , transpose

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

      -- ** Accumulating maps
    , mapAccumL
    , mapAccumR

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
    , splitAt
    , takeWhile
    , dropWhile
    , span
    , break
    , group
    , groupBy
    , inits
    , tails

      -- * Predicates
    , isPrefixOf
    , isSuffixOf
    , isInfixOf

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
    , zip7
    , zipWith
    , zipWith3
    , zipWith4
    , zipWith5
    , zipWith6
    , zipWith7
    , unzip
    , unzip3
    , unzip4
    , unzip5
    , unzip6
    , unzip7

      -- * Special streams
      -- ** \"Set\" operations
    , nub
    , delete
    , (\\)
    , (∖)
    , (∆)
    , union
    , (∪)
    , intersect
    , (∩)
    , nubBy
    , deleteBy
    , deleteFirstsBy
    , unionBy
    , intersectBy

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
import Data.Bitstream.Packet
import qualified Data.ByteString.Lazy as LS
import qualified Data.List as L
import Data.Monoid
import qualified Data.Vector.Fusion.Stream as S
import Data.Vector.Fusion.Stream.Monadic (Stream(..), Step(..))
import Data.Vector.Fusion.Stream.Size
import Data.Vector.Fusion.Util
import Foreign.Storable
import Prelude ( Bool(..), Either(..), Eq(..), Int, Integral, Maybe(..)
               , Monad(..), Num(..), Ord(..), Ordering(..), Show(..)
               , ($), error, flip, fmap, otherwise
               )
import Prelude.Unicode hiding ((⧺), (∈), (∉))
import System.IO (FilePath, Handle, IO)

-- 32 KiB * sizeOf (Packet d) == 64 KiB
chunkSize ∷ Num α ⇒ α
chunkSize = fromInteger (32 ⋅ 1024)
{-# INLINE chunkSize #-}

chunkBits ∷ Num α ⇒ α
chunkBits = chunkSize ⋅ 8

newtype Bitstream d
    = Empty
    | Chunk {-# UNPACK #-} !(SB.Bitstream d) (Bitstream d)

instance Show (Packet d) ⇒ Show (Bitstream d) where
    {-# INLINEABLE show #-}
    show (Bitstream v0)
        = L.concat
          [ "(L"
          , L.concat (L.unfoldr go v0)
          , ")"
          ]
        where
          {-# INLINE go #-}
          go v = do (p, v') ← LV.viewL v
                    return (show p, v')

instance Ord (Bitstream d) ⇒ Eq (Bitstream d) where
    {-# INLINE (==) #-}
    x == y = (x `compare` y) ≡ EQ

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

instance G.Bitstream (Packet d) ⇒ Monoid (Bitstream d) where
    mempty  = (∅)
    mappend = (⧺)
    mconcat = concat

instance G.Bitstream (Packet d) ⇒ G.Bitstream (Bitstream d) where
    {-# INLINE [0] pack #-}
    pack = Bitstream ∘ LV.unfoldr chunkSize f ∘ Just
        where
          {-# INLINE f #-}
          f Nothing   = Nothing
          f (Just xs) = case unfoldrN (8 ∷ Int) g xs of
                          (p, mxs')
                              | null p    → Nothing
                              | otherwise → Just (p, mxs')
          {-# INLINE g #-}
          g []     = Nothing
          g (x:xs) = Just (x, xs)

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

    {-# INLINE [2] cons #-}
    cons b (Bitstream v)
        = Bitstream (LV.cons (singleton b) v)

    {-# INLINEABLE [2] cons' #-}
    cons' b Empty
        = Chunk (SB.singleton b) Empty
    cons' b (Chunk x xs)
        | length x < (chunkBits ∷ Int)
            = Chunk (x `cons` b) xs
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
    append (Bitstream x) (Bitstream y)
        = Bitstream (LV.append x y)

    {-# INLINE [2] head #-}
    head (Bitstream v)
        = case LV.viewL v of
            Just (p, _) → head p
            Nothing     → emptyStream

    {-# INLINE [2] last #-}
    last = go ∘ toChunks
        where
          {-# INLINE go #-}
          go []     = emptyStream
          go [x]    = last x
          go (_:xs) = go xs

    {-# INLINEABLE [2] tail #-}
    tail (Bitstream v)
        = case LV.viewL v of
            Just (p, v')
                → case tail p of
                     p' | null p'   → Bitstream v'
                        | otherwise → Bitstream (p' `LV.cons` v')
            Nothing
                → emptyStream

    {-# INLINEABLE [2] init #-}
    init Empty           = emptyStream
    init (Chunk x Empty) = Chunk (init x) Empty
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

    {-# INLINE [2] concat #-}
    concat Empty ch           = ch
    concat ch Empty           = ch
    concat (Chunk x Empty) ch = Chunk x ch
    concat (Chunk x xs   ) ch = Chunk x (concat xs ch)

    {-# INLINEABLE replicate #-}
    replicate n b
        | n ≤ 0                  = Empty
        | n ≥ (chunkBits ∷ Int) = Chunk x (replicate (n - chunkBits) b)
        where
          x = replicate chunkBits b

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
    dropWhile f Empty        = Empty
    dropWhile f (Chunk x xs) = case dropWhile f x of
                                 x' | null x'   → dropWhile f xs
                                    | otherwise → Chunk x' xs

    {-# INLINEABLE [2] filter #-}
    filter _ Empty        = Empty
    filter f (Chunk x xs) = case filter f x of
                              x' | null x'   → filter f xs
                                 | otherwise → Chunk x' (filter f xs)

lazyNull ∷ Bitstream d → Bool
{-# RULES "null → lazyNull" [2] null = lazyNull #-}
{-# INLINE lazyNull #-}
lazyNull Empty = True
lazyNull _     = False

lazyLength ∷ (G.Bitstream (Packet n), Num n) ⇒ Bitstream d → n
{-# RULES "length → lazyLength" [2]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d).
    length v = lazyLength v #-}
{-# INLINE lazyLength #-}
lazyLength = go 0
    where
      {-# INLINE go #-}
      go !soFar Empty        = soFar
      go !soFar (Chunk x xs) = go (soFar + length x) xs

lazyAnd ∷ G.Bitstream (Packet d) ⇒ Bitstream d ⇒ Bool
{-# RULES "and → lazyAnd" [2]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d).
    and v = lazyAnd v #-}
{-# INLINEABLE lazyAnd #-}
lazyAnd Empty        = False
lazyAnd (Chunk x xs)
    | and x          = lazyAnd xs
    | otherwise      = False

lazyOr ∷ G.Bitstream (Packet d) ⇒ Bitstream d ⇒ Bool
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
      go Empty _
          = indexOutOfRange
      go (Chunk x xs) i
          | i < length x = x !! i
          | otherwise    = go xs (i - length x)

inconsistentState ∷ α
inconsistentState
    = error "Data.Bitstream.Lazy: internal error: inconsistent state"

emptyStream ∷ α
emptyStream
    = error "Data.Bitstream.Lazy: empty stream"

{-# INLINE indexOutOfRange #-}
indexOutOfRange ∷ Integral n ⇒ n → α
indexOutOfRange n = error ("Data.Bitstream.Lazy: index out of range: " L.++ show n)

fromChunks ∷ [SB.Bitstream d] → Bitstream d
{-# INLINE fromChunks #-}
fromChunks []     = Empty
fromChunks (x:xs) = Chunk x (fromChunks xs)

toChunks ∷ Bitstream d → [SB.Bitstream d]
{-# INLINE toChunks #-}
toChunks Empty        = []
toChunks (Chunk x xs) = x : toChunks xs

{-# INLINE fromByteString #-}
fromByteString ∷ LS.ByteString → Bitstream d
fromByteString = fromChunks ∘ L.map SB.fromByteString ∘ LS.toChunks

{-# INLINE toByteString #-}
toByteString ∷ G.Bitstream (Packet d) ⇒ Bitstream d → LS.ByteString
toByteString = L.map SB.toByteString ∘ toChunks

-- | /O(1)/ Convert a 'LV.Vector' of 'Packet's into a 'Bitstream'.
fromPackets ∷ [Packet d] → Bitstream d
{-# INLINE fromPackets #-}
fromPackets = unstreamPackets ∘ S.fromList

-- | /O(1)/ Convert a 'Bitstream' into a list of 'Packet's.
toPackets ∷ Bitstream d → [Packet d]
{-# INLINE toPackets #-}
toPackets = S.toList ∘ streamPackets

streamPackets ∷ Monad m ⇒ Bitstream d → Stream m (Packet d)
{-# INLINE [0] streamPackets #-}
streamPackets ch0 = Stream step ch0 Unknown
    where
      {-# INLINE step #-}
      step Empty        = return Done
      step (Chunk x xs) = return $ Yield x xs

unstreamPackets ∷ Monad m ⇒ Stream m (Packet d) → m (Bitstream d)
{-# INLINE [0] unstreamPackets #-}
unstreamPackets (Stream step s0 _) = go s0
    where
      {-# INLINE go #-}
      go s = do r ← step s
                case r of
                  Yield p s' → do xs ← go s'
                                  return $ Chunk p xs
                  Skip    s' → go s'
                  Done       → return Empty

{-# RULES
"Lazy Bitstream streamPackets/unstreamPackets fusion"
    ∀s. streamPackets (unstreamPackets s) = s

"Lazy Bitstream unstreamPackets/streamPackets fusion"
    ∀v. unstreamPackets (streamPackets v) = v
  #-}

-- | /O(n)/ Convert a @'Bitstream' 'Left'@ into a @'Bitstream'
-- 'Right'@. Bit directions only affect octet-based operations like
-- 'toByteString'.
directionLToR ∷ Bitstream Left → Bitstream Right
{-# INLINE directionLToR #-}
directionLToR Empty        = Empty
directionLToR (Chunk x xs) = Chunk (SB.directionLToR x) (directionLToR xs)

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
   involves unsafeInlineIO!
 -}
{-# INLINE iterate #-}
iterate ∷ G.Bitstream (Packet d) ⇒ (Bool → Bool) → Bool → Bitstream d
iterate f b = xs
    where
      xs = Chunk x xs
      x  = repeat chunkSize p
      p  = pack (L.take 8 (L.iterate f b))

{-# INLINE repeat #-}
repeat ∷ G.Bitstream (Packet d) ⇒ Bool → Bitstream d
repeat b = xs
    where
      xs = Chunk x xs
      x  = repeat chunkSize p
      p  = replicate 8 b

{-# INLINE cycle #-}
cycle ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bitstream d
cycle Empty          = emptyStream
cycle ch@(Chunk x _) = Chunk x ch

{-# INLINE getContents #-}
getContents ∷ G.Bitstream (Packet d) ⇒ IO (Bitstream d)
getContents = fmap fromByteString LS.getContents

{-# INLINE putBits #-}
putBits ∷ G.Bitstream (Packet d) ⇒ Bitstream d → IO ()
putBits = LS.putStr ∘ toByteString

{-# INLINE interact #-}
interact ∷ G.Bitstream (Packet d) ⇒ (Bitstream d → Bitstream d) → IO ()
interact = LS.interact ∘ lift'
    where
      {-# INLINE lift' #-}
      lift' f = toByteString ∘ f ∘ fromByteString

{-# INLINE readFile #-}
readFile ∷ G.Bitstream (Packet d) ⇒ FilePath → IO (Bitstream d)
readFile = fmap fromByteString ∘ LS.readFile

{-# INLINE writeFile #-}
writeFile ∷ G.Bitstream (Packet d) ⇒ FilePath → Bitstream d → IO ()
writeFile = (∘ toByteString) ∘ LS.writeFile

{-# INLINE appendFile #-}
appendFile ∷ G.Bitstream (Packet d) ⇒ FilePath → Bitstream d → IO ()
appendFile = (∘ toByteString) ∘ LS.appendFile

{-# INLINE hGetContents #-}
hGetContents ∷ G.Bitstream (Packet d) ⇒ Handle → IO (Bitstream d)
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

{-# INLINE hGetNonBlocking #-}
hGetNonBlocking ∷ G.Bitstream (Packet d) ⇒ Handle → Int → IO (Bitstream d)
hGetNonBlocking = (fmap fromByteString ∘) ∘ LS.hGetNonBlocking

{-# INLINE hPut #-}
hPut ∷ G.Bitstream (Packet d) ⇒ Handle → Bitstream d → IO ()
hPut = (∘ toByteString) ∘ LS.hPut
