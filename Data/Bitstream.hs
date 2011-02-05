{-# LANGUAGE
    FlexibleContexts
  , ScopedTypeVariables
  , UndecidableInstances
  , UnicodeSyntax
  #-}
-- | Fast, packed, strict bit vectors.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions. e.g.
--
-- > import qualified Data.BitStream as BS
--
-- FIXME: explain about directions
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

      -- ** Replication
    , replicate

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
import qualified Data.List.Stream as L
import Data.Monoid
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SV
import qualified Data.Stream as S
import Foreign.Marshal.Array
import Foreign.Storable
import Prelude ( Bool(..), Eq(..), Int, Integral, Maybe(..), Monad(..), Num(..)
               , Ord(..), Ordering(..), Show(..), ($), div, error, fmap
               , fromIntegral, fst, otherwise
               )
import Prelude.Unicode hiding ((⧺), (∈), (∉))
import System.IO (FilePath, Handle, IO)
import System.IO.Unsafe

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
          go v = do (p, v') ← SV.viewL v
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
-- @
instance G.Bitstream (Packet d) ⇒ Ord (Bitstream d) where
    {-# INLINEABLE compare #-}
    (Bitstream x0) `compare` (Bitstream y0) = go ((∅), x0) ((∅), y0)
        where
          {-# INLINE go #-}
          go (px, x) (py, y)
              | null px
                  = case SV.viewL x of
                      Just (px', x')
                          → go (px', x') (py, y)
                      Nothing
                          → if null py then
                                case SV.viewL y of
                                  Just _  → LT
                                  Nothing → EQ
                            else
                                LT
              | null py
                  = case SV.viewL y of
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
    {-# SPECIALISE instance G.Bitstream (Bitstream Left ) #-}
    {-# SPECIALISE instance G.Bitstream (Bitstream Right) #-}

    {-# INLINEABLE [0] pack #-}
    pack xs0 = Bitstream (fst $ SV.unfoldrN l f xs0)
        where
          {-# INLINE l #-}
          l ∷ Int
          l = (L.length xs0 + 7) `div` 8
          {-# INLINE f #-}
          f xs = case L.splitAt 8 xs of
                   (hd, tl)
                       | L.null hd → Nothing
                       | otherwise → Just (pack hd, tl)

    {-# INLINEABLE [0] unpack #-}
    unpack (Bitstream v) = L.concatMap unpack (SV.unpack v)

    {-# INLINE [0] stream #-}
    stream (Bitstream v)
        = S.concatMap stream (streamSV v)

    {-# INLINE [0] unstream #-}
    unstream
        = Bitstream ∘ unstreamSV ∘ packStream

    {-# INLINE empty #-}
    empty = Bitstream SV.empty

    {-# INLINE singleton #-}
    singleton b
        = Bitstream (SV.singleton (singleton b))

    {-# INLINEABLE cons #-}
    cons b (Bitstream v)
        = case SV.viewL v of
            Just (p, v')
                | length p < (8 ∷ Int)
                      → Bitstream (SV.cons (cons    b p) v')
                | otherwise
                      → Bitstream (SV.cons (singleton b) v )
            Nothing   → Bitstream (SV.singleton (singleton b))

    {-# INLINEABLE snoc #-}
    snoc (Bitstream v) b
        = case SV.viewR v of
            Just (v', p)
                | length p < (8 ∷ Int)
                      → Bitstream (SV.snoc v' (snoc    p b))
                | otherwise
                      → Bitstream (SV.snoc v  (singleton b))
            Nothing   → Bitstream (SV.singleton (singleton b))

    {-# INLINE append #-}
    append (Bitstream x) (Bitstream y)
        = Bitstream (SV.append x y)

    {-# INLINE head #-}
    head (Bitstream v)
        = head (SV.head v)

    {-# INLINEABLE uncons #-}
    uncons (Bitstream v)
        = do (p, v') ← SV.viewL v
             case uncons p of
               Just (b, p')
                   | null p'   → return (b, Bitstream v')
                   | otherwise → return (b, Bitstream (p' `SV.cons` v'))
               Nothing         → inconsistentState

    {-# INLINE last #-}
    last (Bitstream v)
        = last (SV.last v)

    {-# INLINEABLE tail #-}
    tail (Bitstream v)
        = case SV.viewL v of
            Just (p, v')
                → case tail p of
                     p' | null p'   → Bitstream v'
                        | otherwise → Bitstream (p' `SV.cons` v')
            Nothing
                → emptyStream

    {-# INLINEABLE init #-}
    init (Bitstream v)
        = case SV.viewR v of
            Just (v', p)
                → case init p of
                     p' | null p'   → Bitstream v'
                        | otherwise → Bitstream (v' `SV.snoc` p')
            Nothing
                → emptyStream

    {-# INLINE null #-}
    null (Bitstream v)
        = SV.null v

    {-# INLINE length #-}
    {-# SPECIALISE length ∷ Bitstream Left  → Int #-}
    {-# SPECIALISE length ∷ Bitstream Right → Int #-}
    length (Bitstream v)
        = SV.foldl' (\n p → n + length p) 0 v

    {-# INLINE map #-}
    map f (Bitstream v)
        = Bitstream (SV.map (map f) v)

    {-# INLINE reverse #-}
    reverse (Bitstream v)
        = Bitstream (SV.reverse (SV.map reverse v))

    {-# INLINEABLE foldl #-}
    foldl f β0 (Bitstream v0) = go β0 v0
        where
          {-# INLINE go #-}
          go β v = case SV.viewL v of
                      Just (p, v') → go (foldl f β p) v'
                      Nothing      → β

    {-# INLINEABLE foldl' #-}
    foldl' f β0 (Bitstream v0) = go β0 v0
        where
          {-# INLINE go #-}
          go β v = case SV.viewL v of
                      Just (p, v') → go (foldl' f β p) v'
                      Nothing      → β

    {-# INLINEABLE foldr #-}
    foldr f β0 (Bitstream v0) = go β0 v0
        where
          {-# INLINE go #-}
          go β v = case SV.viewR v of
                      Just (v', p) → go (foldr f β p) v'
                      Nothing      → β

    {-# INLINE concat #-}
    concat = Bitstream ∘ SV.concat ∘ L.map g
        where
          {-# INLINE g #-}
          g (Bitstream v) = v

    {-# INLINE concatMap #-}
    concatMap f (Bitstream v0) = Bitstream (SV.concatMap g v0)
        where
          {-# INLINE g #-}
          g = SV.concat ∘ L.map (i ∘ f) ∘ unpack
          {-# INLINE i #-}
          i (Bitstream v) = v

    {-# INLINE and #-}
    and (Bitstream v) = SV.all and v

    {-# INLINE or #-}
    or (Bitstream v) = SV.any or v

    {-# INLINE any #-}
    any f (Bitstream v) = SV.any (any f) v

    {-# INLINE all #-}
    all f (Bitstream v) = SV.all (all f) v

    {-# INLINEABLE replicate #-}
    {-# SPECIALISE replicate ∷ Int → Bool → Bitstream Left  #-}
    {-# SPECIALISE replicate ∷ Int → Bool → Bitstream Right #-}
    replicate n0 b
        | n0 ≤ 0    = (∅)
        | otherwise = Bitstream (fst $ SV.unfoldrN len g n0)
        where
          {-# INLINE len #-}
          len ∷ Int
          len = fromIntegral ((n0 + 7) `div` 8)
          {-# INLINE g #-}
          g 0 = Nothing
          g n = let n' = min 8 n
                    p  = replicate n' b
                in
                  Just (p, n-n')

    {-# INLINEABLE unfoldr #-}
    unfoldr f = Bitstream ∘ SV.unfoldr g ∘ Just
        where
          {-# INLINE g #-}
          g Nothing  = Nothing
          g (Just β) = case unfoldrN (8 ∷ Int) f β of
                          (p, mβ')
                              | null p    → Nothing
                              | otherwise → Just (p, mβ')

    {-# INLINEABLE unfoldrN #-}
    {-# SPECIALISE unfoldrN ∷ Int → (β → Maybe (Bool, β)) → β → (Bitstream Left , Maybe β) #-}
    {-# SPECIALISE unfoldrN ∷ Int → (β → Maybe (Bool, β)) → β → (Bitstream Right, Maybe β) #-}
    unfoldrN n0 f β0
        | n0 ≤ 0    = ((∅), Just β0)
        | otherwise = case unsafePerformIO $ SV.createAndTrim' l $ \p → go p l n0 β0 of
                        (v, mβ1) → (Bitstream v, mβ1)
        where
          {-# INLINE l #-}
          l ∷ Int
          l = fromIntegral ((n0 + 7) `div` 8)
          {-# INLINE go #-}
          go _ 0 _ β = return (0, l, Just β)
          go p i n β = case consume8 n β (∅) of
                          (pk, Just β')
                              | null pk   → return (0, l-i, Just β')
                              | otherwise → do poke p pk
                                               go (advancePtr p 1) (i-1) (n - length pk) β'
                          (pk, Nothing)
                              | null pk   → return (0, l-i, Nothing)
                              | otherwise → do poke p pk
                                               return (0, l-i+1, Nothing)
          {-# INLINE consume8 #-}
          consume8 0 β p  = (p, Just β)
          consume8 n β p
              | full p    = (p, Just β)
              | otherwise = case f β of
                              Nothing
                                  → (p, Nothing)
                              Just (b, β')
                                  → consume8 (n-1) β' (p `snoc` b)

    {-# INLINEABLE take #-}
    {-# SPECIALISE take ∷ Int → Bitstream Left  → Bitstream Left  #-}
    {-# SPECIALISE take ∷ Int → Bitstream Right → Bitstream Right #-}
    take n0 (Bitstream v0)
        | n0 ≤ 0    = (∅)
        | otherwise = Bitstream (SV.unfoldr g (n0, v0))
        where
          {-# INLINE g #-}
          g (0, _) = Nothing
          g (n, v) = do (p, v') ← SV.viewL v
                        let p' = take n p
                            n' = n - length p'
                        return (p', (n', v'))

    {-# INLINEABLE drop #-}
    {-# SPECIALISE drop ∷ Int → Bitstream Left  → Bitstream Left  #-}
    {-# SPECIALISE drop ∷ Int → Bitstream Right → Bitstream Right #-}
    drop n0 (Bitstream v0)
        | n0 ≤ 0    = Bitstream v0
        | otherwise = Bitstream (g n0 v0)
        where
          {-# INLINE g #-}
          g 0 v = v
          g n v = case SV.viewL v of
                    Just (p, v')
                        | n ≥ length p → g (n - length p) v'
                        | otherwise    → drop n p `SV.cons` v'
                    Nothing            → v

    {-# INLINEABLE splitAt #-}
    {-# SPECIALISE splitAt ∷ Int → Bitstream Left  → (Bitstream Left , Bitstream Left ) #-}
    {-# SPECIALISE splitAt ∷ Int → Bitstream Right → (Bitstream Right, Bitstream Right) #-}
    splitAt n0 (Bitstream v0)
        = case unfoldrN n0 split' ((∅), v0) of
            (hd, Just (p, tl))
                | null p    → (hd, Bitstream tl)
                | otherwise → (hd, Bitstream (p `SV.cons` tl))
            (hd, Nothing)   → (hd, (∅))
        where
          {-# INLINE split' #-}
          split' (p, v)
              | null p    = do (p', v') ← SV.viewL v
                               (h , t ) ← uncons p'
                               return (h, (t, v'))
              | otherwise = do (h , t ) ← uncons p
                               return (h, (t, v))

    {-# INLINEABLE takeWhile #-}
    takeWhile f (Bitstream v0)
        = Bitstream (fst $ SV.unfoldrN (SV.length v0) g (Just v0))
        where
          {-# INLINE g #-}
          g mv = do v       ← mv
                    (p, v') ← SV.viewL v
                    case takeWhile f p of
                      p' | p ≡ p'    → Just (p', Just v')
                         | otherwise → Just (p', Nothing)

    {-# INLINEABLE dropWhile #-}
    dropWhile f (Bitstream v0) = Bitstream (g v0)
        where
          {-# INLINE g #-}
          g v = case SV.viewL v of
                  Just (p, v')
                      → case dropWhile f p of
                           p' | null p'   → g v'
                              | otherwise → p' `SV.cons` v'
                  Nothing
                      → SV.empty

    {-# INLINEABLE isPrefixOf #-}
    isPrefixOf (Bitstream x0) (Bitstream y0) = go ((∅), x0) ((∅), y0)
        where
          {-# INLINE go #-}
          go (px, x) (py, y)
              | null px
                  = case SV.viewL x of
                      Just (px', x') → go (px', x') (py, y)
                      Nothing        → True
              | null py
                  = case SV.viewL y of
                      Just (py', y') → go (px, x) (py', y')
                      Nothing        → False
              | otherwise
                  = let n          ∷ Int
                        n          = min (length px) (length py)
                        (pxH, pxT) = splitAt n px
                        (pyH, pyT) = splitAt n py
                    in
                      if pxH ≡ pyH then
                          go (pxT, x) (pyT, y)
                      else
                          False

    {-# INLINEABLE find #-}
    find f (Bitstream v0) = go v0
        where
          {-# INLINE go #-}
          go v = case SV.viewL v of
                   Just (p, v')
                       → case find f p of
                            r@(Just _) → r
                            Nothing    → go v'
                   Nothing
                       → Nothing

    {-# INLINEABLE filter #-}
    filter f (Bitstream v0)
        = Bitstream (fst $ SV.unfoldrN (SV.length v0) g v0)
        where
          {-# INLINE g #-}
          g v = do (p, v') ← SV.viewL v
                   case filter f p of
                     p' | null p'   → g v'
                        | otherwise → return (p', v')

    {-# INLINEABLE (!!) #-}
    {-# SPECIALISE (!!) ∷ Bitstream Left  → Int → Bool #-}
    {-# SPECIALISE (!!) ∷ Bitstream Right → Int → Bool #-}
    (Bitstream v0) !! i0
        | i0 < 0    = indexOutOfRange i0
        | otherwise = go v0 i0
        where
          {-# INLINE go #-}
          go v i = case SV.viewL v of
                     Just (p, v')
                         | i < length p → p !! i
                         | otherwise    → go v' (i - length p)
                     Nothing            → indexOutOfRange i

    {-# INLINEABLE findIndex #-}
    {-# SPECIALISE findIndex ∷ (Bool → Bool) → Bitstream Left  → Maybe Int #-}
    {-# SPECIALISE findIndex ∷ (Bool → Bool) → Bitstream Right → Maybe Int #-}
    findIndex f (Bitstream v0) = go v0 0
        where
          {-# INLINE go #-}
          go v i = do (p, v') ← SV.viewL v
                      case findIndex f p of
                        Just j  → return (i + j)
                        Nothing → go v' (i + length p)

    {-# INLINEABLE findIndices #-}
    {-# SPECIALISE findIndices ∷ (Bool → Bool) → Bitstream Left  → [Int] #-}
    {-# SPECIALISE findIndices ∷ (Bool → Bool) → Bitstream Right → [Int] #-}
    findIndices f (Bitstream v0) = go 0 v0
        where
          {-# INLINE go #-}
          go i v = case SV.viewL v of
                     Just (p, v')
                         → let is   = L.map (+ i) (findIndices f p)
                               rest = go (i + length p) v'
                           in
                             is L.++ rest
                     Nothing
                         → []

inconsistentState ∷ α
inconsistentState
    = error "Data.Bitstream: internal error: inconsistent state"

emptyStream ∷ α
emptyStream
    = error "Data.Bitstream: empty stream"

{-# INLINE indexOutOfRange #-}
indexOutOfRange ∷ Integral n ⇒ n → α
indexOutOfRange n = error ("Data.Bitstream: index out of range: " L.++ show n)

{-# INLINE fromByteString #-}
fromByteString ∷ BS.ByteString → Bitstream d
fromByteString = Bitstream ∘ fromBS

{-# INLINE toByteString #-}
toByteString ∷ G.Bitstream (Packet d) ⇒ Bitstream d → BS.ByteString
toByteString (Bitstream v) = toBS v

{-# INLINE fromPackets #-}
fromPackets ∷ SV.Vector (Packet d) → Bitstream d
fromPackets = Bitstream

{-# INLINE toPackets #-}
toPackets ∷ Bitstream d → SV.Vector (Packet d)
toPackets (Bitstream d) = d

{-# INLINE directionLToR #-}
directionLToR ∷ Bitstream Left → Bitstream Right
directionLToR (Bitstream v) = Bitstream (SV.map packetLToR v)

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
