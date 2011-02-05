{-# LANGUAGE
    FlexibleContexts
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
    )
    where
import qualified Data.Bitstream as Strict
import Data.Bitstream.Generic hiding (Bitstream)
import qualified Data.Bitstream.Generic as G
import Data.Bitstream.Internal
import Data.Bitstream.Packet
import qualified Data.ByteString.Lazy as LS
import qualified Data.List.Stream as L
import Data.Monoid
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Lazy as LV
import qualified Data.Stream as S
import Foreign.Storable
import Prelude ( Bool(..), Either(..), Eq(..), Int, Integral, Maybe(..)
               , Monad(..), Num(..), Ord(..), Ordering(..), Show(..), ($), div
               , error, fmap, fromIntegral, fst, otherwise
               )
import Prelude.Unicode hiding ((⧺), (∈), (∉))

-- 32 KiB * sizeOf (Packet d) == 64 KiB
chunkSize ∷ Num α ⇒ α
chunkSize = fromInteger (32 ⋅ 1024)
{-# INLINE chunkSize #-}

newtype Bitstream d
    = Bitstream (LV.Vector (Packet d))

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
    {-# SPECIALISE instance G.Bitstream (Bitstream Left ) #-}
    {-# SPECIALISE instance G.Bitstream (Bitstream Right) #-}

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

    {-# INLINE cons #-}
    cons b (Bitstream v)
        = Bitstream (LV.cons (singleton b) v)

    {-# INLINE snoc #-}
    snoc = snoc'

    {-# INLINE append #-}
    append (Bitstream x) (Bitstream y)
        = Bitstream (LV.append x y)

    {-# INLINE head #-}
    head (Bitstream v)
        = case LV.viewL v of
            Just (p, _) → head p
            Nothing     → emptyStream

    {-# INLINEABLE uncons #-}
    uncons (Bitstream v)
        = do (p, v') ← LV.viewL v
             case uncons p of
               Just (b, p')
                   | null p'   → return (b, Bitstream v')
                   | otherwise → return (b, Bitstream (p' `LV.cons` v'))
               Nothing         → inconsistentState

    {-# INLINE last #-}
    last (Bitstream v)
        = case LV.viewR v of
            Just (_, p) → last p
            Nothing     → emptyStream

    {-# INLINEABLE tail #-}
    tail (Bitstream v)
        = case LV.viewL v of
            Just (p, v')
                → case tail p of
                     p' | null p'   → Bitstream v'
                        | otherwise → Bitstream (p' `LV.cons` v')
            Nothing
                → emptyStream

    {-# INLINEABLE init #-}
    init (Bitstream v)
        = case LV.viewR v of
            Just (v', p)
                → case init p of
                     p' | null p'   → Bitstream v'
                        | otherwise → Bitstream (v' `snocLV'` p')
            Nothing
                → emptyStream

    {-# INLINE null #-}
    null (Bitstream v)
        = LV.null v

    {-# INLINE length #-}
    {-# SPECIALISE length ∷ Bitstream Left  → Int #-}
    {-# SPECIALISE length ∷ Bitstream Right → Int #-}
    length (Bitstream v)
        = LV.foldl' (\n p → n + length p) 0 v

    {-# INLINE map #-}
    map f (Bitstream v)
        = Bitstream (LV.map (map f) v)

    {-# INLINE reverse #-}
    reverse (Bitstream v)
        = Bitstream (LV.reverse (LV.map reverse v))

    {-# INLINEABLE foldl #-}
    foldl f β0 (Bitstream v0) = go β0 v0
        where
          {-# INLINE go #-}
          go β v = case LV.viewL v of
                      Just (p, v') → go (foldl f β p) v'
                      Nothing      → β

    {-# INLINEABLE foldl' #-}
    foldl' f β0 (Bitstream v0) = go β0 v0
        where
          {-# INLINE go #-}
          go β v = case LV.viewL v of
                      Just (p, v') → go (foldl' f β p) v'
                      Nothing      → β

    {-# INLINEABLE foldr #-}
    foldr f β0 (Bitstream v0) = go β0 v0
        where
          {-# INLINE go #-}
          go β v = case LV.viewR v of
                      Just (v', p) → go (foldr f β p) v'
                      Nothing      → β

    {-# INLINE concat #-}
    concat = Bitstream ∘ LV.fromChunks ∘ L.concatMap g
        where
          {-# INLINE g #-}
          g (Bitstream v) = LV.chunks v

    {-# INLINE concatMap #-}
    concatMap f (Bitstream v0) = Bitstream (LV.concat (L.map g (LV.unpack v0)))
        where
          {-# INLINE g #-}
          g = LV.concat ∘ L.map (i ∘ f) ∘ unpack
          {-# INLINE i #-}
          i (Bitstream v) = v

    {-# INLINE and #-}
    and (Bitstream v) = LV.all and v

    {-# INLINE or #-}
    or (Bitstream v) = LV.any or v

    {-# INLINE any #-}
    any f (Bitstream v) = LV.any (any f) v

    {-# INLINE all #-}
    all f (Bitstream v) = LV.all (all f) v

    {-# INLINEABLE replicate #-}
    {-# SPECIALISE replicate ∷ Int → Bool → Bitstream Left  #-}
    {-# SPECIALISE replicate ∷ Int → Bool → Bitstream Right #-}
    replicate n0 b
        | n0 ≤ 0    = (∅)
        | otherwise = Bitstream (LV.unfoldr chunkSize g n0)
        where
          {-# INLINE g #-}
          g 0 = Nothing
          g n = let n' = min 8 n
                    p  = replicate n' b
                in
                  Just (p, n-n')

    {-# INLINEABLE unfoldr #-}
    unfoldr f = Bitstream ∘ LV.unfoldr chunkSize g ∘ Just
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
        | otherwise = case LV.unfoldrResult chunkSize g (n0, Just β0) of
                        (v, mβ1) → (Bitstream v, mβ1)
        where
          {-# INLINE g #-}
          g (0, mβ     ) = Left mβ
          g (_, Nothing) = Left Nothing
          g (n, Just β ) = case unfoldrN (min 8 n) f β of
                            (p, mβ')
                                | null p    → Left Nothing
                                | otherwise → Right (p, (n - length p, mβ'))

    {-# INLINEABLE take #-}
    {-# SPECIALISE take ∷ Int → Bitstream Left  → Bitstream Left  #-}
    {-# SPECIALISE take ∷ Int → Bitstream Right → Bitstream Right #-}
    take n0 (Bitstream v0)
        | n0 ≤ 0    = (∅)
        | otherwise = Bitstream (LV.unfoldr chunkSize g (n0, v0))
        where
          {-# INLINE g #-}
          g (0, _) = Nothing
          g (n, v) = do (p, v') ← LV.viewL v
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
          g n v = case LV.viewL v of
                    Just (p, v')
                        | n ≥ length p → g (n - length p) v'
                        | otherwise    → drop n p `LV.cons` v'
                    Nothing            → v

    {-# INLINEABLE splitAt #-}
    {-# SPECIALISE splitAt ∷ Int → Bitstream Left  → (Bitstream Left , Bitstream Left ) #-}
    {-# SPECIALISE splitAt ∷ Int → Bitstream Right → (Bitstream Right, Bitstream Right) #-}
    splitAt n0 (Bitstream v0)
        = case unfoldrN n0 split' ((∅), v0) of
            (hd, Just (p, tl))
                | null p    → (hd, Bitstream tl)
                | otherwise → (hd, Bitstream (p `LV.cons` tl))
            (hd, Nothing)   → (hd, (∅))
        where
          {-# INLINE split' #-}
          split' (p, v)
              | null p    = do (p', v') ← LV.viewL v
                               (h , t ) ← uncons p'
                               return (h, (t, v'))
              | otherwise = do (h , t ) ← uncons p
                               return (h, (t, v))

    {-# INLINEABLE takeWhile #-}
    takeWhile f (Bitstream v0)
        = Bitstream (LV.unfoldr chunkSize g (Just v0))
        where
          {-# INLINE g #-}
          g mv = do v       ← mv
                    (p, v') ← LV.viewL v
                    case takeWhile f p of
                      p' | p ≡ p'    → Just (p', Just v')
                         | otherwise → Just (p', Nothing)

    {-# INLINEABLE dropWhile #-}
    dropWhile f (Bitstream v0) = Bitstream (g v0)
        where
          {-# INLINE g #-}
          g v = case LV.viewL v of
                  Just (p, v')
                      → case dropWhile f p of
                           p' | null p'   → g v'
                              | otherwise → p' `LV.cons` v'
                  Nothing
                      → LV.empty

inconsistentState ∷ α
inconsistentState
    = error "Data.Bitstream.Lazy: internal error: inconsistent state"

emptyStream ∷ α
emptyStream
    = error "Data.Bitstream.Lazy: empty stream"

{-# INLINE snocLV' #-}
snocLV' ∷ Storable α ⇒ LV.Vector α → α → LV.Vector α
snocLV' v α = LV.fromChunks (go (LV.chunks v))
    where
      {-# INLINE go #-}
      go []     = [SV.singleton α]
      go [x]    = [x `SV.snoc` α]
      go (x:xs) = x : go xs

{-# INLINE fromChunks #-}
fromChunks ∷ [Strict.Bitstream d] → Bitstream d
fromChunks = Bitstream ∘ LV.fromChunks ∘ L.map Strict.toPackets

{-# INLINE toChunks #-}
toChunks ∷ Bitstream d → [Strict.Bitstream d]
toChunks (Bitstream v) = L.map Strict.fromPackets (LV.chunks v)

{-# INLINE fromByteString #-}
fromByteString ∷ LS.ByteString → Bitstream d
fromByteString = Bitstream ∘ fromLBS

{-# INLINE toByteString #-}
toByteString ∷ G.Bitstream (Packet d) ⇒ Bitstream d → LS.ByteString
toByteString (Bitstream v) = toLBS v

{-# INLINE directionLToR #-}
directionLToR ∷ Bitstream Left → Bitstream Right
directionLToR (Bitstream v) = Bitstream (LV.map packetLToR v)

{-# INLINE directionRToL #-}
directionRToL ∷ Bitstream Right → Bitstream Left
directionRToL (Bitstream v) = Bitstream (LV.map packetRToL v)

{-# INLINEABLE cons' #-}
{-# SPECIALISE cons' ∷ Bool → Bitstream Left  → Bitstream Left  #-}
{-# SPECIALISE cons' ∷ Bool → Bitstream Right → Bitstream Right #-}
cons' ∷ G.Bitstream (Packet d) ⇒ Bool → Bitstream d → Bitstream d
cons' b (Bitstream v) = Bitstream (LV.fromChunks (go (LV.chunks v)))
    where
      {-# INLINE go #-}
      go []     = [SV.singleton (singleton b)]
      go (x:xs) = case SV.viewL x of
                    Just (p, ps)
                        | length p < (8 ∷ Int)
                              → SV.cons (cons b p) ps : xs
                        | SV.length x < chunkSize
                              → SV.cons (singleton b) x : xs
                        | otherwise
                              → SV.singleton (singleton b) : x : xs
                    Nothing   → inconsistentState

{-# INLINEABLE snoc' #-}
{-# SPECIALISE snoc' ∷ Bitstream Left  → Bool → Bitstream Left  #-}
{-# SPECIALISE snoc' ∷ Bitstream Right → Bool → Bitstream Right #-}
snoc' ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bool → Bitstream d
snoc' (Bitstream v) b = Bitstream (LV.fromChunks (go (LV.chunks v)))
    where
      {-# INLINE go #-}
      go []     = [SV.singleton (singleton b)]
      go (x:[]) = case SV.viewR x of
                    Just (ps, p)
                        | length p < (8 ∷ Int)
                              → [SV.snoc ps (snoc p b)]
                        | SV.length x < chunkSize
                              → [SV.snoc x (singleton b)]
                        | otherwise
                              → [x, SV.singleton (singleton b)]
                    Nothing   → [SV.singleton (singleton b)]
      go (x:xs) = x : go xs

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
iterate f b
    = let p = pack (L.take 8 (L.iterate f b))
      in
        Bitstream (LV.repeat chunkSize p)

{-# INLINE repeat #-}
repeat ∷ G.Bitstream (Packet d) ⇒ Bool → Bitstream d
repeat b
    = let p = pack (L.replicate 8 b)
      in
        Bitstream (LV.repeat chunkSize p)

{-# INLINE cycle #-}
cycle ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bitstream d
cycle (Bitstream v) = Bitstream (LV.cycle v)
