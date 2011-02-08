{-# LANGUAGE
    BangPatterns
  , RankNTypes
  , UnicodeSyntax
  #-}
module Data.Bitstream.Generic
    ( Bitstream(..)

    , pack
    , unpack

    , empty
    , singleton

    , scanr
    , scanr1
{-
    , (∅)
    , (⧺)
    , (∈)
    , (∋)
    , (∉)
    , (∌)
    , (∖)
    , (∪)
    , (∩)
    , (∆)
-}
    )
    where
--import Control.Monad
import qualified Data.List as L
--import Data.Maybe
import Data.Vector.Fusion.Stream (Stream)
import qualified Data.Vector.Fusion.Stream as S
import Prelude ( Bool(..), Integer, Integral(..), Num(..), Ord(..), ($), error
               , flip, otherwise
               )
import Prelude.Unicode hiding ((∈), (∉), (⧺))

{-
infix  4 ∈, ∋, ∉, ∌, `elem`, `notElem`
infixr 5 ⧺, `append`
infixl 6 ∪, `union`
infixr 6 ∩, `intersect`
infixl 9 !!, ∖, \\, ∆

-- THINKME: consider using numeric-prelude's non-negative numbers
-- instead of Integral n.
-}

class Ord α ⇒ Bitstream α where
    -- | /O(n)/ Explicitly convert a 'Bitstream' into a 'Stream' of
    -- 'Bool'.
    --
    -- You should be careful when you use 'stream'. Most functions in
    -- this package are optimised to minimise frequency of memory
    -- allocations, but getting 'Bitstream's back from @'Stream'
    -- 'Bool'@ requires the whole 'Bitstream' to be constructed from
    -- scratch. Moreover, for lazy 'Bitstream's this leads to be an
    -- incorrect strictness behaviour because lazy 'Bitstream's are
    -- represented as lists of strict 'Bitstream' chunks but 'stream'
    -- can't preserve the original chunk structure. Let's say you have
    -- a lazy 'Bitstream' with the following chunks:
    --
    -- @
    -- bs = [chunk1, chunk2, chunk3, ...]
    -- @
    --
    -- and you want to drop the first bit of such stream. Our 'tail'
    -- is only strict on the @chunk1@ and will produce the following
    -- chunks:
    --
    -- @
    -- 'tail' bs = [chunk0, chunk1', chunk2, chunk3, ...]
    -- @
    --
    -- where @chunk0@ is a singleton vector of the first packet of
    -- @chunk1@ whose first bit is dropped, and @chunk1'@ is a vector
    -- of remaining packets of the @chunk1@. Neither @chunk2@ nor
    -- @chunk3@ have to be evaluated here as you might expect.
    --
    -- But think about the following expression:
    --
    -- @
    -- import qualified Data.Vector.Fusion.Stream as Stream
    -- 'unstream' $ Stream.tail $ 'stream' bs
    -- @
    --
    -- the resulting chunk structure will be:
    --
    -- @
    -- [chunk1', chunk2', chunk3', ...]
    -- @
    --
    -- where each and every chunks are slightly different from the
    -- original chunks, and this time @chunk1'@ has the same length as
    -- @chunk1@ but the last bit of @chunk1'@ is from the first bit of
    -- @chunk2@. This means when you next time apply some functions
    -- strict on the first chunk, you end up fully evaluating @chunk2@
    -- as well as @chunk1@ and this can be a serious misbehaviour for
    -- lazy 'Bitstream's.
    stream ∷ α → Stream Bool

    -- | /O(n)/ Convert a 'S.Stream' of 'Bool' into a 'Bitstream'.
    unstream ∷ Stream Bool → α

    -- | /strict: O(n), lazy: O(1)/ 'cons' is an analogous to (':')
    -- for lists.
    cons ∷ Bool → α → α

    {-# INLINE cons' #-}
    cons' ∷ Bool → α → α
    cons' = cons

    -- | /O(n)/ Append a bit to the end of a 'Bitstream'.
    snoc ∷ α → Bool → α

    -- | /O(n)/ Append two 'Bitstream's.
    append ∷ α → α → α

    -- | /O(1)/ Extract the first bit of a non-empty 'Bitstream'. An
    -- exception will be thrown if empty.
    {-# INLINE head #-}
    head ∷ α → Bool
    head = S.head ∘ stream

    -- | /strict: O(1), lazy: O(n)/ Extract the last bit of a finite
    -- 'Bitstream'. An exception will be thrown if empty.
    {-# INLINE last #-}
    last ∷ α → Bool
    last = S.last ∘ stream

    -- | /O(1)/ Extract the bits after the 'head' of a non-empty
    -- 'Bitstream'. An exception will be thrown if empty.
    tail ∷ α → α

    -- | /O(n)/ Return all the bits of a 'Bitstream' except the last
    -- one. An exception will be thrown if empty.
    init ∷ α → α

    -- | /O(1)/ Test whether a 'Bitstream' is empty.
    {-# INLINE null #-}
    null ∷ α → Bool
    null = S.null ∘ stream

    -- | /O(n)/ Retern the length of a finite 'Bitstream'.
    {-# INLINE length #-}
    length ∷ Num n ⇒ α → n
    length = streamLength ∘ stream

    map ∷ (Bool → Bool) → α → α

    reverse ∷ α → α

    {-# INLINE foldl #-}
    foldl ∷ (β → Bool → β) → β → α → β
    foldl f β = S.foldl f β ∘ stream

    {-# INLINE foldl' #-}
    foldl' ∷ (β → Bool → β) → β → α → β
    foldl' f β = S.foldl' f β ∘ stream

    {-# INLINE foldl1 #-}
    foldl1 ∷ (Bool → Bool → Bool) → α → Bool
    foldl1 f = S.foldl1 f ∘ stream

    {-# INLINE foldl1' #-}
    foldl1' ∷ (Bool → Bool → Bool) → α → Bool
    foldl1' f = S.foldl1' f ∘ stream

    {-# INLINE foldr #-}
    foldr ∷ (Bool → β → β) → β → α → β
    foldr f β = S.foldr f β ∘ stream

    {-# INLINE foldr1 #-}
    foldr1 ∷ (Bool → Bool → Bool) → α → Bool
    foldr1 f = S.foldr1 f ∘ stream

    concat ∷ [α] → α

    {-# INLINE concatMap #-}
    concatMap ∷ (Bool → α) → α → α
    concatMap f = concat ∘ L.map f ∘ unpack

    {-# INLINE and #-}
    and ∷ α → Bool
    and = S.and ∘ stream

    {-# INLINE or #-}
    or ∷ α → Bool
    or = S.or ∘ stream

    {-# INLINE any #-}
    any ∷ (Bool → Bool) → α → Bool
    any f = S.or ∘ S.map f ∘ stream

    {-# INLINE all #-}
    all ∷ (Bool → Bool) → α → Bool
    all f = S.and ∘ S.map f ∘ stream

    scanl ∷ (Bool → Bool → Bool) → Bool → α → α

    scanl1 ∷ (Bool → Bool → Bool) → α → α

{-
    {-# INLINEABLE mapAccumL #-}
    mapAccumL ∷ (β → Bool → (β, Bool)) → β → α → (β, α)
    mapAccumL f s α
        = case uncons α of
            Nothing      → (s, α)
            Just (a, as) → let (s' , b ) = f s a
                               (s'', α') = mapAccumL f s' as
                           in
                             (s'', b `cons` α')

    {-# INLINEABLE mapAccumR #-}
    mapAccumR ∷ (β → Bool → (β, Bool)) → β → α → (β, α)
    mapAccumR f s α
        = case uncons α of
            Nothing      → (s, α)
            Just (a, as) → let (s'', b ) = f s' a
                               (s' , α') = mapAccumR f s as
                           in
                             (s'', b `cons` α')

    {-# INLINEABLE replicate #-}
    replicate ∷ Integral n ⇒ n → Bool → α
    replicate n b
        | n ≤ 0     = (∅)
        | otherwise = b `cons` replicate (n-1) b

    {-# INLINEABLE unfoldr #-}
    unfoldr ∷ (β → Maybe (Bool, β)) → β → α
    unfoldr f β0 = loop_unfoldr β0 (∅)
        where
          {-# INLINE loop_unfoldr #-}
          loop_unfoldr β α
              = case f β of
                  Nothing      → α
                  Just (a, β') → loop_unfoldr β' (α `snoc` a)

    {-# INLINEABLE unfoldrN #-}
    unfoldrN ∷ Integral n ⇒ n → (β → Maybe (Bool, β)) → β → (α, Maybe β)
    unfoldrN n0 f β0
        | n0 < 0    = ((∅), Just β0)
        | otherwise = loop_unfoldrN n0 β0 (∅)
        where
          loop_unfoldrN 0 β α = (α, Just β)
          loop_unfoldrN n β α
              = case f β of
                  Nothing      → (α, Nothing)
                  Just (a, β') → loop_unfoldrN (n-1) β' (α `snoc` a)

    take ∷ Integral n ⇒ n → α → α
    take = (pack ∘) ∘ (∘ unpack) ∘ L.genericTake
    {-# INLINE take #-}

    drop ∷ Integral n ⇒ n → α → α
    drop = (pack ∘) ∘ (∘ unpack) ∘ L.genericDrop
    {-# INLINE drop #-}

    splitAt ∷ Integral n ⇒ n → α → (α, α)
    splitAt n α
        = (take n α, drop n α)
    {-# INLINE splitAt #-}

    takeWhile ∷ (Bool → Bool) → α → α
    takeWhile = (pack ∘) ∘ (∘ unpack) ∘ L.takeWhile
    {-# INLINE takeWhile #-}

    dropWhile ∷ (Bool → Bool) → α → α
    dropWhile = (pack ∘) ∘ (∘ unpack) ∘ L.dropWhile
    {-# INLINE dropWhile #-}

    span ∷ (Bool → Bool) → α → (α, α)
    span f α
        = let hd = takeWhile f α
              tl = drop (length hd ∷ Integer) α
          in
            (hd, tl)
    {-# INLINEABLE span #-}

    break ∷ (Bool → Bool) → α → (α, α)
    break f = span ((¬) ∘ f)
    {-# INLINE break #-}

    group ∷ α → [α]
    group α
        = case uncons α of
            Nothing      → []
            Just (a, as) → let (β, γ) = span (a ≡) as
                           in
                             (a `cons` β) : group γ
    {-# INLINEABLE group #-}

    inits ∷ α → [α]
    inits α
        = case uncons α of
            Nothing      → α : []
            Just (a, as) → (∅) : L.map (cons a) (inits as)
    {-# INLINEABLE inits #-}

    tails ∷ α → [α]
    tails α
        = case uncons α of
            Nothing      → α : []
            Just (_, as) → α : tails as
    {-# INLINEABLE tails #-}

    isPrefixOf ∷ α → α → Bool
    isPrefixOf x y = L.isPrefixOf (unpack x) (unpack y)
    {-# INLINE isPrefixOf #-}

    isSuffixOf ∷ α → α → Bool
    isSuffixOf x y = reverse x `isPrefixOf` reverse y
    {-# INLINE isSuffixOf #-}

    isInfixOf ∷ α → α → Bool
    isInfixOf x y = L.any (x `isPrefixOf`) (tails y)
    {-# INLINE isInfixOf #-}

    elem ∷ Bool → α → Bool
    elem True  = or
    elem False = (¬) ∘ and
    {-# INLINE elem #-}

    notElem ∷ Bool → α → Bool
    notElem = ((¬) ∘) ∘ (∈)
    {-# INLINE notElem #-}

    find ∷ (Bool → Bool) → α → Maybe Bool
    find = (∘ unpack) ∘ L.find
    {-# INLINE find #-}

    filter ∷ (Bool → Bool) → α → α
    filter = (pack ∘) ∘ (∘ unpack) ∘ L.filter
    {-# INLINE filter #-}

    partition ∷ (Bool → Bool) → α → (α, α)
    partition f α = (filter f α, filter ((¬) ∘ f) α)
    {-# INLINEABLE partition #-}

    (!!) ∷ Integral n ⇒ α → n → Bool
    (!!) = L.genericIndex ∘ unpack
    {-# INLINE (!!) #-}

    elemIndex ∷ Integral n ⇒ Bool → α → Maybe n
    elemIndex = findIndex ∘ (≡)
    {-# INLINE elemIndex #-}

    elemIndices ∷ Integral n ⇒ Bool → α → [n]
    elemIndices = findIndices ∘ (≡)
    {-# INLINE elemIndices #-}

    findIndex ∷ Integral n ⇒ (Bool → Bool) → α → Maybe n
    findIndex = (listToMaybe ∘) ∘ findIndices
    {-# INLINE findIndex #-}

    {-# INLINEABLE findIndices #-}
    findIndices ∷ Integral n ⇒ (Bool → Bool) → α → [n]
    findIndices f = find' 0
        where
          find' n α
              = case uncons α of
                  Nothing         → []
                  Just (a, as)
                      | f a       → n : find' (n+1) as
                      | otherwise →     find' (n+1) as

    zip ∷ α → α → [(Bool, Bool)]
    zip = zipWith (,)
    {-# INLINE zip #-}

    zip3 ∷ α → α → α → [(Bool, Bool, Bool)]
    zip3 = zipWith3 (,,)
    {-# INLINE zip3 #-}

    zip4 ∷ α → α → α → α → [(Bool, Bool, Bool, Bool)]
    zip4 = zipWith4 (,,,)
    {-# INLINE zip4 #-}

    zip5 ∷ α → α → α → α → α → [(Bool, Bool, Bool, Bool, Bool)]
    zip5 = zipWith5 (,,,,)
    {-# INLINE zip5 #-}

    zip6 ∷ α → α → α → α → α → α → [(Bool, Bool, Bool, Bool, Bool, Bool)]
    zip6 = zipWith6 (,,,,,)
    {-# INLINE zip6 #-}

    zip7 ∷ α → α → α → α → α → α → α → [(Bool, Bool, Bool, Bool, Bool, Bool, Bool)]
    zip7 = zipWith7 (,,,,,,)
    {-# INLINE zip7 #-}

    zipWith ∷ (Bool → Bool → β) → α → α → [β]
    zipWith f α β = L.zipWith f
                      (unpack α)
                      (unpack β)
    {-# INLINE zipWith #-}

    zipWith3 ∷ (Bool → Bool → Bool → β) → α → α → α → [β]
    zipWith3 f α β γ = L.zipWith3 f
                          (unpack α)
                          (unpack β)
                          (unpack γ)
    {-# INLINE zipWith3 #-}

    zipWith4 ∷ (Bool → Bool → Bool → Bool → β) → α → α → α → α → [β]
    zipWith4 f α β γ δ = L.zipWith4 f
                             (unpack α)
                             (unpack β)
                             (unpack γ)
                             (unpack δ)
    {-# INLINE zipWith4 #-}

    zipWith5 ∷ (Bool → Bool → Bool → Bool → Bool → β) → α → α → α → α → α → [β]
    zipWith5 f α β γ δ ε = L.zipWith5 f
                                (unpack α)
                                (unpack β)
                                (unpack γ)
                                (unpack δ)
                                (unpack ε)
    {-# INLINE zipWith5 #-}

    zipWith6 ∷ (Bool → Bool → Bool → Bool → Bool → Bool → β) → α → α → α → α → α → α → [β]
    zipWith6 f α β γ δ ε ζ = L.zipWith6 f
                                   (unpack α)
                                   (unpack β)
                                   (unpack γ)
                                   (unpack δ)
                                   (unpack ε)
                                   (unpack ζ)
    {-# INLINE zipWith6 #-}

    zipWith7 ∷ (Bool → Bool → Bool → Bool → Bool → Bool → Bool → β) → α → α → α → α → α → α → α → [β]
    zipWith7 f α β γ δ ε ζ η = L.zipWith7 f
                                      (unpack α)
                                      (unpack β)
                                      (unpack γ)
                                      (unpack δ)
                                      (unpack ε)
                                      (unpack ζ)
                                      (unpack η)
    {-# INLINE zipWith7 #-}

    unzip ∷ [(Bool, Bool)] → (α, α)
    unzip = L.foldr (\(a, b) ~(as, bs) →
                         ( a `cons` as
                         , b `cons` bs )) ((∅), (∅))
    {-# INLINEABLE unzip #-}

    unzip3 ∷ [(Bool, Bool, Bool)] → (α, α, α)
    unzip3 = L.foldr (\(a, b, c) ~(as, bs, cs) →
                          ( a `cons` as
                          , b `cons` bs
                          , c `cons` cs )) ((∅), (∅), (∅))
    {-# INLINEABLE unzip3 #-}

    unzip4 ∷ [(Bool, Bool, Bool, Bool)] → (α, α, α, α)
    unzip4 = L.foldr (\(a, b, c, d) ~(as, bs, cs, ds) →
                          ( a `cons` as
                          , b `cons` bs
                          , c `cons` cs
                          , d `cons` ds )) ((∅), (∅), (∅), (∅))
    {-# INLINEABLE unzip4 #-}

    unzip5 ∷ [(Bool, Bool, Bool, Bool, Bool)] → (α, α, α, α, α)
    unzip5 = L.foldr (\(a, b, c, d, e) ~(as, bs, cs, ds, es) →
                          ( a `cons` as
                          , b `cons` bs
                          , c `cons` cs
                          , d `cons` ds
                          , e `cons` es )) ((∅), (∅), (∅), (∅), (∅))
    {-# INLINEABLE unzip5 #-}

    unzip6 ∷ [(Bool, Bool, Bool, Bool, Bool, Bool)] → (α, α, α, α, α, α)
    unzip6 = L.foldr (\(a, b, c, d, e, f) ~(as, bs, cs, ds, es, fs) →
                          ( a `cons` as
                          , b `cons` bs
                          , c `cons` cs
                          , d `cons` ds
                          , e `cons` es
                          , f `cons` fs )) ((∅), (∅), (∅), (∅), (∅), (∅))
    {-# INLINEABLE unzip6 #-}

    unzip7 ∷ [(Bool, Bool, Bool, Bool, Bool, Bool, Bool)] → (α, α, α, α, α, α, α)
    unzip7 = L.foldr (\(a, b, c, d, e, f, g) ~(as, bs, cs, ds, es, fs, gs) →
                          ( a `cons` as
                          , b `cons` bs
                          , c `cons` cs
                          , d `cons` ds
                          , e `cons` es
                          , f `cons` fs
                          , g `cons` gs )) ((∅), (∅), (∅), (∅), (∅), (∅), (∅))
    {-# INLINEABLE unzip7 #-}

    {-# INLINEABLE nub #-}
    nub ∷ α → α
    nub = flip nub' (∅)
        where
          nub' ∷ Bitstream α ⇒ α → α → α
          nub' α α'
              = case uncons α of
                  Nothing         → α
                  Just (a, as)
                      | a ∈ α'    → nub' as α'
                      | otherwise → a `cons` nub' as (a `cons` α')

    {-# INLINE delete #-}
    delete ∷ Bool → α → α
    delete = deleteBy (≡)

    {-# INLINE (\\) #-}
    (\\) ∷ α → α → α
    (\\) = foldl (flip delete)

    {-# INLINE union #-}
    union ∷ α → α → α
    union = unionBy (≡)

    {-# INLINE intersect #-}
    intersect ∷ α → α → α
    intersect = intersectBy (≡)

    {-# INLINEABLE nubBy #-}
    nubBy ∷ (Bool → Bool → Bool) → α → α
    nubBy f = flip nubBy' (∅)
        where
          nubBy' ∷ Bitstream α ⇒ α → α → α
          nubBy' α α'
              = case uncons α of
                  Nothing            → α
                  Just (a, as)
                      | elemBy' a α' → nubBy' as α'
                      | otherwise    → a `cons` nubBy' as (a `cons` α')

          elemBy' ∷ Bitstream α ⇒ Bool → α → Bool
          elemBy' b α
              = case uncons α of
                  Nothing         → False
                  Just (a, as)
                      | f b a     → True
                      | otherwise → elemBy' b as

    {-# INLINEABLE deleteBy #-}
    deleteBy ∷ (Bool → Bool → Bool) → Bool → α → α
    deleteBy f b α
        = case uncons α of
            Nothing         → α
            Just (a, as)
                | f b a     → as
                | otherwise → a `cons` deleteBy f b as

    {-# INLINEABLE deleteFirstsBy #-}
    deleteFirstsBy ∷ (Bool → Bool → Bool) → α → α → α
    deleteFirstsBy = foldl ∘ flip ∘ deleteBy

    {-# INLINEABLE unionBy #-}
    unionBy ∷ (Bool → Bool → Bool) → α → α → α
    unionBy f x y = x ⧺ foldl (flip (deleteBy f)) (nubBy f y) x

    {-# INLINEABLE intersectBy #-}
    intersectBy ∷ (Bool → Bool → Bool) → α → α → α
    intersectBy f x y = filter (\a → any (f a) y) x

    {-# INLINEABLE groupBy #-}
    groupBy ∷ (Bool → Bool → Bool) → α → [α]
    groupBy f α
        = case uncons α of
            Nothing      → []
            Just (a, α') → let (β, γ) = span (f a) α'
                           in
                             (a `cons` β) : groupBy f γ

emptyStream ∷ α
emptyStream
    = error "Data.Bitstream.Generic: empty stream"

-- | (&#x2205;) = 'empty'
--
-- U+2205, EMPTY SET
{-# INLINE (∅) #-}
(∅) ∷ Bitstream α ⇒ α
(∅) = empty

-- | (&#x29FA;) = 'append'
--
-- U+29FA, DOUBLE PLUS
(⧺) ∷ Bitstream α ⇒ α → α → α
(⧺) = append
{-# INLINE (⧺) #-}

(∈) ∷ Bitstream α ⇒ Bool → α → Bool
(∈) = elem
{-# INLINE (∈) #-}

(∋) ∷ Bitstream α ⇒ α → Bool → Bool
(∋) = flip elem
{-# INLINE (∋) #-}

(∉) ∷ Bitstream α ⇒ Bool → α → Bool
(∉) = notElem
{-# INLINE (∉) #-}

(∌) ∷ Bitstream α ⇒ α → Bool → Bool
(∌) = flip notElem
{-# INLINE (∌) #-}

(∖) ∷ Bitstream α ⇒ α → α → α
(∖) = (\\)
{-# INLINE (∖) #-}

(∪) ∷ Bitstream α ⇒ α → α → α
(∪) = union
{-# INLINE (∪) #-}

(∩) ∷ Bitstream α ⇒ α → α → α
(∩) = intersect
{-# INLINE (∩) #-}

(∆) ∷ Bitstream α ⇒ α → α → α
x ∆ y = (x ∖ y) ∪ (y ∖ x)
{-# INLINE (∆) #-}
-}

-- | /O(n)/ Convert a ['Bool'] into a 'Bitstream'.
{-# INLINE pack #-}
pack ∷ Bitstream α ⇒ [Bool] → α
pack = unstream ∘ S.fromList

-- | /O(n)/ Convert a 'Bitstream' into a ['Bool'].
{-# INLINE unpack #-}
unpack ∷ Bitstream α ⇒ α → [Bool]
unpack = S.toList ∘ stream

-- | /O(1)/ The empty 'Bitstream'.
{-# INLINE empty #-}
empty ∷ Bitstream α ⇒ α
empty = unstream S.empty

-- | /O(1)/ Convert a 'Bool' into a 'Bitstream'.
{-# INLINE singleton #-}
singleton ∷ Bitstream α ⇒ Bool → α
singleton = unstream ∘ S.singleton

{-# INLINE streamLength #-}
streamLength ∷ Num n ⇒ Stream α → n
streamLength s = S.foldl' (\n _ → n+1) 0 s

{-# INLINE scanr #-}
scanr ∷ Bitstream α ⇒ (Bool → Bool → Bool) → Bool → α → α
scanr f b = reverse ∘ scanl (flip f) b ∘ reverse

{-# INLINE scanr1 #-}
scanr1 ∷ Bitstream α ⇒ (Bool → Bool → Bool) → α → α
scanr1 f = reverse ∘ scanl1 (flip f) ∘ reverse

{-# RULES
"Bitstream stream/unstream fusion"
    ∀s. stream (unstream s) = s

"Bitstream unstream/stream fusion"
    ∀v. unstream (stream v) = v
  #-}

{-# RULES
"Bitstream cons/unstream fusion"
    ∀b s. cons b (unstream s) = unstream (S.cons b s)

"Bitstream snoc/unstream fusion"
    ∀s b. snoc (unstream s) b = unstream (S.snoc s b)

"Bitstream append/unstream fusion"
    ∀s1 s2. append (unstream s1) (unstream s2) = unstream (s1 S.++ s2)

"Bitstream head/unstream fusion"
    ∀s. head (unstream s) = S.head s

"Bitstream last/unstream fusion"
    ∀s. last (unstream s) = S.last s

"Bitstream tail/unstream fusion"
    ∀s. tail (unstream s) = unstream (S.tail s)

"Bitstream init/unstream fusion"
    ∀s. init (unstream s) = unstream (S.init s)

"Bitstream null/unstream fusion"
    ∀s. null (unstream s) = S.null s

"Bitstream length/unstream fusion"
    ∀s. length (unstream s) = streamLength s
  #-}

{-# RULES
"Bitstream map/unstream fusion"
    ∀f s. map f (unstream s) = unstream (S.map f s)
  #-}

{-# RULES
"Bitstream foldl/unstream fusion"
    ∀f β s. foldl f β (unstream s) = S.foldl f β s

"Bitstream foldl'/unstream fusion"
    ∀f β s. foldl' f β (unstream s) = S.foldl' f β s

"Bitstream fold11/unstream fusion"
    ∀f s. foldl1 f (unstream s) = S.foldl1 f s

"Bitstream foldl1'/unstream fusion"
    ∀f s. foldl1' f (unstream s) = S.foldl1' f s

"Bitstream foldr/unstream fusion"
    ∀f β s. foldr f β (unstream s) = S.foldr f β s

"Bitstream foldr1/unstream fusion"
    ∀f s. foldr1 f (unstream s) = S.foldr1 f s
  #-}

{-# RULES
"Bitstream concatMap/unstream fusion"
    ∀f s. concatMap f (unstream s) = unstream (S.concatMap f s)

"Bitstream and/unstream fusion"
    ∀s. and (unstream s) = S.and s

"Bitstream or/unstream fusion"
    ∀s. or (unstream s) = S.or s

"Bitstream any/unstream fusion"
    ∀s f. any f (unstream s) = S.or (S.map f s)

"Bitstream all/unstream fusion"
    ∀s f. all f (unstream s) = S.and (S.map f s)
  #-}

{-# RULES
"Bitstream scanl/unstream fusion"
    ∀f b s. scanl f b (unstream s) = S.scanl f b s

"Bitstream scanl1/unstream fusion"
    ∀f s. scanl1 f (unstream s) = S.scanl1 f s
  #-}
