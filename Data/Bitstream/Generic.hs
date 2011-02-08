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

    , foldl
    , foldl'
    , foldl1
    , foldl1'
    , foldr
    , foldr1

    , unfoldr
    , unfoldrN

    , span
    , break

    , elem
    , notElem
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
import qualified Data.List as L
import Data.Bitstream.Fusion
import Data.Maybe
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

    cons' ∷ Bool → α → α
    {-# INLINE cons' #-}
    cons' = cons

    -- | /O(n)/ Append a bit to the end of a 'Bitstream'.
    snoc ∷ α → Bool → α

    -- | /O(n)/ Append two 'Bitstream's.
    append ∷ α → α → α

    -- | /O(1)/ Extract the first bit of a non-empty 'Bitstream'. An
    -- exception will be thrown if empty.
    head ∷ α → Bool
    {-# INLINE head #-}
    head = S.head ∘ stream

    -- | /strict: O(1), lazy: O(n)/ Extract the last bit of a finite
    -- 'Bitstream'. An exception will be thrown if empty.
    last ∷ α → Bool
    {-# INLINE last #-}
    last = S.last ∘ stream

    -- | /O(1)/ Extract the bits after the 'head' of a non-empty
    -- 'Bitstream'. An exception will be thrown if empty.
    tail ∷ α → α

    -- | /O(n)/ Return all the bits of a 'Bitstream' except the last
    -- one. An exception will be thrown if empty.
    init ∷ α → α

    -- | /O(1)/ Test whether a 'Bitstream' is empty.
    null ∷ α → Bool
    {-# INLINE null #-}
    null = S.null ∘ stream

    -- | /O(n)/ Retern the length of a finite 'Bitstream'.
    length ∷ Num n ⇒ α → n
    {-# INLINE length #-}
    length = genericLength ∘ stream

    map ∷ (Bool → Bool) → α → α

    reverse ∷ α → α

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

    scanr ∷ (Bool → Bool → Bool) → Bool → α → α
    {-# INLINE scanr #-}
    scanr f b = reverse ∘ scanl (flip f) b ∘ reverse

    scanr1 ∷ (Bool → Bool → Bool) → α → α
    {-# INLINE scanr1 #-}
    scanr1 f = reverse ∘ scanl1 (flip f) ∘ reverse

    {-# INLINE replicate #-}
    replicate ∷ Integral n ⇒ n → Bool → α
    replicate n = unstream ∘ genericReplicate n

    take ∷ Integral n ⇒ n → α → α

    drop ∷ Integral n ⇒ n → α → α

    takeWhile ∷ (Bool → Bool) → α → α

    dropWhile ∷ (Bool → Bool) → α → α

{-
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
-}

(∈) ∷ Bitstream α ⇒ Bool → α → Bool
{-# INLINE (∈) #-}
(∈) = elem

{-
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

foldl ∷ Bitstream α ⇒ (β → Bool → β) → β → α → β
{-# INLINE foldl #-}
foldl f β = S.foldl f β ∘ stream

foldl' ∷ Bitstream α ⇒ (β → Bool → β) → β → α → β
{-# INLINE foldl' #-}
foldl' f β = S.foldl' f β ∘ stream

foldl1 ∷ Bitstream α ⇒ (Bool → Bool → Bool) → α → Bool
{-# INLINE foldl1 #-}
foldl1 f = S.foldl1 f ∘ stream

foldl1' ∷ Bitstream α ⇒ (Bool → Bool → Bool) → α → Bool
{-# INLINE foldl1' #-}
foldl1' f = S.foldl1' f ∘ stream

foldr ∷ Bitstream α ⇒ (Bool → β → β) → β → α → β
{-# INLINE foldr #-}
foldr f β = S.foldr f β ∘ stream

foldr1 ∷ Bitstream α ⇒ (Bool → Bool → Bool) → α → Bool
{-# INLINE foldr1 #-}
foldr1 f = S.foldr1 f ∘ stream

unfoldr ∷ Bitstream α ⇒ (β → Maybe (Bool, β)) → β → α
{-# INLINE unfoldr #-}
unfoldr f = unstream ∘ S.unfoldr f

unfoldrN ∷ (Bitstream α, Integral n) ⇒ n → (β → Maybe (Bool, β)) → β → α
{-# INLINE unfoldrN #-}
unfoldrN n f = unstream ∘ genericUnfoldrN n f

span ∷ Bitstream α ⇒ (Bool → Bool) → α → (α, α)
{-# INLINEABLE span #-}
span f α
    = let hd = takeWhile f α
          tl = drop (length hd ∷ Integer) α
      in
        (hd, tl)

break ∷ Bitstream α ⇒ (Bool → Bool) → α → (α, α)
{-# INLINE break #-}
break f = span ((¬) ∘ f)

elem ∷ Bitstream α ⇒ Bool → α → Bool
{-# INLINE elem #-}
elem True  = or
elem False = (¬) ∘ and

notElem ∷ Bitstream α ⇒ Bool → α → Bool
{-# INLINE notElem #-}
notElem = ((¬) ∘) ∘ (∈)

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
    ∀s. length (unstream s) = genericLength s
  #-}

{-# RULES
"Bitstream map/unstream fusion"
    ∀f s. map f (unstream s) = unstream (S.map f s)
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
    ∀f b s. scanl f b (unstream s) = unstream (S.scanl f b s)

"Bitstream scanl1/unstream fusion"
    ∀f s. scanl1 f (unstream s) = unstream (S.scanl1 f s)
  #-}

{-# RULES
"Bitstream take/unstream fusion"
    ∀n s. take n (unstream s) = unstream (genericTake n s)

"Bitstream drop/unstream fusion"
    ∀n s. drop n (unstream s) = unstream (genericDrop n s)

"Bitstream takeWhile/unstream fusion"
    ∀f s. takeWhile f (unstream s) = unstream (S.takeWhile f s)

"Bitstream dropWhile/unstream fusion"
    ∀f s. dropWhile f (unstream s) = unstream (S.dropWhile f s)
  #-}
