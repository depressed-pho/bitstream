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

    , head
    , last
    , null
    , length

    , concatMap

    , foldl
    , foldl'
    , foldl1
    , foldl1'
    , foldr
    , foldr1

    , and
    , or
    , any
    , all

    , unfoldr
    , unfoldrN

    , scanr
    , scanr1

    , replicate

    , span
    , break

    , elem
    , notElem

    , find

    , (!!)
    , elemIndex
    , elemIndices
    , findIndex
    , findIndices

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

    , (∅)
    , (⧺)
    , (∈)
    , (∋)
    , (∉)
    , (∌)
    )
    where
import qualified Data.List as L
import Data.Bitstream.Fusion
import Data.Maybe
import Data.Vector.Fusion.Stream (Stream)
import qualified Data.Vector.Fusion.Stream as S
import Prelude ( Bool(..), Integer, Integral(..), Num(..), Ord(..), ($)
               , fst, flip, snd
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

-- FIXME: Explain what kind of functions are defined as methods: funcs
-- that need to preserve the packet/chunk structure
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

    -- | /O(1)/ Extract the bits after the 'head' of a non-empty
    -- 'Bitstream'. An exception will be thrown if empty.
    tail ∷ α → α

    -- | /O(n)/ Return all the bits of a 'Bitstream' except the last
    -- one. An exception will be thrown if empty.
    init ∷ α → α

    map ∷ (Bool → Bool) → α → α

    reverse ∷ α → α

    concat ∷ [α] → α

    scanl ∷ (Bool → Bool → Bool) → Bool → α → α

    scanl1 ∷ (Bool → Bool → Bool) → α → α

    take ∷ Integral n ⇒ n → α → α

    drop ∷ Integral n ⇒ n → α → α

    takeWhile ∷ (Bool → Bool) → α → α

    dropWhile ∷ (Bool → Bool) → α → α

    filter ∷ (Bool → Bool) → α → α
    {-# INLINE filter #-}
    filter f = unstream ∘ S.filter f ∘ stream

    partition ∷ (Bool → Bool) → α → (α, α)
    {-# INLINEABLE partition #-}
    partition f α = (filter f α, filter ((¬) ∘ f) α)


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
{-# INLINE (∈) #-}
(∈) = elem

(∋) ∷ Bitstream α ⇒ α → Bool → Bool
(∋) = flip elem
{-# INLINE (∋) #-}

(∉) ∷ Bitstream α ⇒ Bool → α → Bool
(∉) = notElem
{-# INLINE (∉) #-}

(∌) ∷ Bitstream α ⇒ α → Bool → Bool
(∌) = flip notElem
{-# INLINE (∌) #-}

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

-- | /O(1)/ Extract the first bit of a non-empty 'Bitstream'. An
-- exception will be thrown if empty.
head ∷ Bitstream α ⇒ α → Bool
{-# INLINE head #-}
head = S.head ∘ stream

-- | /strict: O(1), lazy: O(n)/ Extract the last bit of a finite
-- 'Bitstream'. An exception will be thrown if empty.
last ∷ Bitstream α ⇒ α → Bool
{-# INLINE last #-}
last = S.last ∘ stream

-- | /O(1)/ Test whether a 'Bitstream' is empty.
null ∷ Bitstream α ⇒ α → Bool
{-# INLINE null #-}
null = S.null ∘ stream

-- | /O(n)/ Retern the length of a finite 'Bitstream'.
length ∷ Bitstream α ⇒ Num n ⇒ α → n
{-# INLINE length #-}
length = genericLength ∘ stream

concatMap ∷ Bitstream α ⇒ (Bool → α) → α → α
{-# INLINE concatMap #-}
concatMap f = concat ∘ L.map f ∘ unpack

and ∷ Bitstream α ⇒ α → Bool
{-# INLINE and #-}
and = S.and ∘ stream

or ∷ Bitstream α ⇒ α → Bool
{-# INLINE or #-}
or = S.or ∘ stream

any ∷ Bitstream α ⇒ (Bool → Bool) → α → Bool
{-# INLINE any #-}
any f = S.or ∘ S.map f ∘ stream

all ∷ Bitstream α ⇒ (Bool → Bool) → α → Bool
{-# INLINE all #-}
all f = S.and ∘ S.map f ∘ stream

scanr ∷ Bitstream α ⇒ (Bool → Bool → Bool) → Bool → α → α
{-# INLINE scanr #-}
scanr f b = reverse ∘ scanl (flip f) b ∘ reverse

scanr1 ∷ Bitstream α ⇒ (Bool → Bool → Bool) → α → α
{-# INLINE scanr1 #-}
scanr1 f = reverse ∘ scanl1 (flip f) ∘ reverse

replicate ∷ (Bitstream α, Integral n) ⇒ n → Bool → α
{-# INLINE replicate #-}
replicate n = unstream ∘ genericReplicate n

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

(!!) ∷ (Bitstream α, Integral n) ⇒ α → n → Bool
{-# INLINE (!!) #-}
α !! n = genericIndex (stream α) n

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

find ∷ Bitstream α ⇒ (Bool → Bool) → α → Maybe Bool
{-# INLINE find #-}
find f = S.find f ∘ stream

elemIndex ∷ (Bitstream α, Integral n) ⇒ Bool → α → Maybe n
{-# INLINE elemIndex #-}
elemIndex = findIndex ∘ (≡)

elemIndices ∷ (Bitstream α, Integral n) ⇒ Bool → α → [n]
{-# INLINE elemIndices #-}
elemIndices = findIndices ∘ (≡)

findIndex ∷ (Bitstream α, Integral n) ⇒ (Bool → Bool) → α → Maybe n
{-# INLINE findIndex #-}
findIndex f = genericFindIndex f ∘ stream

findIndices ∷ (Bitstream α, Integral n) ⇒ (Bool → Bool) → α → [n]
{-# INLINE findIndices #-}
findIndices f
    = S.toList
    ∘ S.map fst
    ∘ S.filter (f ∘ snd)
    ∘ genericIndexed
    ∘ stream

zip ∷ Bitstream α ⇒ α → α → [(Bool, Bool)]
{-# INLINE zip #-}
zip = zipWith (,)

zip3 ∷ Bitstream α ⇒ α → α → α → [(Bool, Bool, Bool)]
{-# INLINE zip3 #-}
zip3 = zipWith3 (,,)

zip4 ∷ Bitstream α ⇒ α → α → α → α → [(Bool, Bool, Bool, Bool)]
{-# INLINE zip4 #-}
zip4 = zipWith4 (,,,)

zip5 ∷ Bitstream α ⇒ α → α → α → α → α → [(Bool, Bool, Bool, Bool, Bool)]
{-# INLINE zip5 #-}
zip5 = zipWith5 (,,,,)

zip6 ∷ Bitstream α ⇒ α → α → α → α → α → α → [(Bool, Bool, Bool, Bool, Bool, Bool)]
{-# INLINE zip6 #-}
zip6 = zipWith6 (,,,,,)

zipWith ∷ Bitstream α ⇒ (Bool → Bool → β) → α → α → [β]
{-# INLINE zipWith #-}
zipWith f α β = S.toList $
                S.zipWith f
                     (stream α)
                     (stream β)

zipWith3 ∷ Bitstream α ⇒ (Bool → Bool → Bool → β) → α → α → α → [β]
{-# INLINE zipWith3 #-}
zipWith3 f α β γ = S.toList $
                   S.zipWith3 f
                        (stream α)
                        (stream β)
                        (stream γ)

zipWith4 ∷ Bitstream α ⇒ (Bool → Bool → Bool → Bool → β) → α → α → α → α → [β]
{-# INLINE zipWith4 #-}
zipWith4 f α β γ δ = S.toList $
                     S.zipWith4 f
                          (stream α)
                          (stream β)
                          (stream γ)
                          (stream δ)

zipWith5 ∷ Bitstream α ⇒ (Bool → Bool → Bool → Bool → Bool → β) → α → α → α → α → α → [β]
{-# INLINE zipWith5 #-}
zipWith5 f α β γ δ ε = S.toList $
                       S.zipWith5 f
                            (stream α)
                            (stream β)
                            (stream γ)
                            (stream δ)
                            (stream ε)

zipWith6 ∷ Bitstream α ⇒ (Bool → Bool → Bool → Bool → Bool → Bool → β) → α → α → α → α → α → α → [β]
{-# INLINE zipWith6 #-}
zipWith6 f α β γ δ ε ζ = S.toList $
                         S.zipWith6 f
                              (stream α)
                              (stream β)
                              (stream γ)
                              (stream δ)
                              (stream ε)
                              (stream ζ)

unzip ∷ Bitstream α ⇒ [(Bool, Bool)] → (α, α)
{-# INLINEABLE unzip #-}
unzip xs = ( unstream $ S.map fst $ S.fromList xs
           , unstream $ S.map snd $ S.fromList xs )

unzip3 ∷ Bitstream α ⇒ [(Bool, Bool, Bool)] → (α, α, α)
{-# INLINEABLE unzip3 #-}
unzip3 xs = ( unstream $ S.map (\(α, _, _) → α) $ S.fromList xs
            , unstream $ S.map (\(_, β, _) → β) $ S.fromList xs
            , unstream $ S.map (\(_, _, γ) → γ) $ S.fromList xs )

unzip4 ∷ Bitstream α ⇒ [(Bool, Bool, Bool, Bool)] → (α, α, α, α)
{-# INLINEABLE unzip4 #-}
unzip4 xs = ( unstream $ S.map (\(α, _, _, _) → α) $ S.fromList xs
            , unstream $ S.map (\(_, β, _, _) → β) $ S.fromList xs
            , unstream $ S.map (\(_, _, γ, _) → γ) $ S.fromList xs
            , unstream $ S.map (\(_, _, _, δ) → δ) $ S.fromList xs )

unzip5 ∷ Bitstream α ⇒ [(Bool, Bool, Bool, Bool, Bool)] → (α, α, α, α, α)
{-# INLINEABLE unzip5 #-}
unzip5 xs = ( unstream $ S.map (\(α, _, _, _, _) → α) $ S.fromList xs
            , unstream $ S.map (\(_, β, _, _, _) → β) $ S.fromList xs
            , unstream $ S.map (\(_, _, γ, _, _) → γ) $ S.fromList xs
            , unstream $ S.map (\(_, _, _, δ, _) → δ) $ S.fromList xs
            , unstream $ S.map (\(_, _, _, _, ε) → ε) $ S.fromList xs )

unzip6 ∷ Bitstream α ⇒ [(Bool, Bool, Bool, Bool, Bool, Bool)] → (α, α, α, α, α, α)
{-# INLINEABLE unzip6 #-}
unzip6 xs = ( unstream $ S.map (\(α, _, _, _, _, _) → α) $ S.fromList xs
            , unstream $ S.map (\(_, β, _, _, _, _) → β) $ S.fromList xs
            , unstream $ S.map (\(_, _, γ, _, _, _) → γ) $ S.fromList xs
            , unstream $ S.map (\(_, _, _, δ, _, _) → δ) $ S.fromList xs
            , unstream $ S.map (\(_, _, _, _, ε, _) → ε) $ S.fromList xs
            , unstream $ S.map (\(_, _, _, _, _, ζ) → ζ) $ S.fromList xs )

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

"Bitstream init/unstream fusion"
    ∀s. init (unstream s) = unstream (S.init s)

"Bitstream null/unstream fusion"
    ∀s. null (unstream s) = S.null s

"Bitstream map/unstream fusion"
    ∀f s. map f (unstream s) = unstream (S.map f s)

"Bitstream scanl/unstream fusion"
    ∀f b s. scanl f b (unstream s) = unstream (S.scanl f b s)

"Bitstream scanl1/unstream fusion"
    ∀f s. scanl1 f (unstream s) = unstream (S.scanl1 f s)

"Bitstream take/unstream fusion"
    ∀n s. take n (unstream s) = unstream (genericTake n s)

"Bitstream drop/unstream fusion"
    ∀n s. drop n (unstream s) = unstream (genericDrop n s)

"Bitstream takeWhile/unstream fusion"
    ∀f s. takeWhile f (unstream s) = unstream (S.takeWhile f s)

"Bitstream dropWhile/unstream fusion"
    ∀f s. dropWhile f (unstream s) = unstream (S.dropWhile f s)

"Bitstream filter/unstream fusion"
    ∀f s. filter f (unstream s) = unstream (S.filter f s)
  #-}
