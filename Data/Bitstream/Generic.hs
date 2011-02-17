{-# LANGUAGE
    BangPatterns
  , RankNTypes
  , UnicodeSyntax
  #-}
-- | Generic interface to diverse types of 'Bitstream'.
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

    , scanl1
    , scanr
    , scanr1

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
import Prelude ( Bool(..), Integer, Integral(..), Num(..), ($)
               , fst, flip, otherwise, snd
               )
import Prelude.Unicode hiding ((∈), (∉), (⧺))

infix  4 ∈, ∋, ∉, ∌, `elem`, `notElem`
infixr 5 ⧺, `append`
infixl 9 !!

-- THINKME: consider using numeric-prelude's non-negative numbers
-- instead of Integral n.

{- Notes about inlining / rewriting phase control:

   1. We want "*/unstream fusion" rules always fire.
   2. Unfused form specialisations should occur at phase 2 and later.
   3. Fusible form inlinings should occur at phase 1 and later.
   4. stream / unstream inlinings should occur last i.e. phase 0.
 -}

-- | Class of diverse types of 'Bitstream'.
--
-- Methods of this class are functions of 'Bitstream's that is either
-- basic functions to implement other ones, or have to preserve their
-- packet/chunk structure for efficiency and strictness behaviour.
--
-- Minimum complete implementation: /All but/ 'cons'', 'concat',
-- 'replicate' and 'partition'.
class Bitstream α where
    -- | /O(n)/ Explicitly convert a 'Bitstream' into a 'Stream' of
    -- 'Bool'.
    --
    -- 'Bitstream' operations are automatically fused whenever it's
    -- possible, safe, and effective to do so, but sometimes you may
    -- find the rules are too conservative. These two functions
    -- 'stream' and 'unstream' provide a means for coercive stream
    -- fusion.
    --
    -- You should be careful when you use 'stream'. Most functions in
    -- this package are optimised to minimise frequency of memory
    -- allocations and copyings, but getting 'Bitstream's back from
    -- @'Stream' 'Bool'@ requires the whole 'Bitstream' to be
    -- constructed from scratch. Moreover, for lazy 'Bitstream's this
    -- leads to be an incorrect strictness behaviour because lazy
    -- 'Bitstream's are represented as lists of strict 'Bitstream'
    -- chunks but 'stream' can't preserve the original chunk
    -- structure. Let's say you have a lazy 'Bitstream' with the
    -- following chunks:
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
    --
    -- The automatic fusion rules are carefully designed to fire only
    -- when there aren't any reason to preserve the original packet /
    -- chunk structure.
    stream ∷ α → Stream Bool

    -- | /O(n)/ Convert a 'S.Stream' of 'Bool' into a 'Bitstream'.
    unstream ∷ Stream Bool → α

    -- | /strict: O(n), lazy: O(1)/ 'cons' is an analogous to (':')
    -- for lists.
    cons ∷ Bool → α → α

    -- | /O(n)/ For strict 'Bitstream's, 'cons'' is exactly the same
    -- as 'cons'.
    --
    -- For lazy ones, 'cons'' is strict in the 'Bitstream' we are
    -- consing onto. More precisely, it forces the first chunk to be
    -- evaluated. It does this because, for space efficiency, it may
    -- coalesce the new bit onto the first chunk rather than starting
    -- a new chunk.
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

    -- | /O(n)/ Map a function over a 'Bitstream'.
    map ∷ (Bool → Bool) → α → α

    -- | /O(n)/ Reverse a 'Bitstream'.
    reverse ∷ α → α

    -- | /O(n)/ Concatenate all 'Bitstream's in the list.
    concat ∷ [α] → α
    {-# INLINE concat #-}
    concat []     = (∅)
    concat (α:αs) = α ⧺ concat αs

    -- | /O(n)/ 'scanl' is similar to 'foldl', but returns a
    -- 'Bitstream' of successive reduced bits from the left:
    --
    -- @
    -- 'scanl' f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
    -- @
    --
    -- Note that
    --
    -- @
    -- 'last' ('scanl' f z xs) == 'foldl' f z xs
    -- @
    scanl ∷ (Bool → Bool → Bool) → Bool → α → α

    -- | /O(n)/ @'replicate' n x@ is a 'Bitstream' of length @n@ with
    -- @x@ the value of every bit.
    replicate ∷ Integral n ⇒ n → Bool → α
    {-# INLINE replicate #-}
    replicate n = unstream ∘ genericReplicate n

    -- | /O(n)/ 'take' @n@, applied to a 'Bitstream' @xs@, returns the
    -- prefix of @xs@ of length @n@, or @xs@ itself if @n > 'length'
    -- xs@.
    take ∷ Integral n ⇒ n → α → α

    -- | /O(n)/ 'drop' @n xs@ returns the suffix of @xs@ after the
    -- first @n@ bits, or 'empty' if @n > 'length' xs@.
    drop ∷ Integral n ⇒ n → α → α

    -- | /O(n)/ 'takeWhile', applied to a predicate @p@ and a
    -- 'Bitstream' @xs@, returns the longest prefix (possibly 'empty')
    -- of @xs@ of bits that satisfy @p@.
    takeWhile ∷ (Bool → Bool) → α → α

    -- | /O(n)/ 'dropWhile' @p xs@ returns the suffix remaining after
    -- 'takeWhile' @p xs@.
    dropWhile ∷ (Bool → Bool) → α → α

    -- | /O(n)/ 'filter', applied to a predicate and a 'Bitstream',
    -- returns the 'Bitstream' of those bits that satisfy the
    -- predicate.
    filter ∷ (Bool → Bool) → α → α

    -- | /O(n)/ The 'partition' function takes a predicate and a
    -- 'Bitstream' and returns the pair of 'Bitstream's of bits which
    -- do and do not satisfy the predicate, respectively.
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

-- | (&#x2208;) = 'elem'
--
-- U+2208, ELEMENT OF
(∈) ∷ Bitstream α ⇒ Bool → α → Bool
{-# INLINE (∈) #-}
(∈) = elem

-- | (&#x220B;) = 'flip' (&#x2208;)
--
-- U+220B, CONTAINS AS MEMBER
(∋) ∷ Bitstream α ⇒ α → Bool → Bool
(∋) = flip elem
{-# INLINE (∋) #-}

-- | (&#x2209;) = 'notElem'
--
-- U+2209, NOT AN ELEMENT OF
(∉) ∷ Bitstream α ⇒ Bool → α → Bool
(∉) = notElem
{-# INLINE (∉) #-}

-- | (&#x220C;) = 'flip' (&#x2209;)
--
-- U+220C, DOES NOT CONTAIN AS MEMBER
(∌) ∷ Bitstream α ⇒ α → Bool → Bool
(∌) = flip notElem
{-# INLINE (∌) #-}

-- | /O(n)/ Convert a ['Bool'] into a 'Bitstream'.
{-# INLINE [1] pack #-}
pack ∷ Bitstream α ⇒ [Bool] → α
pack = unstream ∘ S.fromList

-- | /O(n)/ Convert a 'Bitstream' into a ['Bool'].
unpack ∷ Bitstream α ⇒ α → [Bool]
{-# RULES "Bitstream unpack/unstream fusion"
    ∀s. unpack (unstream s) = S.toList s
  #-}
{-# INLINE [1] unpack #-}
unpack = S.toList ∘ stream

-- | /O(1)/ The empty 'Bitstream'.
empty ∷ Bitstream α ⇒ α
{-# INLINE [1] empty #-}
empty = unstream S.empty

-- | /O(1)/ Convert a 'Bool' into a 'Bitstream'.
singleton ∷ Bitstream α ⇒ Bool → α
{-# INLINE [1] singleton #-}
singleton = unstream ∘ S.singleton

-- | /O(1)/ Extract the first bit of a non-empty 'Bitstream'. An
-- exception will be thrown if empty.
head ∷ Bitstream α ⇒ α → Bool
{-# RULES "Bitstream head/unstream fusion"
    ∀s. head (unstream s) = S.head s
  #-}
{-# INLINE [1] head #-}
head = S.head ∘ stream

-- | /strict: O(1), lazy: O(n)/ Extract the last bit of a finite
-- 'Bitstream'. An exception will be thrown if empty.
last ∷ Bitstream α ⇒ α → Bool
{-# RULES "Bitstream last/unstream fusion"
    ∀s. last (unstream s) = S.last s
  #-}
{-# INLINE [1] last #-}
last = S.last ∘ stream

-- | /O(1)/ Test whether a 'Bitstream' is empty.
null ∷ Bitstream α ⇒ α → Bool
{-# RULES "Bitstream null/unstream fusion"
    ∀s. null (unstream s) = S.null s
  #-}
{-# INLINE [1] null #-}
null = S.null ∘ stream

-- | /O(n)/ Retern the length of a finite 'Bitstream'.
length ∷ Bitstream α ⇒ Num n ⇒ α → n
{-# RULES "Bitstream length/unstream fusion"
    ∀s. length (unstream s) = genericLength s
  #-}
{-# INLINE [1] length #-}
length = genericLength ∘ stream

-- | Map a function over a 'Bitstream' and concatenate the results.
concatMap ∷ Bitstream α ⇒ (Bool → α) → α → α
{-# RULES "Bitstream concatMap/unstream fusion"
    ∀f s. concatMap f (unstream s) = unstream (S.concatMap f s)
  #-}
{-# INLINE [1] concatMap #-}
concatMap f = concat ∘ L.map f ∘ unpack

-- | /O(n)/ 'and' returns the conjunction of a 'Bool' list. For the
-- result to be 'True', the 'Bitstream' must be finite; 'False',
-- however, results from a 'False' value at a finite index of a finite
-- or infinite 'Bitstream'. Note that strict 'Bitstream's are always
-- finite.
and ∷ Bitstream α ⇒ α → Bool
{-# RULES "Bitstream and/unstream fusion"
    ∀s. and (unstream s) = S.and s
  #-}
{-# INLINE [1] and #-}
and = S.and ∘ stream

-- | /O(n)/ 'or' returns the disjunction of a 'Bool' list. For the
-- result to be 'False', the 'Bitstream' must be finite; 'True',
-- however, results from a 'True' value at a finite index of a finite
-- or infinite 'Bitstream'. Note that strict 'Bitstream's are always
-- finite.
or ∷ Bitstream α ⇒ α → Bool
{-# RULES "Bitstream or/unstream fusion"
    ∀s. or (unstream s) = S.or s
  #-}
{-# INLINE [1] or #-}
or = S.or ∘ stream

-- | /O(n)/ Applied to a predicate and a 'Bitstream', 'any' determines
-- if any bit of the 'Bitstream' satisfies the predicate. For the
-- result to be 'False', the 'Bitstream' must be finite; 'True',
-- however, results from a 'True' value for the predicate applied to a
-- bit at a finite index of a finite or infinite 'Bitstream'.
any ∷ Bitstream α ⇒ (Bool → Bool) → α → Bool
{-# RULES "Bitstream any/unstream fusion"
    ∀f s. any f (unstream s) = S.or (S.map f s)
  #-}
{-# INLINE [1] any #-}
any f = S.or ∘ S.map f ∘ stream

-- | /O(n)/ Applied to a predicate and a 'Bitstream', 'all' determines
-- if all bits of the 'Bitstream' satisfy the predicate. For the
-- result to be 'True', the 'Bitstream' must be finite; 'False',
-- however, results from a 'False' value for the predicate applied to
-- a bit at a finite index of a finite or infinite 'Bitstream'.
all ∷ Bitstream α ⇒ (Bool → Bool) → α → Bool
{-# RULES "Bitstream all/unstream fusion"
    ∀f s. all f (unstream s) = S.and (S.map f s)
  #-}
{-# INLINE [1] all #-}
all f = S.and ∘ S.map f ∘ stream

-- | /O(n)/ 'scanl1' is a variant of 'scanl' that has no starting
-- value argument:
--
-- @
-- 'scanl1' f [x1, x2, ...] == [x1, x1 `f` x2, ...]
-- @
scanl1 ∷ Bitstream α ⇒ (Bool → Bool → Bool) → α → α
{-# RULES "Bitstream scanl1/unstream fusion"
    ∀f s. scanl1 f (unstream s) = S.scanl1 f s
  #-}
{-# INLINE [1] scanl1 #-}
scanl1 f α
    | null α    = α
    | otherwise = scanl f (head α) (tail α)

-- | /O(n)/ 'scanr' is the right-to-left dual of 'scanl'.  Note that
--
-- @
-- 'head' ('scanr' f z xs) == 'foldr' f z xs
-- @
scanr ∷ Bitstream α ⇒ (Bool → Bool → Bool) → Bool → α → α
{-# INLINE [1] scanr #-}
scanr f b = reverse ∘ scanl (flip f) b ∘ reverse

-- | /O(n)/ 'scanr1' is a variant of 'scanr' that has no starting
-- value argument.
scanr1 ∷ Bitstream α ⇒ (Bool → Bool → Bool) → α → α
{-# INLINE [1] scanr1 #-}
scanr1 f = reverse ∘ scanl1 (flip f) ∘ reverse

-- | /O(n)/ 'foldl', applied to a binary operator, a starting value
-- (typically the left-identity of the operator), and a 'Bitstream',
-- reduces the 'Bitstream' using the binary operator, from left to
-- right:
--
-- @
-- 'foldl' f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
-- @
--
-- The 'Bitstream' must be finite.
foldl ∷ Bitstream α ⇒ (β → Bool → β) → β → α → β
{-# RULES "Bitstream foldl/unstream fusion"
    ∀f β s. foldl f β (unstream s) = S.foldl f β s
  #-}
{-# INLINE [1] foldl #-}
foldl f β = S.foldl f β ∘ stream

-- | /O(n)/ 'foldl'' is a variant of 'foldl' that is strict on the
-- accumulator.
foldl' ∷ Bitstream α ⇒ (β → Bool → β) → β → α → β
{-# RULES "Bitstream foldl'/unstream fusion"
    ∀f β s. foldl' f β (unstream s) = S.foldl' f β s
  #-}
{-# INLINE [1] foldl' #-}
foldl' f β = S.foldl' f β ∘ stream

-- | /O(n)/ 'foldl1' is a variant of 'foldl' that has no starting
-- value argument, and thus must be applied to non-empty 'Bitstream's.
foldl1 ∷ Bitstream α ⇒ (Bool → Bool → Bool) → α → Bool
{-# RULES "Bitstream foldl1/unstream fusion"
    ∀f s. foldl1 f (unstream s) = S.foldl1 f s
  #-}
{-# INLINE [1] foldl1 #-}
foldl1 f = S.foldl1 f ∘ stream

-- | /O(n)/ A strict version of 'foldl1'.
foldl1' ∷ Bitstream α ⇒ (Bool → Bool → Bool) → α → Bool
{-# RULES "Bitstream foldl1'/unstream fusion"
    ∀f s. foldl1' f (unstream s) = S.foldl1' f s
  #-}
{-# INLINE [1] foldl1' #-}
foldl1' f = S.foldl1' f ∘ stream

-- | /O(n)/ 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a 'Bitstream',
-- reduces the 'Bitstream' using the binary operator, from right to
-- left:
--
-- @
-- 'foldr' f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
-- @
foldr ∷ Bitstream α ⇒ (Bool → β → β) → β → α → β
{-# RULES "Bitstream foldr/unstream fusion"
    ∀f β s. foldr f β (unstream s) = S.foldr f β s
  #-}
{-# INLINE [1] foldr #-}
foldr f β = S.foldr f β ∘ stream

-- | /O(n)/ 'foldr1' is a variant of 'foldr' that has no starting
-- value argument, and thus must be applied to non-empty 'Bitstream's.
foldr1 ∷ Bitstream α ⇒ (Bool → Bool → Bool) → α → Bool
{-# RULES "Bitstream foldr1/unstream fusion"
    ∀f s. foldr1 f (unstream s) = S.foldr1 f s
  #-}
{-# INLINE [1] foldr1 #-}
foldr1 f = S.foldr1 f ∘ stream

-- | /O(n)/ The 'unfoldr' function is a \`dual\' to 'foldr': while
-- 'foldr' reduces a 'Bitstream' to a summary value, 'unfoldr' builds
-- a 'Bitstream' from a seed value. The function takes the element and
-- returns 'Nothing' if it is done producing the 'Bitstream' or
-- returns 'Just' @(a, b)@, in which case, @a@ is a prepended to the
-- 'Bitstream' and @b@ is used as the next element in a recursive
-- call.
unfoldr ∷ Bitstream α ⇒ (β → Maybe (Bool, β)) → β → α
{-# INLINE [1] unfoldr #-}
unfoldr f = unstream ∘ S.unfoldr f

-- | /O(n)/ 'unfoldrN' is a variant of 'unfoldr' but constructs a
-- 'Bitstream' with at most @n@ bits.
unfoldrN ∷ (Bitstream α, Integral n) ⇒ n → (β → Maybe (Bool, β)) → β → α
{-# INLINE [1] unfoldrN #-}
unfoldrN n f = unstream ∘ genericUnfoldrN n f

-- | /O(n)/ 'Bitstream' index (subscript) operator, starting from 0.
(!!) ∷ (Bitstream α, Integral n) ⇒ α → n → Bool
{-# RULES "Bitstream (!!)/unstream fusion"
    ∀s n. (unstream s) !! n = genericIndex s n
  #-}
{-# INLINE [1] (!!) #-}
α !! n = genericIndex (stream α) n

-- | /O(n)/ 'span', applied to a predicate @p@ and a 'Bitstream' @xs@,
-- returns a tuple where first element is longest prefix (possibly
-- 'empty') of @xs@ of bits that satisfy @p@ and second element is the
-- remainder of the 'Bitstream'.
-- 
-- 'span' @p xs@ is equivalent to @('takeWhile' p xs, 'dropWhile' p
-- xs)@
span ∷ Bitstream α ⇒ (Bool → Bool) → α → (α, α)
{-# INLINE [1] span #-}
span f α
    = let hd = takeWhile f α
          tl = drop (length hd ∷ Integer) α
      in
        (hd, tl)

-- | /O(n)/ 'break', applied to a predicate @p@ and a 'Bitstream'
-- @xs@, returns a tuple where first element is longest prefix
-- (possibly 'empty') of @xs@ of bits that /do not satisfy/ @p@ and
-- second element is the remainder of the 'Bitstream'.
--
-- 'break' @p@ is equivalent to @'span' ('not' . p)@.
break ∷ Bitstream α ⇒ (Bool → Bool) → α → (α, α)
{-# INLINE [1] break #-}
break f = span ((¬) ∘ f)

-- | /O(n)/ 'elem' is the 'Bitstream' membership predicate, usually
-- written in infix form, e.g., @x \`elem\` xs@.  For the result to be
-- 'False', the 'Bitstream' must be finite; 'True', however, results
-- from an bit equal to @x@ found at a finite index of a finite or
-- infinite 'Bitstream'.
elem ∷ Bitstream α ⇒ Bool → α → Bool
{-# RULES "Bitstream elem/unstream fusion"
    ∀b s. elem b (unstream s) = S.elem b s
  #-}
{-# INLINE [1] elem #-}
elem True  = or
elem False = (¬) ∘ and

-- | /O(n)/ 'notElem' is the negation of 'elem'.
notElem ∷ Bitstream α ⇒ Bool → α → Bool
{-# RULES "Bitstream notElem/unstream fusion"
    ∀b s. notElem b (unstream s) = S.notElem b s
  #-}
{-# INLINE [1] notElem #-}
notElem = ((¬) ∘) ∘ (∈)

-- | /O(n)/ The 'find' function takes a predicate and a 'Bitstream'
-- and returns the bit in the 'Bitstream' matching the predicate, or
-- 'Nothing' if there is no such bit.
find ∷ Bitstream α ⇒ (Bool → Bool) → α → Maybe Bool
{-# RULES "Bitstream find/unstream fusion"
    ∀f s. find f (unstream s) = S.find f s
  #-}
{-# INLINE [1] find #-}
find f = S.find f ∘ stream

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- bit in the given 'Bitstream' which is equal to the query bit, or
-- 'Nothing' if there is no such bit.
elemIndex ∷ (Bitstream α, Integral n) ⇒ Bool → α → Maybe n
{-# RULES "Bitstream elemIndex/unstream fusion"
    ∀b s. elemIndex b (unstream s) = genericFindIndex (≡ b) s
  #-}
{-# INLINE [1] elemIndex #-}
elemIndex = findIndex ∘ (≡)

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by
-- returning the indices of all bits equal to the query bit, in
-- ascending order.
elemIndices ∷ (Bitstream α, Integral n) ⇒ Bool → α → [n]
{-# RULES "Bitstream elemIndices/unstream fusion"
    ∀b s. elemIndices b (unstream s)
              = S.toList
              $ S.map fst
              $ S.filter ((≡ b) ∘ snd)
              $ genericIndexed s
  #-}
{-# INLINE [1] elemIndices #-}
elemIndices = findIndices ∘ (≡)

-- | /O(n)/ The 'findIndex' function takes a predicate and a
-- 'Bitstream' and returns the index of the first bit in the
-- 'Bitstream' satisfying the predicate, or 'Nothing' if there is no
-- such bit.
findIndex ∷ (Bitstream α, Integral n) ⇒ (Bool → Bool) → α → Maybe n
{-# RULES "Bitstream findIndex/unstream fusion"
    ∀f s. findIndex f (unstream s) = genericFindIndex f s
  #-}
{-# INLINE [1] findIndex #-}
findIndex f = genericFindIndex f ∘ stream

-- | /O(n)/ The 'findIndices' function extends 'findIndex', by
-- returning the indices of all bits satisfying the predicate, in
-- ascending order.
findIndices ∷ (Bitstream α, Integral n) ⇒ (Bool → Bool) → α → [n]
{-# RULES "Bitstream findIndices/unstream fusion"
    ∀f s. findIndices f (unstream s)
              = S.toList
              $ S.map fst
              $ S.filter (f ∘ snd)
              $ genericIndexed s
  #-}
{-# INLINE [1] findIndices #-}
findIndices f
    = S.toList
    ∘ S.map fst
    ∘ S.filter (f ∘ snd)
    ∘ genericIndexed
    ∘ stream

-- | /O(min(m, n))/ 'zip' takes two 'Bitstream's and returns a list of
-- corresponding bit pairs. If one input 'Bitstream' is short, excess
-- bits of the longer 'Bitstream' are discarded.
zip ∷ Bitstream α ⇒ α → α → [(Bool, Bool)]
{-# RULES "Bitstream zip/unstream fusion" ∀s1 s2.
    zip (unstream s1) (unstream s2)
        = S.toList (S.zip s1 s2)
  #-}
{-# INLINE [1] zip #-}
zip = zipWith (,)

-- | The 'zip3' function takes three 'Bitstream's and returns a list
-- of triples, analogous to 'zip'.
zip3 ∷ Bitstream α ⇒ α → α → α → [(Bool, Bool, Bool)]
{-# RULES "Bitstream zip3/unstream fusion" ∀s1 s2 s3.
    zip3 (unstream s1) (unstream s2) (unstream s3)
        = S.toList (S.zip3 s1 s2 s3)
  #-}
{-# INLINE [1] zip3 #-}
zip3 = zipWith3 (,,)

-- | The 'zip4' function takes four lists and returns a list of
-- quadruples, analogous to 'zip'.
zip4 ∷ Bitstream α ⇒ α → α → α → α → [(Bool, Bool, Bool, Bool)]
{-# RULES "Bitstream zip4/unstream fusion" ∀s1 s2 s3 s4.
    zip4 (unstream s1) (unstream s2) (unstream s3) (unstream s4)
        = S.toList (S.zip4 s1 s2 s3 s4)
  #-}
{-# INLINE [1] zip4 #-}
zip4 = zipWith4 (,,,)

-- | The 'zip5' function takes five 'Bitstream's and returns a list of
-- five-tuples, analogous to 'zip'.
zip5 ∷ Bitstream α ⇒ α → α → α → α → α → [(Bool, Bool, Bool, Bool, Bool)]
{-# RULES "Bitstream zip5/unstream fusion" ∀s1 s2 s3 s4 s5.
    zip5 (unstream s1) (unstream s2) (unstream s3) (unstream s4) (unstream s5)
        = S.toList (S.zip5 s1 s2 s3 s4 s5)
  #-}
{-# INLINE [1] zip5 #-}
zip5 = zipWith5 (,,,,)

-- | The 'zip6' function takes six 'Bitstream's and returns a list of
-- six-tuples, analogous to 'zip'.
zip6 ∷ Bitstream α ⇒ α → α → α → α → α → α → [(Bool, Bool, Bool, Bool, Bool, Bool)]
{-# RULES "Bitstream zip6/unstream fusion" ∀s1 s2 s3 s4 s5 s6.
    zip6 (unstream s1) (unstream s2) (unstream s3) (unstream s4) (unstream s5) (unstream s6)
        = S.toList (S.zip6 s1 s2 s3 s4 s5 s6)
  #-}
{-# INLINE [1] zip6 #-}
zip6 = zipWith6 (,,,,,)

-- | /O(min(m, n))/ 'zipWith' generalises 'zip' by zipping with the
-- function given as the first argument, instead of a tupling
-- function.
zipWith ∷ Bitstream α ⇒ (Bool → Bool → β) → α → α → [β]
{-# RULES "Bitstream zipWith/unstream fusion" ∀f s1 s2.
    zipWith f (unstream s1) (unstream s2)
        = S.toList (S.zipWith f s1 s2)
  #-}
{-# INLINEABLE [1] zipWith #-}
zipWith f α β = S.toList $
                S.zipWith f
                     (stream α)
                     (stream β)

-- | The 'zipWith3' function takes a function which combines three
-- bits, as well as three 'Bitstream's and returns a list of their
-- point-wise combination, analogous to 'zipWith'.
zipWith3 ∷ Bitstream α ⇒ (Bool → Bool → Bool → β) → α → α → α → [β]
{-# RULES "Bitstream zipWith3/unstream fusion" ∀f s1 s2 s3.
    zipWith3 f (unstream s1) (unstream s2) (unstream s3)
        = S.toList (S.zipWith3 f s1 s2 s3)
  #-}
{-# INLINEABLE [1] zipWith3 #-}
zipWith3 f α β γ = S.toList $
                   S.zipWith3 f
                        (stream α)
                        (stream β)
                        (stream γ)

-- | The 'zipWith4' function takes a function which combines four
-- bits, as well as four 'Bitstream's and returns a list of their
-- point-wise combination, analogous to 'zipWith'.
zipWith4 ∷ Bitstream α ⇒ (Bool → Bool → Bool → Bool → β) → α → α → α → α → [β]
{-# RULES "Bitstream zipWith4/unstream fusion" ∀f s1 s2 s3 s4.
    zipWith4 f (unstream s1) (unstream s2) (unstream s3) (unstream s4)
        = S.toList (S.zipWith4 f s1 s2 s3 s4)
  #-}
{-# INLINEABLE [1] zipWith4 #-}
zipWith4 f α β γ δ = S.toList $
                     S.zipWith4 f
                          (stream α)
                          (stream β)
                          (stream γ)
                          (stream δ)

-- | The 'zipWith5' function takes a function which combines five
-- bits, as well as five 'Bitstream's and returns a list of their
-- point-wise combination, analogous to 'zipWith'.
zipWith5 ∷ Bitstream α ⇒ (Bool → Bool → Bool → Bool → Bool → β) → α → α → α → α → α → [β]
{-# RULES "Bitstream zipWith5/unstream fusion" ∀f s1 s2 s3 s4 s5.
    zipWith5 f (unstream s1) (unstream s2) (unstream s3) (unstream s4) (unstream s5)
        = S.toList (S.zipWith5 f s1 s2 s3 s4 s5)
  #-}
{-# INLINEABLE [1] zipWith5 #-}
zipWith5 f α β γ δ ε = S.toList $
                       S.zipWith5 f
                            (stream α)
                            (stream β)
                            (stream γ)
                            (stream δ)
                            (stream ε)

-- | The 'zipWith6' function takes a function which combines six bits,
-- as well as six 'Bitstream's and returns a list of their point-wise
-- combination, analogous to 'zipWith'.
zipWith6 ∷ Bitstream α ⇒ (Bool → Bool → Bool → Bool → Bool → Bool → β) → α → α → α → α → α → α → [β]
{-# RULES "Bitstream zipWith6/unstream fusion" ∀f s1 s2 s3 s4 s5 s6.
    zipWith6 f (unstream s1) (unstream s2) (unstream s3) (unstream s4) (unstream s5) (unstream s6)
        = S.toList (S.zipWith6 f s1 s2 s3 s4 s5 s6)
  #-}
{-# INLINEABLE [1] zipWith6 #-}
zipWith6 f α β γ δ ε ζ = S.toList $
                         S.zipWith6 f
                              (stream α)
                              (stream β)
                              (stream γ)
                              (stream δ)
                              (stream ε)
                              (stream ζ)

-- | /O(min(m, n))/ 'unzip' transforms a list of bit pairs into a
-- 'Bitstream' of first components and a 'Bitstream' of second
-- components.
unzip ∷ Bitstream α ⇒ [(Bool, Bool)] → (α, α)
{-# INLINEABLE [1] unzip #-}
unzip xs = ( unstream $ S.map fst $ S.fromList xs
           , unstream $ S.map snd $ S.fromList xs )

-- | The 'unzip3' function takes a list of triples and returns three
-- 'Bitstream's, analogous to 'unzip'.
unzip3 ∷ Bitstream α ⇒ [(Bool, Bool, Bool)] → (α, α, α)
{-# INLINEABLE [1] unzip3 #-}
unzip3 xs = ( unstream $ S.map (\(α, _, _) → α) $ S.fromList xs
            , unstream $ S.map (\(_, β, _) → β) $ S.fromList xs
            , unstream $ S.map (\(_, _, γ) → γ) $ S.fromList xs )

-- | The 'unzip4' function takes a list of quadruples and returns
-- four 'Bitstream's, analogous to 'unzip'.
unzip4 ∷ Bitstream α ⇒ [(Bool, Bool, Bool, Bool)] → (α, α, α, α)
{-# INLINEABLE [1] unzip4 #-}
unzip4 xs = ( unstream $ S.map (\(α, _, _, _) → α) $ S.fromList xs
            , unstream $ S.map (\(_, β, _, _) → β) $ S.fromList xs
            , unstream $ S.map (\(_, _, γ, _) → γ) $ S.fromList xs
            , unstream $ S.map (\(_, _, _, δ) → δ) $ S.fromList xs )

-- | The 'unzip5' function takes a list of five-tuples and returns
-- five 'Bitstream's, analogous to 'unzip'.
unzip5 ∷ Bitstream α ⇒ [(Bool, Bool, Bool, Bool, Bool)] → (α, α, α, α, α)
{-# INLINEABLE [1] unzip5 #-}
unzip5 xs = ( unstream $ S.map (\(α, _, _, _, _) → α) $ S.fromList xs
            , unstream $ S.map (\(_, β, _, _, _) → β) $ S.fromList xs
            , unstream $ S.map (\(_, _, γ, _, _) → γ) $ S.fromList xs
            , unstream $ S.map (\(_, _, _, δ, _) → δ) $ S.fromList xs
            , unstream $ S.map (\(_, _, _, _, ε) → ε) $ S.fromList xs )

-- | The 'unzip6' function takes a list of six-tuples and returns six
-- 'Bitstream's, analogous to 'unzip'.
unzip6 ∷ Bitstream α ⇒ [(Bool, Bool, Bool, Bool, Bool, Bool)] → (α, α, α, α, α, α)
{-# INLINEABLE [1] unzip6 #-}
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

"Bitstream cons'/unstream fusion"
    ∀b s. cons' b (unstream s) = unstream (S.cons b s)

"Bitstream snoc/unstream fusion"
    ∀s b. snoc (unstream s) b = unstream (S.snoc s b)

"Bitstream append/unstream fusion"
    ∀s1 s2. append (unstream s1) (unstream s2) = unstream (s1 S.++ s2)

"Bitstream tail/unstream fusion"
    ∀s. tail (unstream s) = unstream (S.tail s)

"Bitstream init/unstream fusion"
    ∀s. init (unstream s) = unstream (S.init s)

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
