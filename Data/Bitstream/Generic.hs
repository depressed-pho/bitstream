{-# LANGUAGE
    RankNTypes
  , UnicodeSyntax
  #-}
module Data.Bitstream.Generic
    ( Bitstream(..)
    )
    where
import qualified Data.List   as L
import qualified Data.Stream as S
import Prelude hiding ( concat, foldr, head, length, map, null, replicate
                      , scanr, scanr1, tail
                      )
import Prelude.Unicode hiding ((⧺))

infixr 5 ⧺

-- FIXME: use numeric-prelude

class Bitstream α where
    stream   ∷ α → S.Stream Bool
    unstream ∷ S.Stream Bool → α

    empty ∷ α
    empty = unstream (S.stream [])
    {-# INLINE empty #-}

    (∅) ∷ α
    (∅) = empty
    {-# INLINE (∅) #-}

    singleton ∷ Bool → α
    singleton = unstream ∘ S.stream ∘ flip (:) []
    {-# INLINE singleton #-}

    pack ∷ [Bool] → α
    pack = unstream ∘ S.stream
    {-# INLINE pack #-}

    unpack ∷ α → [Bool]
    unpack = S.unstream ∘ stream
    {-# INLINE unpack #-}

    cons ∷ Bool → α → α
    cons = (unstream ∘) ∘ (∘ stream) ∘ S.cons
    {-# INLINE cons #-}

    snoc ∷ α → Bool → α
    snoc = (unstream ∘) ∘ S.snoc . stream
    {-# INLINE snoc #-}

    append ∷ α → α → α
    append = (unstream ∘) ∘ (∘ stream) ∘ S.append ∘ stream
    {-# INLINE append #-}

    (⧺) ∷ α → α → α
    (⧺) = append
    {-# INLINE (⧺) #-}

    head ∷ α → Bool
    head = S.head ∘ stream
    {-# INLINE head #-}

    uncons ∷ α → Maybe (Bool, α)
    uncons α
        | null α    = Nothing
        | otherwise = Just (head α, tail α)
    {-# INLINE uncons #-}

    last ∷ α → Bool
    last = S.last ∘ stream
    {-# INLINE last #-}

    tail ∷ α → α
    tail = unstream ∘ S.tail ∘ stream
    {-# INLINE tail #-}

    init ∷ α → α
    init = unstream ∘ S.init ∘ stream
    {-# INLINE init #-}

    null ∷ α → Bool
    null = S.null ∘ stream
    {-# INLINE null #-}

    length ∷ Num n ⇒ α → n
    length = S.genericLength ∘ stream
    {-# INLINE length #-}

    map ∷ (Bool → Bool) → α → α
    map = (unstream ∘) ∘ (∘ stream) ∘ S.map
    {-# INLINE map #-}

    reverse ∷ α → α
    reverse = foldl' (flip cons) (∅)
    {-# INLINE reverse #-}

    intersperse ∷ Bool → α → α
    intersperse = (unstream ∘) ∘ (∘ stream) ∘ S.intersperse
    {-# INLINE intersperse #-}

    intercalate ∷ α → [α] → α
    intercalate α = S.foldr (⧺) (∅) ∘ S.intersperse α ∘ S.stream
    {-# INLINE intercalate #-}

    transpose ∷ [α] → [α]
    transpose []       = []
    transpose (xs:xss)
        | null xs      = transpose xss
        | otherwise    = (head xs `cons` pack (L.map head xss))
                         : transpose (tail xs : L.map tail xss)

    foldl ∷ (β → Bool → β) → β → α → β
    foldl f z = S.foldl f z ∘ stream
    {-# INLINE foldl #-}

    foldl' ∷ (β → Bool → β) → β → α → β
    foldl' f z = S.foldl' f z ∘ stream
    {-# INLINE foldl' #-}

    foldl1 ∷ (Bool → Bool → Bool) → α → Bool
    foldl1 = (∘ stream) ∘ S.foldl1
    {-# INLINE foldl1 #-}

    foldl1' ∷ (Bool → Bool → Bool) → α → Bool
    foldl1' = (∘ stream) ∘ S.foldl1'
    {-# INLINE foldl1' #-}

    foldr ∷ (Bool → β → β) → β → α → β
    foldr f z = S.foldr f z ∘ stream
    {-# INLINE foldr #-}

    foldr1 ∷ (Bool → Bool → Bool) → α → Bool
    foldr1 = (∘ stream) ∘ S.foldr1
    {-# INLINE foldr1 #-}

    concat ∷ [α] → α
    concat = S.foldr (⧺) (∅) ∘ S.stream
    {-# INLINE concat #-}

    concatMap ∷ (Bool → α) → α → α
    concatMap f = foldr (\x y → f x ⧺ y) (∅)
    {-# INLINE concatMap #-}

    and ∷ α → Bool
    and = S.and ∘ stream
    {-# INLINE and #-}

    or ∷ α → Bool
    or = S.or ∘ stream
    {-# INLINE or #-}

    any ∷ (Bool → Bool) → α → Bool
    any = (∘ stream) ∘ S.any
    {-# INLINE any #-}

    all ∷ (Bool → Bool) → α → Bool
    all = (∘ stream) ∘ S.all
    {-# INLINE all #-}

    scanl ∷ (Bool → Bool → Bool) → Bool → α → α
    scanl f z xs = unstream (S.scanl f z (S.snoc (stream xs) (⊥)))
    {-# INLINE scanl #-}

    scanl1 ∷ (Bool → Bool → Bool) → α → α
    scanl1 f xs = unstream (S.scanl1 f (S.snoc (stream xs) (⊥)))
    {-# INLINE scanl1 #-}

    scanr ∷ (Bool → Bool → Bool) → Bool → α → α
    scanr f z xs
        | null xs   = singleton z
        | otherwise = let xs' = scanr f z (tail xs)
                      in
                        f (head xs) (head xs') `cons` xs'
    {-# INLINE scanr #-}

    scanr1 ∷ (Bool → Bool → Bool) → α → α
    scanr1 f xs
        | null xs        = xs
        | null (tail xs) = xs
        | otherwise      = let xs' = scanr1 f (tail xs)
                           in
                             f (head xs) (head xs') `cons` xs'
    {-# INLINE scanr1 #-}

    mapAccumL ∷ (β → Bool → (β, Bool)) → β → α → (β, α)
    mapAccumL f s xs
        | null xs   = (s, xs)
        | otherwise = let (s' , y ) = f s (head xs)
                          (s'', ys) = mapAccumL f s' (tail xs)
                      in
                        (s'', y `cons` ys)

    mapAccumR ∷ (β → Bool → (β, Bool)) → β → α → (β, α)
    mapAccumR f s xs
        | null xs   = (s, xs)
        | otherwise = let (s'', y ) = f s' (head xs)
                          (s' , ys) = mapAccumR f s (tail xs)
                      in
                        (s'', y `cons` ys)

    iterate ∷ (Bool → Bool) → Bool → α
    iterate = (unstream ∘) ∘ S.iterate
    {-# INLINE iterate #-}

    repeat ∷ Bool → α
    repeat = unstream ∘ S.repeat
    {-# INLINE repeat #-}

    replicate ∷ Integral n ⇒ n → Bool → α
    replicate n x
        | n ≤ 0     = (∅)
        | otherwise = x `cons` replicate (n-1) x
    {-# INLINE replicate #-}

    cycle ∷ α → α
    cycle = unstream ∘ S.cycle ∘ stream
    {-# INLINE cycle #-}

    unfoldr ∷ (β → Maybe (Bool, β)) → β → α
    unfoldr = (unstream ∘) ∘ S.unfoldr
    {-# INLINE unfoldr #-}

{-# RULES
"Bitstream stream/unstream fusion"
    ∀s. stream (unstream s) = s
  #-}
