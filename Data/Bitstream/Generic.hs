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
import Prelude hiding ( break, concat, foldr, head, length, map, null
                      , replicate, scanr, scanr1, span, tail
                      )
import Prelude.Unicode hiding ((⧺))

infixr 5 ⧺

-- THINKME: consider using numeric-prelude's non-negative numbers
-- instead of Integral n.

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
    transpose (α:αs)
        | null α       = transpose αs
        | otherwise    = (head α `cons` pack (L.map head αs))
                         : transpose (tail α : L.map tail αs)

    foldl ∷ (β → Bool → β) → β → α → β
    foldl f β = S.foldl f β ∘ stream
    {-# INLINE foldl #-}

    foldl' ∷ (β → Bool → β) → β → α → β
    foldl' f β = S.foldl' f β ∘ stream
    {-# INLINE foldl' #-}

    foldl1 ∷ (Bool → Bool → Bool) → α → Bool
    foldl1 = (∘ stream) ∘ S.foldl1
    {-# INLINE foldl1 #-}

    foldl1' ∷ (Bool → Bool → Bool) → α → Bool
    foldl1' = (∘ stream) ∘ S.foldl1'
    {-# INLINE foldl1' #-}

    foldr ∷ (Bool → β → β) → β → α → β
    foldr f β = S.foldr f β ∘ stream
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
    scanl f β α = unstream (S.scanl f β (S.snoc (stream α) (⊥)))
    {-# INLINE scanl #-}

    scanl1 ∷ (Bool → Bool → Bool) → α → α
    scanl1 f α = unstream (S.scanl1 f (S.snoc (stream α) (⊥)))
    {-# INLINE scanl1 #-}

    scanr ∷ (Bool → Bool → Bool) → Bool → α → α
    scanr f β α
        | null α    = singleton β
        | otherwise = let α' = scanr f β (tail α)
                      in
                        f (head α) (head α') `cons` α'
    {-# INLINE scanr #-}

    scanr1 ∷ (Bool → Bool → Bool) → α → α
    scanr1 f α
        | null α        = α
        | null (tail α) = α
        | otherwise     = let α' = scanr1 f (tail α)
                          in
                            f (head α) (head α') `cons` α'
    {-# INLINE scanr1 #-}

    mapAccumL ∷ (β → Bool → (β, Bool)) → β → α → (β, α)
    mapAccumL f s α
        | null α    = (s, α)
        | otherwise = let (s' , a ) = f s (head α)
                          (s'', α') = mapAccumL f s' (tail α)
                      in
                        (s'', a `cons` α')

    mapAccumR ∷ (β → Bool → (β, Bool)) → β → α → (β, α)
    mapAccumR f s α
        | null α    = (s, α)
        | otherwise = let (s'', a ) = f s' (head α)
                          (s' , α') = mapAccumR f s (tail α)
                      in
                        (s'', a `cons` α')

    iterate ∷ (Bool → Bool) → Bool → α
    iterate = (unstream ∘) ∘ S.iterate
    {-# INLINE iterate #-}

    repeat ∷ Bool → α
    repeat = unstream ∘ S.repeat
    {-# INLINE repeat #-}

    replicate ∷ Integral n ⇒ n → Bool → α
    replicate n a
        | n ≤ 0     = (∅)
        | otherwise = a `cons` replicate (n-1) a
    {-# INLINE replicate #-}

    cycle ∷ α → α
    cycle = unstream ∘ S.cycle ∘ stream
    {-# INLINE cycle #-}

    unfoldr ∷ (β → Maybe (Bool, β)) → β → α
    unfoldr = (unstream ∘) ∘ S.unfoldr
    {-# INLINE unfoldr #-}

    take ∷ Integral n ⇒ n → α → α
    take = (unstream ∘) ∘ (∘ stream) ∘ S.genericTake
    {-# INLINE take #-}

    drop ∷ Integral n ⇒ n → α → α
    drop = (unstream ∘) ∘ (∘ stream) ∘ S.genericDrop
    {-# INLINE drop #-}

    splitAt ∷ Integral n ⇒ n → α → (α, α)
    splitAt n α
        = case S.genericSplitAt n (stream α) of
            (xs, ys)
                → (pack xs, pack ys)
    {-# INLINE splitAt #-}

    takeWhile ∷ (Bool → Bool) → α → α
    takeWhile = (unstream ∘) ∘ (∘ stream) ∘ S.takeWhile
    {-# INLINE takeWhile #-}

    dropWhile ∷ (Bool → Bool) → α → α
    dropWhile = (unstream ∘) ∘ (∘ stream) ∘ S.dropWhile
    {-# INLINE dropWhile #-}

    span ∷ (Bool → Bool) → α → (α, α)
    span f α
        | null α     = (α, α)
        | f (head α) = let (β, γ) = span f (tail α)
                       in
                         (head α `cons` β, γ)
        | otherwise  = ((∅), α)

    break ∷ (Bool → Bool) → α → (α, α)
    break f α
        | null α     = (α, α)
        | f (head α) = ((∅), α)
        | otherwise  = let (β, γ) = break f (tail α)
                       in
                         (head α `cons` β, γ)

{-# RULES
"Bitstream stream/unstream fusion"
    ∀s. stream (unstream s) = s
  #-}
