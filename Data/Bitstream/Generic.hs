{-# LANGUAGE
    RankNTypes
  , UnicodeSyntax
  #-}
module Data.Bitstream.Generic
    ( Bitstream(..)

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
    )
    where
import qualified Data.List.Stream as L
import Data.Maybe
import qualified Data.Stream as S
import Prelude (Bool(..), Integral(..), Num(..), flip, otherwise)
import Prelude.Unicode hiding ((∈), (∉), (⧺))

infix  4 ∈, ∋, ∉, ∌, `elem`, `notElem`
infixr 5 ⧺, `append`
infixl 6 ∪, `union`
infixr 6 ∩, `intersect`
infixl 9 !!, ∖, \\, ∆

-- THINKME: consider using numeric-prelude's non-negative numbers
-- instead of Integral n.

class Bitstream α where
    stream   ∷ α → S.Stream Bool
    unstream ∷ S.Stream Bool → α

    empty ∷ α
    empty = unstream (S.stream [])
    {-# INLINE empty #-}

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
    snoc = (unstream ∘) ∘ S.snoc ∘ stream
    {-# INLINE snoc #-}

    append ∷ α → α → α
    append = (unstream ∘) ∘ (∘ stream) ∘ S.append ∘ stream
    {-# INLINE append #-}

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
    transpose []     = []
    transpose (α:αs)
        = case uncons α of
            Nothing      → transpose αs
            Just (a, as) → (a `cons` pack (L.map head αs))
                           : transpose (as : L.map tail αs)

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
    concat = unstream ∘ S.concatMap stream ∘ S.stream
    {-# INLINE concat #-}

    concatMap ∷ (Bool → α) → α → α
    concatMap f = unstream ∘ S.concatMap (stream ∘ f) ∘ stream
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
        = case uncons α of
            Nothing      → singleton β
            Just (a, as) → let α' = scanr f β as
                           in
                             f a (head α') `cons` α'

    scanr1 ∷ (Bool → Bool → Bool) → α → α
    scanr1 f α
        = case uncons α of
            Nothing         → α
            Just (a, as)
                | null as   → α
                | otherwise → let α' = scanr1 f as
                              in
                                f a (head α') `cons` α'

    mapAccumL ∷ (β → Bool → (β, Bool)) → β → α → (β, α)
    mapAccumL f s α
        = case uncons α of
            Nothing      → (s, α)
            Just (a, as) → let (s' , b ) = f s a
                               (s'', α') = mapAccumL f s' as
                           in
                             (s'', b `cons` α')

    mapAccumR ∷ (β → Bool → (β, Bool)) → β → α → (β, α)
    mapAccumR f s α
        = case uncons α of
            Nothing      → (s, α)
            Just (a, as) → let (s'', b ) = f s' a
                               (s' , α') = mapAccumR f s as
                           in
                             (s'', b `cons` α')

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

    unfoldrN ∷ Integral n ⇒ n → (β → Maybe (Bool, β)) → β → (α, Maybe β)
    unfoldrN n0 f β0 = loop_unfoldrN n0 β0 (∅)
        where
          loop_unfoldrN 0 β α = (α, Just β)
          loop_unfoldrN n β α
              = case f β of
                  Nothing      → (α, Nothing)
                  Just (a, β') → loop_unfoldrN (n-1) β' (a `cons` α)
    {-# INLINE unfoldrN #-}

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
        = case uncons α of
            Nothing         → (α, α)
            Just (a, as)
                | f a       → let (β, γ) = span f as
                              in
                                (a `cons` β, γ)
                | otherwise → ((∅), α)

    break ∷ (Bool → Bool) → α → (α, α)
    break f α
        = case uncons α of
            Nothing         → (α, α)
            Just (a, as)
                | f a       → ((∅), α)
                | otherwise → let (β, γ) = break f as
                              in
                                (a `cons` β, γ)

    group ∷ α → [α]
    group α
        = case uncons α of
            Nothing      → []
            Just (a, as) → let (β, γ) = span (a ≡) as
                           in
                             (a `cons` β) : group γ

    inits ∷ α → [α]
    inits α
        = case uncons α of
            Nothing      → α : []
            Just (a, as) → (∅) : L.map (cons a) (inits as)

    tails ∷ α → [α]
    tails α
        = case uncons α of
            Nothing      → α : []
            Just (_, as) → α : tails as

    isPrefixOf ∷ α → α → Bool
    isPrefixOf x y = S.isPrefixOf (stream x) (stream y)
    {-# INLINE isPrefixOf #-}

    isSuffixOf ∷ α → α → Bool
    isSuffixOf x y = reverse x `isPrefixOf` reverse y
    {-# INLINE isSuffixOf #-}

    isInfixOf ∷ α → α → Bool
    isInfixOf x y = L.any (x `isPrefixOf`) (tails y)
    {-# INLINE isInfixOf #-}

    elem ∷ Bool → α → Bool
    elem = (∘ stream) ∘ S.elem
    {-# INLINE elem #-}

    notElem ∷ Bool → α → Bool
    notElem = ((¬) ∘) ∘ (∈)
    {-# INLINE notElem #-}

    filter ∷ (Bool → Bool) → α → α
    filter = (unstream ∘) ∘ (∘ stream) ∘ S.filter
    {-# INLINE filter #-}

    find ∷ (Bool → Bool) → α → Maybe Bool
    find = (∘ stream) ∘ S.find
    {-# INLINE find #-}

    partition ∷ (Bool → Bool) → α → (α, α)
    partition f α = foldr select ((∅), (∅)) α
        where
          select a ~(β, γ)
              | f a       = (a `cons` β, γ)
              | otherwise = (β, a `cons` γ)

    (!!) ∷ Integral n ⇒ α → n → Bool
    (!!) = S.genericIndex ∘ stream
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
    zip a b = S.unstream (S.zip (stream a) (stream b))
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
    zipWith f α β = S.unstream (S.zipWith f
                                     (stream α)
                                     (stream β))
    {-# INLINE zipWith #-}

    zipWith3 ∷ (Bool → Bool → Bool → β) → α → α → α → [β]
    zipWith3 f α β γ = S.unstream (S.zipWith3 f
                                        (stream α)
                                        (stream β)
                                        (stream γ))
    {-# INLINE zipWith3 #-}

    zipWith4 ∷ (Bool → Bool → Bool → Bool → β) → α → α → α → α → [β]
    zipWith4 f α β γ δ = S.unstream (S.zipWith4 f
                                          (stream α)
                                          (stream β)
                                          (stream γ)
                                          (stream δ))
    {-# INLINE zipWith4 #-}

    zipWith5 ∷ (Bool → Bool → Bool → Bool → Bool → β) → α → α → α → α → α → [β]
    zipWith5 f α β γ δ ε
        = case (uncons α, uncons β, uncons γ, uncons δ, uncons ε) of
            (Nothing, _, _, _, _) → []
            (_, Nothing, _, _, _) → []
            (_, _, Nothing, _, _) → []
            (_, _, _, Nothing, _) → []
            (_, _, _, _, Nothing) → []
            (Just (a, as), Just (b, bs), Just (c, cs), Just (d, ds), Just (e, es))
                → f a b c d e : zipWith5 f as bs cs ds es

    zipWith6 ∷ (Bool → Bool → Bool → Bool → Bool → Bool → β) → α → α → α → α → α → α → [β]
    zipWith6 f α β γ δ ε ζ
        = case (uncons α, uncons β, uncons γ, uncons δ, uncons ε, uncons ζ) of
            (Nothing, _, _, _, _, _) → []
            (_, Nothing, _, _, _, _) → []
            (_, _, Nothing, _, _, _) → []
            (_, _, _, Nothing, _, _) → []
            (_, _, _, _, Nothing, _) → []
            (_, _, _, _, _, Nothing) → []
            (Just (a, as), Just (b, bs), Just (c, cs), Just (d, ds), Just (e, es), Just (f', fs))
                → f a b c d e f' : zipWith6 f as bs cs ds es fs

    zipWith7 ∷ (Bool → Bool → Bool → Bool → Bool → Bool → Bool → β) → α → α → α → α → α → α → α → [β]
    zipWith7 f α β γ δ ε ζ η
        = case (uncons α, uncons β, uncons γ, uncons δ, uncons ε, uncons ζ, uncons η) of
            (Nothing, _, _, _, _, _, _) → []
            (_, Nothing, _, _, _, _, _) → []
            (_, _, Nothing, _, _, _, _) → []
            (_, _, _, Nothing, _, _, _) → []
            (_, _, _, _, Nothing, _, _) → []
            (_, _, _, _, _, Nothing, _) → []
            (_, _, _, _, _, _, Nothing) → []
            (Just (a, as), Just (b, bs), Just (c, cs), Just (d, ds), Just (e, es), Just (f', fs), Just (g, gs))
                → f a b c d e f' g : zipWith7 f as bs cs ds es fs gs

    unzip ∷ [(Bool, Bool)] → (α, α)
    unzip = L.foldr (\(a, b) ~(as, bs) →
                         ( a `cons` as
                         , b `cons` bs )) ((∅), (∅))

    unzip3 ∷ [(Bool, Bool, Bool)] → (α, α, α)
    unzip3 = L.foldr (\(a, b, c) ~(as, bs, cs) →
                          ( a `cons` as
                          , b `cons` bs
                          , c `cons` cs )) ((∅), (∅), (∅))

    unzip4 ∷ [(Bool, Bool, Bool, Bool)] → (α, α, α, α)
    unzip4 = L.foldr (\(a, b, c, d) ~(as, bs, cs, ds) →
                          ( a `cons` as
                          , b `cons` bs
                          , c `cons` cs
                          , d `cons` ds )) ((∅), (∅), (∅), (∅))

    unzip5 ∷ [(Bool, Bool, Bool, Bool, Bool)] → (α, α, α, α, α)
    unzip5 = L.foldr (\(a, b, c, d, e) ~(as, bs, cs, ds, es) →
                          ( a `cons` as
                          , b `cons` bs
                          , c `cons` cs
                          , d `cons` ds
                          , e `cons` es )) ((∅), (∅), (∅), (∅), (∅))

    unzip6 ∷ [(Bool, Bool, Bool, Bool, Bool, Bool)] → (α, α, α, α, α, α)
    unzip6 = L.foldr (\(a, b, c, d, e, f) ~(as, bs, cs, ds, es, fs) →
                          ( a `cons` as
                          , b `cons` bs
                          , c `cons` cs
                          , d `cons` ds
                          , e `cons` es
                          , f `cons` fs )) ((∅), (∅), (∅), (∅), (∅), (∅))

    unzip7 ∷ [(Bool, Bool, Bool, Bool, Bool, Bool, Bool)] → (α, α, α, α, α, α, α)
    unzip7 = L.foldr (\(a, b, c, d, e, f, g) ~(as, bs, cs, ds, es, fs, gs) →
                          ( a `cons` as
                          , b `cons` bs
                          , c `cons` cs
                          , d `cons` ds
                          , e `cons` es
                          , f `cons` fs
                          , g `cons` gs )) ((∅), (∅), (∅), (∅), (∅), (∅), (∅))

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

    delete ∷ Bool → α → α
    delete = deleteBy (≡)
    {-# INLINE delete #-}

    (\\) ∷ α → α → α
    (\\) = foldl (flip delete)
    {-# INLINE (\\) #-}

    union ∷ α → α → α
    union = unionBy (≡)
    {-# INLINE union #-}

    intersect ∷ α → α → α
    intersect = intersectBy (≡)
    {-# INLINE intersect #-}

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

    deleteBy ∷ (Bool → Bool → Bool) → Bool → α → α
    deleteBy f b α
        = case uncons α of
            Nothing         → α
            Just (a, as)
                | f b a     → as
                | otherwise → a `cons` deleteBy f b as

    deleteFirstsBy ∷ (Bool → Bool → Bool) → α → α → α
    deleteFirstsBy = foldl ∘ flip ∘ deleteBy

    unionBy ∷ (Bool → Bool → Bool) → α → α → α
    unionBy f x y = x ⧺ foldl (flip (deleteBy f)) (nubBy f y) x

    intersectBy ∷ (Bool → Bool → Bool) → α → α → α
    intersectBy f x y = filter (\a → any (f a) y) x

    groupBy ∷ (Bool → Bool → Bool) → α → [α]
    groupBy f α
        = case uncons α of
            Nothing     → []
            Just (a, _) → let (β, γ) = span (f a) α
                          in
                            (a `cons` β) : groupBy f γ

{-# RULES

"Bitstream stream/unstream fusion"
    ∀s. stream (unstream s) = s
"Bitstream stream / List unstream fusion"
    ∀s. stream (S.unstream s) = s
"List stream / Bitstream unstream fusion"
    ∀s. S.stream (unstream s) = s

"cons → fusible" [~1]
    ∀b α. cons b α = unstream (S.cons b (stream α))
"cons → unfused" [ 1]
    ∀b α. unstream (S.cons b (stream α)) = cons b α

"snoc → fusible" [~1]
    ∀α b. snoc α b = unstream (S.snoc (stream α) b)
"snoc → unfused" [ 1]
    ∀α b. unstream (S.snoc (stream α) b) = snoc α b

"append → fusible" [~1]
    ∀x y. append x y = unstream (S.append (stream x) (stream y))
"append → unfused" [ 1]
    ∀x y. unstream (S.append (stream x) (stream y)) = append x y

"head → fusible" [~1]
    ∀α. head α = S.head (stream α)
"head → unfused" [ 1]
    ∀α. S.head (stream α) = head α

"last → fusible" [~1]
    ∀α. last α = S.last (stream α)
"last → unfused" [ 1]
    ∀α. S.last (stream α) = last α

"tail → fusible" [~1]
    ∀α. tail α = unstream (S.tail (stream α))
"tail → unfused" [ 1]
    ∀α. unstream (S.tail (stream α)) = last α

"init → fusible" [~1]
    ∀α. init α = unstream (S.init (stream α))
"init → unfused" [ 1]
    ∀α. unstream (S.init (stream α)) = last α

"null → fusible" [~1]
    ∀α. null α = S.null (stream α)
"null → unfused" [ 1]
    ∀α. S.null (stream α) = null α

"length → fusible" [~1]
    ∀α. length α = S.genericLength (stream α)
"length → unfused" [ 1]
    ∀α. S.genericLength (stream α) = length α

"map → fusible" [~1]
    ∀α f. map f α = unstream (S.map f (stream α))
"map → unfused" [ 1]
    ∀α f. unstream (S.map f (stream α)) = map f α

"concat → fusible" [~1]
    ∀αs. concat αs = S.foldr (⧺) (∅) (S.stream αs)
"concat → unfused" [ 1]
    ∀αs. S.foldr (⧺) (∅) (S.stream αs) = concat αs

"concatMap → fusible" [~1]
    ∀α f. concatMap f α = unstream (S.concatMap (stream ∘ f) (stream α))
"concatMap → unfused" [ 1]
    ∀α f. unstream (S.concatMap (stream ∘ f) (stream α)) = concatMap f α

"and → fusible" [~1]
    ∀α. and α = S.and (stream α)
"and → unfused" [ 1]
    ∀α. S.and (stream α) = and α

"or → fusible" [~1]
    ∀α. or α = S.or (stream α)
"or → unfused" [ 1]
    ∀α. S.or (stream α) = or α

  #-}

(∅) ∷ Bitstream α ⇒ α
(∅) = empty
{-# INLINE (∅) #-}

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
