{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , ScopedTypeVariables
  , UndecidableInstances
  , UnicodeSyntax
  #-}
module Main (main) where
import Data.Bitstream.Lazy (Bitstream, Left, Right)
import Data.ByteString.Lazy.Char8 ()
import qualified Data.Bitstream.Lazy as B
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.List.Unicode
import qualified Data.Monoid as M
import qualified Data.Monoid.Unicode as M
import qualified Data.Vector.Fusion.Stream as S
import Prelude.Unicode
import Test.Bitstream.Utils
import Test.QuickCheck

main ∷ IO ()
main = mapM_ runTest tests

type BitL = Bitstream Left
type BitR = Bitstream Right

tests ∷ [Property]
tests = [ -- ∅
          conjoin
          [ property $ B.null      ((B.∅) ∷ BitL)
          , property $ B.length    ((B.∅) ∷ BitL) ≡ (0 ∷Int)
          , property $ B.pack [] ≡ ((B.∅) ∷ BitL)
          , property $ B.empty   ≡ ((B.∅) ∷ BitL)
          ]

        , conjoin
          [ property $ B.null      ((B.∅) ∷ BitR)
          , property $ B.length    ((B.∅) ∷ BitR) ≡ (0 ∷ Int)
          , property $ B.pack [] ≡ ((B.∅) ∷ BitR)
          , property $ B.empty   ≡ ((B.∅) ∷ BitR)
          ]

          -- singleton
        , property $ \b → B.length (B.singleton b ∷ BitL) ≡ (1 ∷ Int)
        , property $ \b → B.head   (B.singleton b ∷ BitL) ≡ b
        , property $ \b → B.pack [b] ≡ (B.singleton b ∷ BitL)

        , property $ \b → B.length (B.singleton b ∷ BitR) ≡ (1 ∷ Int)
        , property $ \b → B.head   (B.singleton b ∷ BitR) ≡ b
        , property $ \b → B.pack [b] ≡ (B.singleton b ∷ BitR)

          -- pack/unpack
        , conjoin
          [ property $ B.unpack (B.pack []      ∷ BitL) ≡ []
          , property $ B.unpack (B.pack [False] ∷ BitL) ≡ [False]
          , property $ B.unpack (B.pack [True ] ∷ BitL) ≡ [True ]
          ]
        , property $ \bl → B.unpack (B.pack bl ∷ BitL) ≡ bl
        , property $ \bs → B.pack (B.unpack (bs ∷ BitL)) ≡ bs

        , conjoin
          [ property $ B.unpack (B.pack []      ∷ BitR) ≡ []
          , property $ B.unpack (B.pack [False] ∷ BitR) ≡ [False]
          , property $ B.unpack (B.pack [True ] ∷ BitR) ≡ [True ]
          ]
        , property $ \bl → B.unpack (B.pack bl ∷ BitR) ≡ bl
        , property $ \bs → B.pack (B.unpack (bs ∷ BitR)) ≡ bs

          -- from/toByteString
        , property $ (B.fromByteString "UNK" ∷ BitL)
                       ≡ B.pack (map n2b [ 1, 0, 1, 0, 1, 0, 1, 0
                                         , 0, 1, 1, 1, 0, 0, 1, 0
                                         , 1, 1, 0, 1, 0, 0, 1, 0 ])
        , property $ \str → B.toByteString (B.fromByteString str ∷ BitL) ≡ str
        , mapSize (⋅ 8) $ \bs → (B.length bs `rem` 8) ≡ (0 ∷ Int)
                                  ⟹ B.fromByteString (B.toByteString (bs ∷ BitL)) ≡ bs

        , property $ (B.fromByteString "UNK" ∷ BitR)
                       ≡ B.pack (map n2b [ 0, 1, 0, 1, 0, 1, 0, 1
                                         , 0, 1, 0, 0, 1, 1, 1, 0
                                         , 0, 1, 0, 0, 1, 0, 1, 1 ])
        , property $ \str → B.toByteString (B.fromByteString str ∷ BitR) ≡ str
        , mapSize (⋅ 8) $ \bs → (B.length bs `rem` 8) ≡ (0 ∷ Int)
                                  ⟹ B.fromByteString (B.toByteString (bs ∷ BitR)) ≡ bs

          -- stream/unstream
        , property $ \bl → B.unstream (S.fromList bl) ≡ (B.pack bl ∷ BitL)
        , property $ \bs → S.toList   (B.stream   bs) ≡ (B.unpack (bs ∷ BitL))

        , property $ \bl → B.unstream (S.fromList bl) ≡ (B.pack bl ∷ BitR)
        , property $ \bs → S.toList   (B.stream   bs) ≡ (B.unpack (bs ∷ BitR))

          -- direction
        , conjoin
          [ property $ B.toByteString (B.directionLToR (B.pack (map n2b [1,1,0,1,0,0,1,0, 1,0,0])))
                         ≡ BS.pack [0xD2, 0x80]
          , property $ B.toByteString (B.directionRToL (B.pack (map n2b [1,1,0,1,0,0,1,0, 1,0,0])))
                         ≡ BS.pack [0x4B, 0x01]
          ]
        , property $ \bs → B.directionRToL (B.directionLToR bs) ≡ bs
        , property $ \bs → B.directionLToR (B.directionRToL bs) ≡ bs

          -- show
        , conjoin
          [ property $ show (B.pack [ True , False, False, True, True
                                    , False, False, False, True, False
                                    , True , False, True , True, True  ] ∷ BitL)
                       ≡ "Chunk (S[00011001←][1110101←]) Empty"
          , property $ show (B.pack [ True , False, False, True, True
                                    , False, False, False, True, False
                                    , True , False, True , True, True  ] ∷ BitR)
                       ≡ "Chunk (S[→10011000][→1010111]) Empty"
          ]

          -- equality
        , property $ \(bl1, bl2) → ((B.pack bl1 ∷ BitL) ≡ B.pack bl2) ≡ (bl1 ≡ bl2)
        , property $ \(bl1, bl2) → ((B.pack bl1 ∷ BitR) ≡ B.pack bl2) ≡ (bl1 ≡ bl2)

          -- ordering
        , property $ \(bl1, bl2) → (B.pack bl1 ∷ BitL) `compare` B.pack bl2 ≡ bl1 `compare` bl2
        , property $ \(bl1, bl2) → (B.pack bl1 ∷ BitR) `compare` B.pack bl2 ≡ bl1 `compare` bl2

          -- monoid
        , conjoin
          [ property $ ((M.∅) ∷ BitL) ≡ B.pack (M.∅)
          , property $ ((M.∅) ∷ BitR) ≡ B.pack (M.∅)
          ]

        , property $ \(bl1, bl2) → (B.pack bl1 ∷ BitL) M.⊕ (B.pack bl2) ≡ B.pack (bl1 M.⊕ bl2)
        , property $ \bls → M.mconcat (map B.pack bls ∷ [BitL]) ≡ B.pack (M.mconcat bls)

        , property $ \(bl1, bl2) → (B.pack bl1 ∷ BitR) M.⊕ (B.pack bl2) ≡ B.pack (bl1 M.⊕ bl2)
        , property $ \bls → M.mconcat (map B.pack bls ∷ [BitR]) ≡ B.pack (M.mconcat bls)

          -- basic interface
        , property $ \(b, bl) → B.cons b (B.pack bl ∷ BitL) ≡ B.pack (b:bl)
        , property $ \(b, bl) → B.cons' b (B.pack bl ∷ BitL) ≡ B.pack (b:bl)
        , property $ \(bl, b) → B.snoc (B.pack bl ∷ BitL) b ≡ B.pack (bl ⧺ [b])
        , property $ \(x, y) → (B.pack x ∷ BitL) B.⧺ (B.pack y) ≡ B.pack (x ⧺ y)
        , property $ \bl → (¬) (null bl) ⟹ B.head (B.pack bl ∷ BitL) ≡ head bl
        , property $ \bl → (¬) (null bl) ⟹ B.last (B.pack bl ∷ BitL) ≡ last bl
        , property $ \bl → (¬) (null bl) ⟹ B.tail (B.pack bl ∷ BitL) ≡ B.pack (tail bl)
        , property $ \bl → (¬) (null bl) ⟹ B.init (B.pack bl ∷ BitL) ≡ B.pack (init bl)
        , property $ \bl → let bs = B.pack bl ∷ BitL
                           in case bl of
                                [] → label "null"     $ B.null bs
                                _  → label "non-null" $ (¬) (B.null bs)
        , property $ \bl → B.length (B.pack bl ∷ BitL) ≡ length bl

        , property $ \(b, bl) → B.cons b (B.pack bl ∷ BitR) ≡ B.pack (b:bl)
        , property $ \(b, bl) → B.cons' b (B.pack bl ∷ BitR) ≡ B.pack (b:bl)
        , property $ \(bl, b) → B.snoc (B.pack bl ∷ BitR) b ≡ B.pack (bl ⧺ [b])
        , property $ \(x, y) → (B.pack x ∷ BitR) B.⧺ (B.pack y) ≡ B.pack (x ⧺ y)
        , property $ \bl → (¬) (null bl) ⟹ B.head (B.pack bl ∷ BitR) ≡ head bl
        , property $ \bl → (¬) (null bl) ⟹ B.last (B.pack bl ∷ BitR) ≡ last bl
        , property $ \bl → (¬) (null bl) ⟹ B.tail (B.pack bl ∷ BitR) ≡ B.pack (tail bl)
        , property $ \bl → (¬) (null bl) ⟹ B.init (B.pack bl ∷ BitR) ≡ B.pack (init bl)
        , property $ \bl → let bs = B.pack bl ∷ BitR
                           in case bl of
                                [] → label "null"     $ B.null bs
                                _  → label "non-null" $ (¬) (B.null bs)
        , property $ \bl → B.length (B.pack bl ∷ BitR) ≡ length bl

          -- transformation
        , property $ \bl → B.map (¬) (B.pack bl ∷ BitL) ≡ B.pack (map (¬) bl)
        , property $ \bl → B.reverse (B.pack bl ∷ BitL) ≡ B.pack (reverse bl)

        , property $ \bl → B.map (¬) (B.pack bl ∷ BitR) ≡ B.pack (map (¬) bl)
        , property $ \bl → B.reverse (B.pack bl ∷ BitL) ≡ B.pack (reverse bl)

         -- reduction
        , property $ \(n, bl) → B.foldl doubleIf n (B.pack bl ∷ BitL) ≡ foldl doubleIf n bl
        , property $ \(n, bl) → B.foldl' doubleIf n (B.pack bl ∷ BitL) ≡ foldl doubleIf n bl
        , property $ \bl → (¬) (null bl) ⟹ B.foldl1 xor (B.pack bl ∷ BitL) ≡ foldl1 xor bl
        , property $ \bl → (¬) (null bl) ⟹ B.foldl1' xor (B.pack bl ∷ BitL) ≡ foldl1' xor bl
        , property $ \(n, bl) → B.foldr (flip doubleIf) n (B.pack bl ∷ BitL) ≡ foldr (flip doubleIf) n bl
        , property $ \bl → (¬) (null bl) ⟹ B.foldr1 xor (B.pack bl ∷ BitL) ≡ foldr1 xor bl

        , property $ \(n, bl) → B.foldl doubleIf n (B.pack bl ∷ BitR) ≡ foldl doubleIf n bl
        , property $ \(n, bl) → B.foldl' doubleIf n (B.pack bl ∷ BitR) ≡ foldl doubleIf n bl
        , property $ \bl → (¬) (null bl) ⟹ B.foldl1 xor (B.pack bl ∷ BitR) ≡ foldl1 xor bl
        , property $ \bl → (¬) (null bl) ⟹ B.foldl1' xor (B.pack bl ∷ BitR) ≡ foldl1' xor bl
        , property $ \(n, bl) → B.foldr (flip doubleIf) n (B.pack bl ∷ BitR) ≡ foldr (flip doubleIf) n bl
        , property $ \bl → (¬) (null bl) ⟹ B.foldr1 xor (B.pack bl ∷ BitR) ≡ foldr1 xor bl

          -- special folds
        , property $ \bls → B.concat (map B.pack bls ∷ [BitL]) ≡ B.pack (concat bls)
        , property $ \bl → let f True  = [True , True , True ]
                               f False = [False, False, False]
                           in B.concatMap (B.pack ∘ f) (B.pack bl ∷ BitL) ≡ B.pack (concatMap f bl)
        , property $ \bl → B.and (B.pack bl ∷ BitL) ≡ and bl
        , property $ \bl → B.or  (B.pack bl ∷ BitL) ≡ or  bl
        , property $ \bl → B.any id (B.pack bl ∷ BitL) ≡ any id bl
        , property $ \bl → B.all id (B.pack bl ∷ BitL) ≡ all id bl

        , property $ \bls → B.concat (map B.pack bls ∷ [BitR]) ≡ B.pack (concat bls)
        , property $ \bl → let f True  = [True , True , True ]
                               f False = [False, False, False]
                           in B.concatMap (B.pack ∘ f) (B.pack bl ∷ BitR) ≡ B.pack (concatMap f bl)
        , property $ \bl → B.and (B.pack bl ∷ BitR) ≡ and bl
        , property $ \bl → B.or  (B.pack bl ∷ BitR) ≡ or  bl
        , property $ \bl → B.any id (B.pack bl ∷ BitR) ≡ any id bl
        , property $ \bl → B.all id (B.pack bl ∷ BitR) ≡ all id bl

          -- scans
        , property $ \(b, bl) → B.scanl xor b (B.pack bl ∷ BitL) ≡ B.pack (scanl xor b bl)
        , property $ \bl → B.scanl1 xor (B.pack bl ∷ BitL) ≡ B.pack (scanl1 xor bl)
        , property $ \(b, bl) → B.scanr xor b (B.pack bl ∷ BitL) ≡ B.pack (scanr xor b bl)
        , property $ \bl → B.scanr1 xor (B.pack bl ∷ BitL) ≡ B.pack (scanr1 xor bl)

        , property $ \(b, bl) → B.scanl xor b (B.pack bl ∷ BitR) ≡ B.pack (scanl xor b bl)
        , property $ \bl → B.scanl1 xor (B.pack bl ∷ BitR) ≡ B.pack (scanl1 xor bl)
        , property $ \(b, bl) → B.scanr xor b (B.pack bl ∷ BitR) ≡ B.pack (scanr xor b bl)
        , property $ \bl → B.scanr1 xor (B.pack bl ∷ BitR) ≡ B.pack (scanr1 xor bl)

          -- replications
        , property $ \(n, b) → B.take (n `rem` 800) (B.iterate (¬) b ∷ BitL) ≡ B.pack (take (n `rem` 800) (iterate (¬) b))
        , property $ \(n, b) → B.take (n `rem` 800) (B.repeat b ∷ BitL) ≡ B.pack (take (n `rem` 800) (repeat b))
        , property $ \(n, b) → (B.replicate (n `rem` 800) b ∷ BitL) ≡ B.pack (replicate (n `rem` 800) b)
        , property $ \(n, bl) → (¬) (null bl) ⟹
                         B.take (n `rem` 800) (B.cycle (B.pack bl ∷ BitL)) ≡ B.pack (take (n `rem` 800) (cycle bl))

        , property $ \(n, b) → B.take (n `rem` 800) (B.iterate (¬) b ∷ BitR) ≡ B.pack (take (n `rem` 800) (iterate (¬) b))
        , property $ \(n, b) → B.take (n `rem` 800) (B.repeat b ∷ BitR) ≡ B.pack (take (n `rem` 800) (repeat b))
        , property $ \(n, b) → (B.replicate (n `rem` 800) b ∷ BitR) ≡ B.pack (replicate (n `rem` 800) b)
        , property $ \(n, bl) → (¬) (null bl) ⟹
                       B.take (n `rem` 800) (B.cycle (B.pack bl ∷ BitR)) ≡ B.pack (take (n `rem` 800) (cycle bl))

          -- unfolding
        , property $ \n → (B.unfoldr decr (abs (n `rem` 800)) ∷ BitL) ≡ B.pack (unfoldr decr (abs (n `rem` 800)))
        , property $ \(m, n) → let m'            = m `rem` 800
                                   n'            = abs (n `rem` 800)
                                   r             = B.unfoldrN m' decr n'
                                   p | m' ≤ 0    = label "m ≤ 0"     $ r ≡ ((B.∅) ∷ BitL)
                                     | m' ≤ n'   = label "m ≤ n'"    $ r ≡ B.pack (take m' (unfoldr decr n'))
                                     | otherwise = label "otherwise" $ r ≡ B.pack (unfoldr decr n')
                               in p

        , property $ \n → (B.unfoldr decr (abs (n `rem` 800)) ∷ BitR) ≡ B.pack (unfoldr decr (abs (n `rem` 800)))
        , property $ \(m, n) → let m'            = m `rem` 800
                                   n'            = abs (n `rem` 800)
                                   r             = B.unfoldrN m' decr n'
                                   p | m' ≤ 0    = label "m ≤ 0"     $ r ≡ ((B.∅) ∷ BitR)
                                     | m' ≤ n'   = label "m ≤ n'"    $ r ≡ B.pack (take m' (unfoldr decr n'))
                                     | otherwise = label "otherwise" $ r ≡ B.pack (unfoldr decr n')
                               in p

          -- substreams
        , property $ \(n, bl) → B.take n (B.pack bl ∷ BitL) ≡ B.pack (take n bl)
        , property $ \(n, bl) → B.drop n (B.pack bl ∷ BitL) ≡ B.pack (drop n bl)
        , property $ \bl → B.takeWhile id (B.pack bl ∷ BitL) ≡ B.pack (takeWhile id bl)
        , property $ \bl → B.dropWhile id (B.pack bl ∷ BitL) ≡ B.pack (dropWhile id bl)
        , property $ \bl → B.span id (B.pack bl ∷ BitL) ≡ fmapT2 B.pack (span id bl)
        , property $ \bl → B.break id (B.pack bl ∷ BitL) ≡ fmapT2 B.pack (break id bl)

        , property $ \(n, bl) → B.take n (B.pack bl ∷ BitR) ≡ B.pack (take n bl)
        , property $ \(n, bl) → B.drop n (B.pack bl ∷ BitR) ≡ B.pack (drop n bl)
        , property $ \bl → B.takeWhile id (B.pack bl ∷ BitR) ≡ B.pack (takeWhile id bl)
        , property $ \bl → B.dropWhile id (B.pack bl ∷ BitR) ≡ B.pack (dropWhile id bl)
        , property $ \bl → B.span id (B.pack bl ∷ BitR) ≡ fmapT2 B.pack (span id bl)
        , property $ \bl → B.break id (B.pack bl ∷ BitR) ≡ fmapT2 B.pack (break id bl)

          -- searching by equality
        , property $ \(b, bl) → (b B.∈ (B.pack bl ∷ BitL)) ≡ (b ∈ bl)
        , property $ \(b, bl) → (b B.∉ (B.pack bl ∷ BitL)) ≡ (b ∉ bl)

        , property $ \(b, bl) → (b B.∈ (B.pack bl ∷ BitR)) ≡ (b ∈ bl)
        , property $ \(b, bl) → (b B.∉ (B.pack bl ∷ BitR)) ≡ (b ∉ bl)

          -- searching with a predicate
        , property $ \bl → B.find id (B.pack bl ∷ BitL) ≡ find id bl
        , property $ \bl → B.filter id (B.pack bl ∷ BitL) ≡ B.pack (filter id bl)
        , property $ \bl → B.partition id (B.pack bl ∷ BitL) ≡ fmapT2 B.pack (partition id bl)

        , property $ \bl → B.find id (B.pack bl ∷ BitR) ≡ find id bl
        , property $ \bl → B.filter id (B.pack bl ∷ BitR) ≡ B.pack (filter id bl)
        , property $ \bl → B.partition id (B.pack bl ∷ BitR) ≡ fmapT2 B.pack (partition id bl)

          -- indexing streams
        , property $ \bl → (¬) (null bl) ⟹
                             let ig = choose (0, length bl - 1)
                                 bs = B.pack bl ∷ BitL
                             in forAll ig $ \ i → bs B.!! i ≡ bl !! i
        , property $ \(b, bl) → B.elemIndex b (B.pack bl ∷ BitL) ≡ elemIndex b bl
        , property $ \(b, bl) → B.elemIndices b (B.pack bl ∷ BitL) ≡ elemIndices b bl

        , property $ \bl → (¬) (null bl) ⟹
                               let ig = choose (0, length bl - 1)
                                   bs = B.pack bl ∷ BitR
                               in forAll ig $ \ i → bs B.!! i ≡ bl !! i
        , property $ \(b, bl) → B.elemIndex b (B.pack bl ∷ BitR) ≡ elemIndex b bl
        , property $ \(b, bl) → B.elemIndices b (B.pack bl ∷ BitR) ≡ elemIndices b bl

          -- zipping and unzipping streams
        , property $ \(bl1, bl2)
                       → B.zip (B.pack bl1 ∷ BitL) (B.pack bl2)
                              ≡ zip bl1 bl2
        , property $ \(bl1, bl2, bl3)
                       → B.zip3 (B.pack bl1 ∷ BitL) (B.pack bl2) (B.pack bl3)
                              ≡ zip3 bl1 bl2 bl3
        , property $ \(bl1, bl2, bl3, bl4)
                       → B.zip4 (B.pack bl1 ∷ BitL) (B.pack bl2) (B.pack bl3) (B.pack bl4)
                              ≡ zip4 bl1 bl2 bl3 bl4
        , property $ \(bl1, bl2, bl3, bl4, bl5)
                       → B.zip5 (B.pack bl1 ∷ BitL) (B.pack bl2) (B.pack bl3) (B.pack bl4) (B.pack bl5)
                              ≡ zip5 bl1 bl2 bl3 bl4 bl5
        , property $ \(bl1, bl2, bl3, bl4, bl5, bl6)
                       → B.zip6 (B.pack bl1 ∷ BitL) (B.pack bl2) (B.pack bl3) (B.pack bl4) (B.pack bl5) (B.pack bl6)
                              ≡ zip6 bl1 bl2 bl3 bl4 bl5 bl6

        , property $ \(bl1, bl2)
                       → B.zip (B.pack bl1 ∷ BitR) (B.pack bl2)
                              ≡ zip bl1 bl2
        , property $ \(bl1, bl2, bl3)
                       → B.zip3 (B.pack bl1 ∷ BitR) (B.pack bl2) (B.pack bl3)
                              ≡ zip3 bl1 bl2 bl3
        , property $ \(bl1, bl2, bl3, bl4)
                       → B.zip4 (B.pack bl1 ∷ BitR) (B.pack bl2) (B.pack bl3) (B.pack bl4)
                              ≡ zip4 bl1 bl2 bl3 bl4
        , property $ \(bl1, bl2, bl3, bl4, bl5)
                       → B.zip5 (B.pack bl1 ∷ BitR) (B.pack bl2) (B.pack bl3) (B.pack bl4) (B.pack bl5)
                              ≡ zip5 bl1 bl2 bl3 bl4 bl5
        , property $ \(bl1, bl2, bl3, bl4, bl5, bl6)
                       → B.zip6 (B.pack bl1 ∷ BitR) (B.pack bl2) (B.pack bl3) (B.pack bl4) (B.pack bl5) (B.pack bl6)
                              ≡ zip6 bl1 bl2 bl3 bl4 bl5 bl6
        ]
