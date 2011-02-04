{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , ScopedTypeVariables
  , UndecidableInstances
  , UnicodeSyntax
  #-}
module Main where
import Data.Bitstream.Lazy (Bitstream, Left, Right)
import Data.ByteString.Lazy.Char8 ()
import qualified Data.Bitstream.Lazy as B
import qualified Data.ByteString.Lazy as BS
import Data.List
import qualified Data.Monoid as M
import qualified Data.Monoid.Unicode as M
import qualified Data.Stream as S
import Prelude.Unicode
import Test.Bitstream.Utils
import Test.QuickCheck

main ∷ IO ()
main = mapM_ runTest tests

type BitL = Bitstream Left
type BitR = Bitstream Right

tests ∷ [Property]
tests = [ -- ∅
          {-conjoin
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
        , mapSize (⋅ 8) $ \bs → (B.length bs `mod` 8) ≡ (0 ∷ Int)
                                  ⟹ B.fromByteString (B.toByteString (bs ∷ BitL)) ≡ bs

        , property $ (B.fromByteString "UNK" ∷ BitR)
                       ≡ B.pack (map n2b [ 0, 1, 0, 1, 0, 1, 0, 1
                                         , 0, 1, 0, 0, 1, 1, 1, 0
                                         , 0, 1, 0, 0, 1, 0, 1, 1 ])
        , property $ \str → B.toByteString (B.fromByteString str ∷ BitR) ≡ str
        , mapSize (⋅ 8) $ \bs → (B.length bs `mod` 8) ≡ (0 ∷ Int)
                                  ⟹ B.fromByteString (B.toByteString (bs ∷ BitR)) ≡ bs

          -- stream/unstream
        , property $ \bl → B.unstream (S.stream bl) ≡ (B.pack bl ∷ BitL)
        , property $ \bs → S.unstream (B.stream bs) ≡ (B.unpack (bs ∷ BitL))

        , property $ \bl → B.unstream (S.stream bl) ≡ (B.pack bl ∷ BitR)
        , property $ \bs → S.unstream (B.stream bs) ≡ (B.unpack (bs ∷ BitR))

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
                       ≡ "(L[00011001←][1110101←])"
          , property $ show (B.pack [ True , False, False, True, True
                                    , False, False, False, True, False
                                    , True , False, True , True, True  ] ∷ BitR)
                       ≡ "(L[→10011000][→1010111])"
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
        , property $ \(bl, b) → B.snoc (B.pack bl ∷ BitL) b ≡ B.pack (bl ⧺ [b])
        , property $ \(x, y) → (B.pack x ∷ BitL) B.⧺ (B.pack y) ≡ B.pack (x ⧺ y)
        , property $ \bl → (¬) (null bl) ⟹ B.head (B.pack bl ∷ BitL) ≡ head bl
        , property $ \bl → let uc = B.uncons (B.pack bl ∷ BitL)
                           in case bl of
                                []     → label "null"     $ uc ≡ Nothing
                                (x:xs) → label "non-null" $ uc ≡ Just (x, B.pack xs)
        , property $ \bl → (¬) (null bl) ⟹ B.last (B.pack bl ∷ BitL) ≡ last bl
        , property $ \bl → (¬) (null bl) ⟹ B.tail (B.pack bl ∷ BitL) ≡ B.pack (tail bl)
        , property $ \bl → (¬) (null bl) ⟹ B.init (B.pack bl ∷ BitL) ≡ B.pack (init bl)
        , property $ \bl → let bs = B.pack bl ∷ BitL
                           in case bl of
                                [] → label "null"     $ B.null bs
                                _  → label "non-null" $ (¬) (B.null bs)
        , property $ \bl → B.length (B.pack bl ∷ BitL) ≡ length bl

        , property $ \(b, bl) → B.cons b (B.pack bl ∷ BitR) ≡ B.pack (b:bl)
        , property $ \(bl, b) → B.snoc (B.pack bl ∷ BitR) b ≡ B.pack (bl ⧺ [b])
        , property $ \(x, y) → (B.pack x ∷ BitR) B.⧺ (B.pack y) ≡ B.pack (x ⧺ y)
        , property $ \bl → (¬) (null bl) ⟹ B.head (B.pack bl ∷ BitR) ≡ head bl
        , property $ \bl → let uc = B.uncons (B.pack bl ∷ BitR)
                           in case bl of
                                []     → label "null"     $ uc ≡ Nothing
                                (x:xs) → label "non-null" $ uc ≡ Just (x, B.pack xs)
        , property $ \bl → (¬) (null bl) ⟹ B.last (B.pack bl ∷ BitR) ≡ last bl
        , property $ \bl → (¬) (null bl) ⟹ B.tail (B.pack bl ∷ BitR) ≡ B.pack (tail bl)
        , property $ \bl → (¬) (null bl) ⟹ B.init (B.pack bl ∷ BitR) ≡ B.pack (init bl)
        , property $ \bl → let bs = B.pack bl ∷ BitR
                           in case bl of
                                [] → label "null"     $ B.null bs
                                _  → label "non-null" $ (¬) (B.null bs)
        , property $ \bl → B.length (B.pack bl ∷ BitR) ≡ length bl

          -- transformation
        , -}property $ \bl → B.map (¬) (B.pack bl ∷ BitL) ≡ B.pack (map (¬) bl)
        , property $ \bl → B.reverse (B.pack bl ∷ BitL) ≡ B.pack (reverse bl)
        , property $ \(bl, b) → B.intersperse b (B.pack bl ∷ BitL) ≡ B.pack (intersperse b bl)
        , property $ \(bl, bls) → B.intercalate (B.pack bl ∷ BitL) (map B.pack bls) ≡ B.pack (intercalate bl bls)
        , property $ let rows  = sized $ \n → listOf (row n)
                         row n = n `vectorOf` arbitrary
                     in forAll rows
                        $ \bls → B.transpose (map B.pack bls ∷ [BitL]) ≡ map B.pack (transpose bls)

        , property $ \bl → B.map (¬) (B.pack bl ∷ BitR) ≡ B.pack (map (¬) bl)
        , property $ \bl → B.reverse (B.pack bl ∷ BitL) ≡ B.pack (reverse bl)
        , property $ \(bl, b) → B.intersperse b (B.pack bl ∷ BitR) ≡ B.pack (intersperse b bl)
        , property $ \(bl, bls) → B.intercalate (B.pack bl ∷ BitR) (map B.pack bls) ≡ B.pack (intercalate bl bls)
        , property $ let rows  = sized $ \n → listOf (row n)
                         row n = n `vectorOf` arbitrary
                     in forAll rows
                        $ \bls → B.transpose (map B.pack bls ∷ [BitR]) ≡ map B.pack (transpose bls)
        ]
