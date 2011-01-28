{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , ScopedTypeVariables
  , UndecidableInstances
  , UnicodeSyntax
  #-}
module Main where
import Control.Monad
import Data.Bitstream (Bitstream, Left)
import qualified Data.Bitstream.Generic as G
import Data.Bitstream.Packet
import qualified Data.Bitstream as B
import qualified Data.ByteString as BS
import Data.ByteString.Char8 ()
import qualified Data.Stream as S
import Data.Word
import Prelude.Unicode
import System.Exit
import Test.QuickCheck

infixr 0 ⟹
infixr 1 .∧., .∨.

(⟹) :: Testable α => Bool -> α -> Property
(⟹) = (==>)

(.∧.) ∷ (Testable α, Testable β) ⇒ α → β → Property
(.∧.) = (.&&.)

(.∨.) ∷ (Testable α, Testable β) ⇒ α → β → Property
(.∨.) = (.||.)

main ∷ IO ()
main = mapM_ run tests
    where
      run ∷ Property → IO ()
      run prop
          = do r ← quickCheckResult prop
               case r of
                 Success {}           → return ()
                 GaveUp  {}           → exitFailure
                 Failure {}           → exitFailure
                 NoExpectedFailure {} → exitFailure

instance G.Bitstream (Packet d) ⇒ Arbitrary (Bitstream d) where
    arbitrary = sized $ \ n →
                do xs ← replicateM n arbitrary
                   return (B.pack xs)

instance Arbitrary BS.ByteString where
    arbitrary = sized $ \ n →
                do xs ← replicateM n arbitrary
                   return (BS.unfoldr uncons xs)
        where
          uncons ∷ [Word8] → Maybe (Word8, [Word8])
          uncons []     = Nothing
          uncons (x:xs) = Just (x, xs)

type BitL = Bitstream Left
type BitR = Bitstream Right

tests ∷ [Property]
tests = [ -- ∅
          conjoin
          [ property $ B.null      ((B.∅) ∷ BitL)
          , property $ B.length    ((B.∅) ∷ BitL) ≡ 0
          , property $ B.pack [] ≡ ((B.∅) ∷ BitL)
          , property $ B.empty   ≡ ((B.∅) ∷ BitL)
          ]

        , conjoin
          [ property $ B.null      ((B.∅) ∷ BitR)
          , property $ B.length    ((B.∅) ∷ BitR) ≡ 0
          , property $ B.pack [] ≡ ((B.∅) ∷ BitR)
          , property $ B.empty   ≡ ((B.∅) ∷ BitR)
          ]

          -- singleton
        , property $ \b → B.length (B.singleton b ∷ BitL) ≡ 1
        , property $ \b → B.head   (B.singleton b ∷ BitL) ≡ b
        , property $ \b → B.pack [b] ≡ (B.singleton b ∷ BitL)

        , property $ \b → B.length (B.singleton b ∷ BitR) ≡ 1
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
        , mapSize (⋅ 8) $ \bs → (B.length bs `mod` 8) ≡ 0
                                  ⟹ B.fromByteString (B.toByteString (bs ∷ BitL)) ≡ bs

        , property $ (B.fromByteString "UNK" ∷ BitR)
                       ≡ B.pack (map n2b [ 0, 1, 0, 1, 0, 1, 0, 1
                                         , 0, 1, 0, 0, 1, 1, 1, 0
                                         , 0, 1, 0, 0, 1, 0, 1, 1 ])
        , property $ \str → B.toByteString (B.fromByteString str ∷ BitR) ≡ str
        , mapSize (⋅ 8) $ \bs → (B.length bs `mod` 8) ≡ 0
                                  ⟹ B.fromByteString (B.toByteString (bs ∷ BitR)) ≡ bs

          -- stream/unstream
        , property $ \bl → B.unstream (S.stream bl) ≡ (B.pack bl ∷ BitL)
        , property $ \bs → S.unstream (B.stream bs) ≡ (B.unpack (bs ∷ BitL))

        , property $ \bl → B.unstream (S.stream bl) ≡ (B.pack bl ∷ BitR)
        , property $ \bs → S.unstream (B.stream bs) ≡ (B.unpack (bs ∷ BitR))

          -- direction
        , conjoin
          [ property $ B.toByteString (B.directionLToR (B.pack (map n2b [1,1,0,1,0,0,1,0, 1,0,0])))
                         ≡ BS.pack [0x4B, 0x20]
          , property $ B.toByteString (B.directionRToL (B.pack (map n2b [1,1,0,1,0,0,1,0, 1,0,0])))
                         ≡ BS.pack [0xD2, 0x04]
          ]
        , property $ \bs → B.directionRToL (B.directionLToR bs) ≡ bs
        , property $ \bs → B.directionLToR (B.directionRToL bs) ≡ bs
        , property $ \str → B.toByteString (B.directionLToR (B.fromByteString str)) ≡ str
        , property $ \str → B.toByteString (B.directionRToL (B.fromByteString str)) ≡ str

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
        ]

n2b ∷ Int → Bool
n2b 0 = False
n2b 1 = True
n2b _ = (⊥)
