{-# LANGUAGE
    FlexibleContexts
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
import Prelude.Unicode hiding ((⧺), (∈), (∉))
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test
{-
infixr 0 ⟹
infixr 1 .∧.

(⟹) :: Testable α => Bool -> α -> Property
(⟹) = (==>)

(.∧.) ∷ (Testable α, Testable β) ⇒ α → β → Property
(.∧.) = (.&&.)
-}

main ∷ IO ()
main = do r ← quickCheckResult tests
          --r ← verboseCheckResult tests
          if isSuccess r then
              return ()
            else
              exitFailure

instance G.Bitstream (Packet d) ⇒ Arbitrary (Bitstream d) where
    arbitrary = sized $ \ n →
                do xs ← replicateM n arbitrary
                   return (B.pack xs)

tests ∷ Property
tests = conjoin
        [ -- ∅
          property $ B.null      ((B.∅) ∷ Bitstream Left)
        , property $ B.length    ((B.∅) ∷ Bitstream Left) ≡ 0
        , property $ B.pack [] ≡ ((B.∅) ∷ Bitstream Left)
        , property $ B.empty   ≡ ((B.∅) ∷ Bitstream Left)

        , property $ B.null      ((B.∅) ∷ Bitstream Right)
        , property $ B.length    ((B.∅) ∷ Bitstream Right) ≡ 0
        , property $ B.pack [] ≡ ((B.∅) ∷ Bitstream Right)
        , property $ B.empty   ≡ ((B.∅) ∷ Bitstream Right)

          -- singleton
        , property $ \b → B.length (B.singleton b ∷ Bitstream Left) ≡ 1
        , property $ \b → B.head   (B.singleton b ∷ Bitstream Left) ≡ b
        , property $ \b → B.pack [b] ≡ (B.singleton b ∷ Bitstream Left)

        , property $ \b → B.length (B.singleton b ∷ Bitstream Right) ≡ 1
        , property $ \b → B.head   (B.singleton b ∷ Bitstream Right) ≡ b
        , property $ \b → B.pack [b] ≡ (B.singleton b ∷ Bitstream Right)

          -- pack/unpack
        , property $ B.unpack (B.pack []      ∷ Bitstream Left) ≡ []
        , property $ B.unpack (B.pack [False] ∷ Bitstream Left) ≡ [False]
        , property $ B.unpack (B.pack [True ] ∷ Bitstream Left) ≡ [True ]
        , property $ \bs → B.unpack (B.pack bs ∷ Bitstream Left) ≡ bs
        , property $ \bs → B.pack (B.unpack (bs ∷ Bitstream Left)) ≡ bs

        , property $ B.unpack (B.pack []      ∷ Bitstream Right) ≡ []
        , property $ B.unpack (B.pack [False] ∷ Bitstream Right) ≡ [False]
        , property $ B.unpack (B.pack [True ] ∷ Bitstream Right) ≡ [True ]
        , property $ \bs → B.unpack (B.pack bs ∷ Bitstream Right) ≡ bs
        , property $ \bs → B.pack (B.unpack (bs ∷ Bitstream Right)) ≡ bs
        ]
