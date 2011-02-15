{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , ScopedTypeVariables
  , UndecidableInstances
  , UnicodeSyntax
  #-}
module Test.Bitstream.Utils where
import Control.Monad
import qualified Data.Bitstream as SB
import qualified Data.Bitstream.Generic as G
import qualified Data.Bitstream.Lazy as LB
import Data.Bitstream.Packet
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LS
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

uncons ∷ [α] → Maybe (α, [α])
uncons []     = Nothing
uncons (α:αs) = Just (α, αs)

instance G.Bitstream (Packet d) ⇒ Arbitrary (SB.Bitstream d) where
    arbitrary = sized $ \ n →
                do xs ← replicateM n arbitrary
                   return (SB.pack xs)

instance G.Bitstream (Packet d) ⇒ Arbitrary (LB.Bitstream d) where
    arbitrary = sized $ \ n →
                do xs ← replicateM n arbitrary
                   return (LB.pack xs)

instance Arbitrary BS.ByteString where
    arbitrary = sized $ \ n →
                do xs ← replicateM n arbitrary
                   return (BS.unfoldr uncons xs)

instance Arbitrary LS.ByteString where
    arbitrary = sized $ \ n →
                do xs ← replicateM n arbitrary
                   return (LS.unfoldr uncons xs)

instance ( Arbitrary α, Arbitrary β, Arbitrary γ
         , Arbitrary δ, Arbitrary ε, Arbitrary ζ
         )
         ⇒ Arbitrary (α, β, γ, δ, ε, ζ) where
    arbitrary = do α ← arbitrary
                   β ← arbitrary
                   γ ← arbitrary
                   δ ← arbitrary
                   ε ← arbitrary
                   ζ ← arbitrary
                   return (α, β, γ, δ, ε, ζ)

instance ( Arbitrary α, Arbitrary β, Arbitrary γ
         , Arbitrary δ, Arbitrary ε, Arbitrary ζ
         , Arbitrary η
         )
         ⇒ Arbitrary (α, β, γ, δ, ε, ζ, η) where
    arbitrary = do α ← arbitrary
                   β ← arbitrary
                   γ ← arbitrary
                   δ ← arbitrary
                   ε ← arbitrary
                   ζ ← arbitrary
                   η ← arbitrary
                   return (α, β, γ, δ, ε, ζ, η)

runTest ∷ Property → IO ()
runTest prop
    = do r ← quickCheckResult prop
         case r of
           Success {}           → return ()
           GaveUp  {}           → exitFailure
           Failure {}           → exitFailure
           NoExpectedFailure {} → exitFailure

n2b ∷ Int → Bool
n2b 0 = False
n2b 1 = True
n2b _ = (⊥)

doubleIf ∷ Int → Bool → Int
doubleIf n True  = n ⋅ 2
doubleIf n False = n

doubleIf' ∷ Int → Bool → (Int, Bool)
doubleIf' n True  = (n ⋅ 2, False)
doubleIf' n False = (n    , True )

decr ∷ Int → Maybe (Bool, Int)
decr 0 = Nothing
decr n = Just (n `mod` 2 ≡ 0, n-1)

xor ∷ Bool → Bool → Bool
xor False False = False
xor True  True  = False
xor _     _     = True

fmapT2 ∷ (a → b) → (a, a) → (b, b)
fmapT2 f (x, y) = (f x, f y)
