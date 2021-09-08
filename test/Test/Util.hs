{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Test.Util where

import Control.Exception (Exception, throw)
import Data.Constraint   (Dict (..))
import Test.Hspec        (Selector)
import Unsafe.Coerce     (unsafeCoerce)

data BadProof = BadProof
  deriving (Exception, Show)

badProof ∷ a
badProof = throw BadProof

exc ∷ ∀a. Exception a ⇒ Selector a
exc _ = True

trustMe ∷ Dict (a ~ b)
trustMe = unsafeCoerce (Dict ∷ Dict (() ~ ()))

testEq ∷ Dict (Eq a) → a → ()
testEq Dict x = x == x `seq` ()
