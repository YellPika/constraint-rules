{-# OPTIONS_GHC -fdefer-type-errors                  #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors            #-}
{-# OPTIONS_GHC -fplugin=Data.Constraint.Rule.Plugin #-}
{-# OPTIONS_GHC -dcore-lint                          #-}
-- {-# OPTIONS_GHC -ddump-tc-trace -ddump-to-file       #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Test.SimplDefs where

import           Data.Constraint         (Dict (..), withDict)
import           Data.Constraint.Rule    (withSimpl)
import qualified Data.Constraint.Rule.TH as TH
import           Test.Util               (badProof, testEq, trustMe)

type family Foo a
type family Bar a

withEq ∷ ∀a b. Eq a ⇒ (Eq (Foo a) ⇒ b) → b
withEq = withDict (proof₂ @a)

proof₁ ∷ Dict (Foo a ~ a)
proof₁ = badProof

proof₂ ∷ Dict (Foo a ~ a)
proof₂ = trustMe

proof₃ ∷ Dict (Bar a ~ a)
proof₃ = trustMe

wanted₁ ∷ Maybe Int
wanted₁ = Nothing @(Foo Int)

wanted₂ ∷ Maybe Int
wanted₂ = withSimpl $(TH.spec 'proof₁) (Nothing @(Foo Int))

wanted₃ ∷ Maybe Int
wanted₃ = withSimpl $(TH.spec 'proof₂) (Nothing @(Foo Int))

wanted₄ ∷ ()
wanted₄ = testEq @(Foo Int) Dict 0

wanted₅ ∷ ()
wanted₅ = withSimpl $(TH.spec 'proof₁) (testEq @(Foo Int) Dict 0)

wanted₆ ∷ ()
wanted₆ = withSimpl $(TH.spec 'proof₂) (testEq @(Foo Int) Dict 0)

given₁ ∷ ()
given₁ = withEq @Int (testEq @Int go 0) where
  go ∷ Eq (Foo a) ⇒ Dict (Eq a)
  go = Dict

given₂ ∷ ()
given₂ = withEq @Int (testEq @Int go 0) where
  go ∷ Eq (Foo a) ⇒ Dict (Eq a)
  go = withSimpl $(TH.spec 'proof₁) Dict

given₃ ∷ ()
given₃ = withEq @Int (testEq @Int go 0) where
  go ∷ Eq (Foo a) ⇒ Dict (Eq a)
  go = withSimpl $(TH.spec 'proof₂) Dict

nested₁ ∷ ()
nested₁ = testEq @Int (Dict @(Eq (Foo (Foo (Foo Int))))) 0

nested₂ ∷ ()
nested₂ = withSimpl $(TH.spec 'proof₂) $
  testEq @Int (Dict @(Eq (Foo (Foo (Foo Int))))) 0

multiple₁ ∷ ()
multiple₁ = testEq @(Bar Int) (Dict @(Eq (Foo Int))) 0

multiple₂ ∷ ()
multiple₂ =
  withSimpl $(TH.spec 'proof₂) $
  withSimpl $(TH.spec 'proof₃) $
    testEq @(Bar Int) (Dict @(Eq (Foo Int))) 0
