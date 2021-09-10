{-# OPTIONS_GHC -fdefer-type-errors                  #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors            #-}
{-# OPTIONS_GHC -fplugin=Data.Constraint.Rule.Plugin #-}
{-# OPTIONS_GHC -dcore-lint                          #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Test.IntroDefs where

import Data.Constraint         (Dict (..), HasDict (..))
import Data.Constraint.Nat     (plusNat, timesNat)
import Data.Constraint.Rule    (withIntro)
import Data.Constraint.Rule.TH (spec)
import Data.Proxy              (Proxy (Proxy))
import GHC.TypeNats            (KnownNat, natVal, type (+), type (*))
import Numeric.Natural         (Natural)
import Test.Util               (badProof)

proof₁ ∷ (KnownNat m, KnownNat n) ⇒ Dict (KnownNat (m + n))
proof₁ = badProof

proof₂ ∷ ∀m n. (KnownNat m, KnownNat n) ⇒ Dict (KnownNat (m + n))
proof₂ = evidence (plusNat @m @n)

proof₃ ∷ ∀m n. (KnownNat m, KnownNat n) ⇒ Dict (KnownNat (m * n))
proof₃ = evidence (timesNat @m @n)

test₁ ∷ ∀x. KnownNat x ⇒ Natural
test₁ = natVal (Proxy @(x + 5))

test₂ ∷ ∀x. KnownNat x ⇒ Natural
test₂ = withIntro $(spec 'proof₁) (natVal (Proxy @(x + 5)))

test₃ ∷ ∀x. KnownNat x ⇒ Natural
test₃ = withIntro $(spec 'proof₂) (natVal (Proxy @(x + 5)))

nested₁ ∷ ∀x y. (KnownNat x, KnownNat y) ⇒ Natural
nested₁ = natVal (Proxy @(x + y + 5))

nested₂ ∷ ∀x y. (KnownNat x, KnownNat y) ⇒ Natural
nested₂ = withIntro $(spec 'proof₂) $
  natVal (Proxy @(x + y + 5))

multiple₁ ∷ ∀x y. (KnownNat x, KnownNat y) ⇒ Natural
multiple₁ = withIntro $(spec 'proof₂) $
  natVal (Proxy @(x * y + 5))

multiple₂ ∷ ∀x y. (KnownNat x, KnownNat y) ⇒ Natural
multiple₂ =
  withIntro $(spec 'proof₂) $
  withIntro $(spec 'proof₃) $
    natVal (Proxy @(x * y + 5))
