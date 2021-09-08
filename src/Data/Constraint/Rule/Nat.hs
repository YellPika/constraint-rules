{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NoStarIsType          #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnicodeSyntax         #-}

module Data.Constraint.Rule.Nat where

import           Data.Constraint           (Dict (..), evidence)
import           Data.Constraint.Nat       (Gcd, Lcm)
import qualified Data.Constraint.Nat       as Nat
import           Data.Constraint.Rule      (RuleUsage (..))
import           Data.Constraint.Rule.Bool (IsBool (..), KnownBool)
import           Data.Proxy                (Proxy (..))
import           Data.Type.Bool            (If)
import           Data.Type.Equality        (type (==))
import           GHC.TypeNats              (Div, KnownNat, Mod, natVal,
                                            type (*), type (+), type (-),
                                            type (<=), type (<=?))
import           Unsafe.Coerce             (unsafeCoerce)

{-# ANN plusNat Intro #-}
plusNat ∷ ∀m n. (KnownNat m, KnownNat n) ⇒ Dict (KnownNat (m + n))
plusNat = evidence (Nat.plusNat @m @n)

{-# ANN minusNat Intro #-}
minusNat ∷ ∀m n. (KnownNat m, KnownNat n, n <= m) ⇒ Dict (KnownNat (m - n))
minusNat = evidence (Nat.minusNat @m @n)

{-# ANN timesNat Intro #-}
timesNat ∷ ∀m n. (KnownNat m, KnownNat n) ⇒ Dict (KnownNat (m * n))
timesNat = evidence (Nat.timesNat @m @n)

{-# ANN divNat Intro #-}
divNat ∷ ∀m n. (KnownNat m, KnownNat n, 1 <= n) ⇒ Dict (KnownNat (Div m n))
divNat = evidence (Nat.divNat @m @n)

{-# ANN modNat Intro #-}
modNat ∷ ∀m n. (KnownNat m, KnownNat n, 1 <= n) ⇒ Dict (KnownNat (Mod m n))
modNat = evidence (Nat.modNat @m @n)

{-# ANN lcmNat Intro #-}
lcmNat ∷ ∀m n. (KnownNat m, KnownNat n) ⇒ Dict (KnownNat (Lcm m n))
lcmNat = evidence (Nat.lcmNat @m @n)

{-# ANN gcdNat Intro #-}
gcdNat ∷ ∀m n. (KnownNat m, KnownNat n) ⇒ Dict (KnownNat (Gcd m n))
gcdNat = evidence (Nat.gcdNat @m @n)

{-# ANN ifNat Simpl #-}
ifNat ∷ Dict (KnownNat (If b m n) ~ If b (KnownNat m) (KnownNat n))
ifNat = unsafeCoerce (Dict ∷ Dict (a ~ a))

{-# ANN lessEqualNat Intro #-}
lessEqualNat ∷ ∀m n. (KnownNat m, KnownNat n) ⇒ Dict (KnownBool (m <=? n))
lessEqualNat =
  if natVal @m Proxy <= natVal @n Proxy
  then unsafeCoerce (evidence IsTrue)
  else unsafeCoerce (evidence IsFalse)

{-# ANN equalNat Intro #-}
equalNat ∷ ∀m n. (KnownNat m, KnownNat n) ⇒ Dict (KnownBool (m == n))
equalNat =
  if natVal @m Proxy == natVal @n Proxy
  then unsafeCoerce (evidence IsTrue)
  else unsafeCoerce (evidence IsFalse)
