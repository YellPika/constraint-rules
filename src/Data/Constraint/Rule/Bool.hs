{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnicodeSyntax         #-}

module Data.Constraint.Rule.Bool where

import Data.Constraint      (Dict (..), HasDict (..))
import Data.Constraint.Rule (RuleUsage (..))
import Data.Type.Bool       (If, Not, type (&&), type (||))
import Data.Type.Equality   (TestEquality (..), type (==), (:~:) (..))
import Unsafe.Coerce        (unsafeCoerce)

data IsBool b where
  IsTrue  ∷ IsBool 'True
  IsFalse ∷ IsBool 'False

deriving instance Eq   (IsBool b)
deriving instance Ord  (IsBool b)
deriving instance Show (IsBool b)

instance TestEquality IsBool where
  testEquality IsTrue  IsTrue  = Just Refl
  testEquality IsFalse IsFalse = Just Refl
  testEquality _       _       = Nothing

instance HasDict (KnownBool a) (IsBool a) where
  evidence IsTrue  = Dict
  evidence IsFalse = Dict

class    KnownBool b      where isBool ∷ IsBool b
instance KnownBool 'True  where isBool = IsTrue
instance KnownBool 'False where isBool = IsFalse

{-# ANN ifConstraint Intro #-}
ifConstraint ∷ ∀b p q. (KnownBool b, b ~ 'True ⇒ p, b ~ 'False ⇒ q) ⇒ Dict (If b p q)
ifConstraint =
  case isBool @b of
    IsTrue  → Dict
    IsFalse → Dict

{-# ANN ifBool Simpl #-}
ifBool ∷ Dict (KnownBool (If b m n) ~ If b (KnownBool m) (KnownBool n))
ifBool = unsafeCoerce (Dict ∷ Dict (a ~ a))

{-# ANN andBool Intro #-}
andBool ∷ ∀b c. (KnownBool b, KnownBool c) ⇒ Dict (KnownBool (b && c))
andBool =
  case isBool @b of
    IsTrue  → Dict
    IsFalse → Dict

{-# ANN orBool Intro #-}
orBool ∷ ∀b c. (KnownBool b, KnownBool c) ⇒ Dict (KnownBool (b || c))
orBool =
  case isBool @b of
    IsTrue  → Dict
    IsFalse → Dict

{-# ANN notBool Intro #-}
notBool ∷ ∀b. KnownBool b ⇒ Dict (KnownBool (Not b))
notBool =
  case isBool @b of
    IsTrue  → Dict
    IsFalse → Dict

{-# ANN equalBool Intro #-}
equalBool ∷ ∀b c. (KnownBool b, KnownBool c) ⇒ Dict (KnownBool (b == c))
equalBool =
  case (isBool @b, isBool @c) of
    (IsTrue,  IsTrue)  → Dict
    (IsFalse, IsTrue)  → Dict
    (IsTrue,  IsFalse) → Dict
    (IsFalse, IsFalse) → Dict
