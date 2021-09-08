{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MonoLocalBinds            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UnicodeSyntax             #-}

module Data.Constraint.Rule (
  RuleUsage (..), RuleArg (..), RuleName (..), RuleSpec (..),
  Use, Ignore, Intro, Deriv, Simpl, NoIntro, NoDeriv, NoSimpl,
  withIntro, withDeriv, withSimpl,
  ignoreIntro, ignoreDeriv, ignoreSimpl
) where

import Data.Constraint.Rule.Plugin.Prelude

import Data.Class.Closed.TH (close)
import Data.Data            (Data, Proxy)
import GHC.TypeLits         (Symbol)

data RuleUsage = Intro | Deriv | Simpl
  deriving (Data, Eq, Ord, Show)

instance Outputable RuleUsage where
  ppr = text . show

data RuleArg  = ∀a. RuleArg a
data RuleName = RuleName Symbol Symbol
data RuleSpec = RuleSpec RuleName [RuleArg]

close [d|
  class     Use    (ruleUsage ∷ RuleUsage) (ruleSpec ∷ RuleSpec)
  instance  Use    (ruleUsage ∷ RuleUsage) (ruleSpec ∷ RuleSpec)
  class     Ignore (ruleUsage ∷ RuleUsage) (ruleName ∷ RuleName)
  instance  Ignore (ruleUsage ∷ RuleUsage) (ruleName ∷ RuleName)
 |]

type Intro = Use 'Intro
type Deriv = Use 'Deriv
type Simpl = Use 'Simpl
type NoIntro = Ignore 'Intro
type NoDeriv = Ignore 'Deriv
type NoSimpl = Ignore 'Simpl

withIntro ∷ Proxy a → (Intro a ⇒ r) → r
withIntro _ x = x

withDeriv ∷ Proxy a → (Deriv a ⇒ r) → r
withDeriv _ x = x

withSimpl ∷ Proxy a → (Simpl a ⇒ r) → r
withSimpl _ x = x

ignoreIntro ∷ Proxy a → (NoIntro a ⇒ r) → r
ignoreIntro _ x = x

ignoreDeriv ∷ Proxy a → (NoDeriv a ⇒ r) → r
ignoreDeriv _ x = x

ignoreSimpl ∷ Proxy a → (NoSimpl a ⇒ r) → r
ignoreSimpl _ x = x
