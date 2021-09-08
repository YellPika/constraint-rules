{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax   #-}
{-# LANGUAGE ViewPatterns    #-}

module Data.Constraint.Rule.Plugin.Definitions (
  Definitions,
  findDefinitions,
  pattern DictTy,
  pattern UseTy,
  pattern IgnoreTy,
  pattern RuleUsageTy,
  pattern RuleArgTy,
  pattern RuleArgListTy,
  pattern RuleNameTy,
  pattern RuleSpecTy,
  pattern CachedTy,
  pattern CachedExpr,
  pattern OpenDictExpr,
  pattern EqPrimTy,
  pattern EqTy,
  pattern HEqTy,
  pattern NumLitTy,
  pattern StrLitTy,
  pattern ListTy
) where

import Data.Constraint.Rule
import Data.Constraint.Rule.Plugin.Prelude
import Data.Constraint.Rule.Plugin.Runtime

import Data.Constraint    (Dict (..))
import GHC.Definitions.TH (makeDefinitions, makePattern)

makeDefinitions
  [ ''Dict
  , ''Use
  , ''Ignore
  , 'Intro
  , 'Deriv
  , 'Simpl
  , 'RuleArg
  , 'RuleName
  , 'RuleSpec
  , ''Cached
  , 'cached
  , 'unsafeOpenDict
  ]

makePattern "DictTy"       'dictTyCon
makePattern "UseTy"        'useClass
makePattern "IgnoreTy"     'ignoreClass
makePattern "IntroTy"      'promotedIntroTyCon
makePattern "DerivTy"      'promotedDerivTyCon
makePattern "SimplTy"      'promotedSimplTyCon
makePattern "RuleArgTy"    'promotedRuleArgTyCon
makePattern "RuleNameTy"   'promotedRuleNameTyCon
makePattern "RuleSpecTy"   'promotedRuleSpecTyCon
makePattern "CachedTy"     'cachedClass
makePattern "CachedExpr"   'cachedVar
makePattern "OpenDictExpr" 'unsafeOpenDictVar

makePattern "EqPrimTy" 'eqPrimTyCon
makePattern "HEqTy"    'heqTyCon
makePattern "EqTy"     'eqTyCon
makePattern "NilTy"    'promotedNilDataCon
makePattern "ConsTy"   'promotedConsDataCon

pattern RuleUsageTy ∷ Definitions ⇒ RuleUsage → Type
pattern RuleUsageTy x ← (isRuleUsageTy → Just x)
  where RuleUsageTy Intro = IntroTy []
        RuleUsageTy Deriv = DerivTy []
        RuleUsageTy Simpl = SimplTy []

isRuleUsageTy ∷ Definitions ⇒ Type → Maybe RuleUsage
isRuleUsageTy (IntroTy []) = Just Intro
isRuleUsageTy (DerivTy []) = Just Deriv
isRuleUsageTy (SimplTy []) = Just Simpl
isRuleUsageTy _            = Nothing

pattern NumLitTy ∷ Integer → Type
pattern NumLitTy x ← (isNumLitTy → Just x)
  where NumLitTy x = mkNumLitTy x

pattern StrLitTy ∷ FastString → Type
pattern StrLitTy x ← (isStrLitTy → Just x)

pattern ListTy ∷ [Type] → Type
pattern ListTy xs ← (isListTy → Just xs)

isListTy ∷ Type → Maybe [Type]
isListTy (NilTy [_])         = Just []
isListTy (ConsTy [_, x, xs]) = (x:) <$> isListTy xs
isListTy _                   = Nothing

pattern RuleArgListTy ∷ Definitions ⇒ [Type] → Type
pattern RuleArgListTy xs ← (ListTy (isRuleArgListTy → Just xs))

isRuleArgListTy ∷ Definitions ⇒ [Type] → Maybe [Type]
isRuleArgListTy []                      = Just []
isRuleArgListTy (RuleArgTy [_, x] : xs) = (x:) <$> isRuleArgListTy xs
isRuleArgListTy _                       = Nothing
