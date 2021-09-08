{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Data.Constraint.Rule.TH (Rule (..), Spec (spec)) where

import Data.Class.Closed.TH       (close)
import Data.Constraint.Rule       (RuleArg (..), RuleName (..), RuleSpec (..))
import Data.Proxy                 (Proxy (..))
import Language.Haskell.TH.Lib    (litT, strTyLit)
import Language.Haskell.TH.Syntax (Exp, ModName (..), Name (..),
                                   NameFlavour (..), NameSpace (..),
                                   OccName (..), Q, Type)

close [d|
  class Rule a where
    rule ∷ Name → Q a

  instance Rule Type where
    rule = ruleType

  instance Rule Exp where
    rule = ruleExp

  class Spec a where
    spec ∷ Name → a
    spec name = specHelper name []

    specHelper ∷ Name → [Q Type] → a

  instance Spec (Q Type) where
    specHelper = specType

  instance Spec (Q Exp) where
    specHelper = specExp

  instance Spec a ⇒ Spec (Q Type → a) where
    specHelper name args arg = specHelper name (arg:args)
 |]

ruleType ∷ Name → Q Type
ruleType (Name (OccName name) (NameG VarName _ (ModName md))) = [t| 'RuleName $(litT (strTyLit md)) $(litT (strTyLit name)) |]
ruleType _ = error "rule: Expected top-level function name"

ruleExp ∷ Name → Q Exp
ruleExp name = [e| Proxy @($(rule name)) |]

specType ∷ Name → [Q Type] → Q Type
specType name args = [t| 'RuleSpec $(rule name) $(list (reverse args)) |]

specExp ∷ Name → [Q Type] → Q Exp
specExp name args = [e| Proxy @($(specHelper name args)) |]

list ∷ [Q Type] → Q Type
list []     = [t| '[] |]
list (x:xs) = [t| 'RuleArg $x ': $(list xs) |]
