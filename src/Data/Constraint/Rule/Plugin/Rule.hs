{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module Data.Constraint.Rule.Plugin.Rule (
  Rule (..), RuleGoal (..), findRules
) where

import Data.Constraint.Rule
import Data.Constraint.Rule.Plugin.Definitions
import Data.Constraint.Rule.Plugin.Equiv
import Data.Constraint.Rule.Plugin.Message
import Data.Constraint.Rule.Plugin.Prelude hiding (empty)

import Control.Applicative       (empty)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Function             (on)
import Data.List                 (nub, sort, sortBy)
import Data.Maybe                (mapMaybe)

data Rule = Rule {
  ruleDef  ∷ Var,
  ruleArgs ∷ TCvSubst,
  ruleVars ∷ [Var],
  ruleCts  ∷ [Type],
  ruleGoal ∷ RuleGoal,
  ruleLoc  ∷ CtLoc,
  ruleElt  ∷ Maybe GlobalRdrElt
}

instance Outputable Rule where
  ppr Rule {..} =
    ppr ruleDef <+> ppr (mapMaybe (\var → (,) var <$> lookupTyVar ruleArgs var) ruleVars) <+> "∷" <+>
    "∀" <+> ppr ruleVars <+>
    "." <+> ppr ruleCts  <+>
    "⇒" <+> ppr ruleGoal

ruleName ∷ Rule → Name
ruleName = varName . ruleDef

ruleUsage ∷ Rule → RuleUsage
ruleUsage = ruleGoalKind . ruleGoal

data RuleGoal
  = IntroGoal Type
  | DerivGoal Type
  | SimplGoal Type Type

instance Outputable RuleGoal where
  ppr (IntroGoal goal)    = ppr goal                    <+> "(Intro)"
  ppr (DerivGoal goal)    = ppr goal                    <+> "(Deriv)"
  ppr (SimplGoal lhs rhs) = ppr lhs <+> "~" <+> ppr rhs <+> "(Simpl)"

ruleGoalKind ∷ RuleGoal → RuleUsage
ruleGoalKind (IntroGoal _)   = Intro
ruleGoalKind (DerivGoal _)   = Deriv
ruleGoalKind (SimplGoal _ _) = Simpl

findRules ∷ (Definitions, Equalities, Messages) ⇒ [Ct] → TcPluginM ([Rule], [Ct])
findRules givens = do
  ignored               ← findIgnored givens
  (specifieds, givens') ← findSpecifiedRules givens
  defaults              ← findDefaultRules
  let rules =
        sortBy (compare `on` ruleUsage) .
        filter (\rule → (ruleUsage rule, ruleName rule) `notElem` ignored) $
          -- Specified rules take precedence over default rules.
          specifieds ++ defaults
  return (rules, givens')

findIgnored ∷ (Definitions, Equalities, Messages) ⇒ [Ct] → TcPluginM [(RuleUsage, Name)]
findIgnored = mapMaybeM (go . ctPred) where
  go (IgnoreTy [RuleUsageTy kind, RuleNameTy [StrLitTy md, StrLitTy var]]) =
    fmap (kind,) <$> lookupName md var
  go t@(IgnoreTy _) = do
    addErrorMessage (hang "Malformatted Ignore constraint:" 4 (ppr t))
    return Nothing
  go _ = return Nothing

findSpecifiedRules ∷ (Definitions, Equalities, Messages) ⇒ [Ct] → TcPluginM ([Rule], [Ct])
findSpecifiedRules = loop where
  loop [] = return ([], [])
  loop (ct:cts) = do
    result ← go ct
    case result of
      Just rule → do
        (rules, cts') ← loop cts
        return (rule:rules, cts')
      Nothing → do
        (rules, cts') ← loop cts
        return (rules, ct:cts')

  go ct@(ctPred → t@(UseTy [RuleUsageTy kind, RuleSpecTy [RuleNameTy [StrLitTy md, StrLitTy nm], RuleArgListTy args]])) = runMaybeT do
    name ← MaybeT (lookupName md nm)
    Rule { ruleArgs = _, ..} ← MaybeT (makeRule Nothing (ctLoc ct) kind name)
    let ruleArgs = mkTvSubstPrs (zip ruleVars args)
    checkArgs emptyTCvSubst t ruleVars args
    return Rule {..}
  go (ctPred → t@(UseTy _)) = do
    addErrorMessage (hang "Malformatted Use constraint:" 4 (ppr t))
    return Nothing
  go _ = return Nothing

findDefaultRules ∷ (Definitions, Messages) ⇒ TcPluginM [Rule]
findDefaultRules = do
  topEnv ← getTopEnv
  annEnv ← tcPluginIO (prepareAnnotations topEnv Nothing)
  let getAnns ∷ Name → [RuleUsage]
      getAnns = nub . sort . findAnns deserializeWithData annEnv . NamedTarget

  (gblEnv, _) ← getEnvs

  let elts   = filter (not . isLocalGRE) (concat (occEnvElts (tcg_rdr_env gblEnv)))
      named  = map (\elt → (elt, grePrintableName elt)) elts
      annotd = concatMap (\(elt, name) → (,,) elt name <$> getAnns name) named
  tcPluginTrace "[constraint-rules] Annotations" (ppr annotd)

  locd ← mapM (\(elt, name, kind) → (,,,) elt name kind <$> getCtLocM (OccurrenceOf name) Nothing) annotd
  mapMaybeM (\(elt, name, kind, loc) → makeRule (Just elt) loc kind name) locd

makeRule ∷ (Definitions, Messages) ⇒ Maybe GlobalRdrElt → CtLoc → RuleUsage → Name → TcPluginM (Maybe Rule)
makeRule ruleElt ruleLoc kind name = runMaybeT do
  ruleDef ← MaybeT (lookupVar name)
  MaybeT . setSrcSpan (toSrcSpan (ctLocSpan ruleLoc)) . runMaybeT $ do
    (ruleVars, ruleCts, goalTy) ← parseRuleType name (varType ruleDef)
    let ruleArgs = emptyTCvSubst
    ruleGoal ← MaybeT (parseRuleGoal name kind goalTy)
    return Rule {..}

parseRuleType ∷ (Definitions, Messages) ⇒ Name → Type → MaybeT TcPluginM ([Var], [Type], Type)
parseRuleType _ (tcSplitForAllTys → (vars, tcSplitPhiTy → (cts, DictTy [t]))) = return (vars, cts, t)
parseRuleType name t = do
  lift . addErrorMessage $
    hang "Malformatted Use constraint:" 4 (ppr name <+> "∷" <+> ppr t)
  empty

parseRuleGoal ∷ (Definitions, Messages) ⇒ Name → RuleUsage → Type → TcPluginM (Maybe RuleGoal)
parseRuleGoal _ Intro ct                       = return (Just (IntroGoal ct))
parseRuleGoal _ Deriv ct                       = return (Just (DerivGoal ct))
parseRuleGoal _ Simpl (EqTy [_, lhs, rhs])     = return (Just (SimplGoal lhs rhs))
parseRuleGoal _ Simpl (HEqTy [_, _, lhs, rhs]) = return (Just (SimplGoal lhs rhs))
parseRuleGoal name kind goal = do
  addErrorMessage (hang ("Rule" <+> quotes (ppr name) <+> "has invalid" <+> ppr kind <+> "goal:") 4 (ppr goal))
  return Nothing

checkArgs ∷ (Equalities, Messages) ⇒ TCvSubst → Type → [Var] → [Type] → MaybeT TcPluginM ()
checkArgs _ _ _ [] = return ()
checkArgs _ t [] _ = do
  lift . addErrorMessage $
    hang "Too many arguments for use constraint:" 4 (ppr t)
  empty
checkArgs σ t (var:vars) (arg:args) =
  if equivClass (typeKind arg) `contains` substTyAddInScope σ (varType var)
  then checkArgs (extendTvSubstAndInScope σ var arg) t vars args
  else do
    lift . addErrorMessage $
      hang "Malformatted Use constraint:" 4 (ppr t) $$
      hang "Expected an argument of type" 4 (ppr (varType var)) $$
      hang "but" 4 (ppr arg) $$
      hang "has type" 4 (ppr (typeKind arg))
    empty

lookupVar ∷ Name → TcPluginM (Maybe Var)
lookupVar name = do
  local ← unsafeTcPluginTcM (tcLookupLcl_maybe name)
  case local of
    Just ATcId {..} → return (Just tct_id)
    _ → do
      global ← unsafeTcPluginTcM (tcLookupImported_maybe name)
      case global of
        Succeeded (AnId x) → return (Just x)
        _ → do
          tcPluginTrace "[constraint-rules]" ("Could not lookup variable" <+> ppr name)
          return Nothing

lookupName ∷ Messages ⇒ FastString → FastString → TcPluginM (Maybe Name)
lookupName mdName occ = do
  result ← findImportedModule (mkModuleNameFS mdName) Nothing
  case result of
    Found _ md → Just <$> lookupOrig md (mkVarOccFS occ)
    FoundMultiple _ → do
      addErrorMessage ("Found multiple modules named" <+> quotes (ppr mdName))
      return Nothing
    _ → do
      addErrorMessage ("Could not find a module named" <+> quotes (ppr mdName))
      return Nothing
