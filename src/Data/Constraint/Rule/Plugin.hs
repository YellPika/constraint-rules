{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ImplicitParams   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax    #-}
{-# LANGUAGE ViewPatterns     #-}

module Data.Constraint.Rule.Plugin where

import Data.Constraint.Rule.Plugin.Cache
import Data.Constraint.Rule.Plugin.Definitions
import Data.Constraint.Rule.Plugin.Equiv
import Data.Constraint.Rule.Plugin.Message
import Data.Constraint.Rule.Plugin.Prelude
import Data.Constraint.Rule.Plugin.Rule

import Control.Monad             (guard)
import Control.Monad.Trans.State (execStateT, runStateT)
import Data.Constraint           (Dict (..))
import Data.Maybe                (fromJust, isJust, maybeToList)

plugin ∷ Plugin
plugin = defaultPlugin
  { tcPlugin = \_ → Just TcPlugin
    { tcPluginInit  = (,,) <$> findDefinitions <*> newCache <*> newMessages
    , tcPluginSolve = solve
    , tcPluginStop  = \(_, _, Dict) → reportMessages
    }
  , pluginRecompile = purePlugin
  }

solve ∷ (Dict Definitions, Dict Cache, Dict Messages) → [Ct] → [Ct] → [Ct] → TcPluginM TcPluginResult
solve (Dict, Dict, Dict) givens deriveds wanteds = do
  Dict ← return (findTraceKeys givens)

  trace @"Constraints" $
    hang (text "Givens:")   4 (ppr givens) $$
    hang (text "Deriveds:") 4 (ppr deriveds) $$
    hang (text "Wanteds:")  4 (ppr wanteds)

  Dict ← return (findEqualities givens)
  let pprCo x = ppr x <+> text "∷" <+> ppr (coercionKind x)
  trace @"Equalities" (ppr (map pprCo ?equalities))

  (Dict, givens') ← findCached givens
  trace @"Cached" (ppr ?cached)

  (rules, givens'') ← findRules givens'
  trace @"Rules" (ppr rules)

  case applyRules givens'' (deriveds ++ wanteds) rules of
    []      → return (TcPluginOk [] [])
    apply:_ → apply

applyRules ∷ (Definitions, Cached, Equalities, Messages, TraceKeys) ⇒ [Ct] → [Ct] → [Rule] → [TcPluginM TcPluginResult]
applyRules givens wanteds rules = do
  rule@Rule {..} ← rules
  apply ← applyRule givens wanteds rule
  return do
    addUsedGREs (maybeToList ruleElt)
    apply

applyRule ∷ (Definitions, Cached, Equalities, Messages, TraceKeys) ⇒ [Ct] → [Ct] → Rule → [TcPluginM TcPluginResult]
applyRule _ wanteds rule@Rule { ruleGoal = IntroGoal template, .. } = do
  ct       ← wanteds
  (coe, σ) ← runStateT (match template (equivClass (ctPred ct))) ruleArgs
  return do
    σ' ← instantiate σ ruleVars
    evs ← mapM (newWanted (bumpCtLocDepth (ctLoc ct)) . substTyAddInScope σ') ruleCts
    trace @"Intro" $
      hang (text "Applying Intro") 4 (ppr rule) $$
      text "with" <+> ppr σ'

    let ruleExpr = Var ruleDef
          `mkTyApps` map (fromJust . lookupTyVar σ') ruleVars
          `mkApps`   map ctEvExpr evs
        DictTy [goal] = exprType ruleExpr
        openExpr = OpenDictExpr [Type goal, Type goal, ruleExpr]
        castExpr = mkCast openExpr (mkSubCo (mkSymCo coe))
    return (TcPluginOk [(EvExpr castExpr, ct)] (map mkNonCanonical evs))
applyRule givens wanteds rule@Rule { ruleGoal = DerivGoal _, .. } = do
  guard (null wanteds)
  (evs, σ) ← runStateT (matchAny ruleCts (map (\ct → (equivClass (ctPred ct), ctEvidence ct)) givens)) ruleArgs
  let ruleExpr = Var ruleDef
        `mkTyApps` map (fromJust . lookupTyVar σ) ruleVars
        `mkApps`   map (\(ev, coe) → mkCast (ctEvExpr ev) (mkSubCo coe)) evs
      DictTy [goal] = exprType ruleExpr
      openExpr = OpenDictExpr [Type goal, Type goal, ruleExpr]
  guard (not (isCached goal))
  return do
    trace @"Deriv" $
      hang (text "Applying Deriv") 4 (ppr rule) $$
      text "with" <+> ppr σ
    cachedExpr ← cached goal
    emitGivens ruleLoc [openExpr, cachedExpr] wanteds
applyRule givens wanteds rule@Rule { ruleGoal = SimplGoal lhs _, .. } = do
  ct ← if null wanteds then givens else wanteds
  guard (not (isHoleCt ct))

  child     ← children (ctPred ct)
  σ         ← execStateT (match lhs (equivClass child)) ruleArgs
  (evs, σ') ← runStateT (matchAny ruleCts (map (\c → (equivClass (ctPred c), ctEvidence c)) givens)) σ
  let ruleExpr = Var ruleDef
        `mkTyApps` map (fromJust . lookupTyVar σ') ruleVars
        `mkApps`   map (\(ev, coe) → mkCast (ctEvExpr ev) (mkSubCo coe)) evs
      DictTy [goal] = exprType ruleExpr
      openExpr = OpenDictExpr [Type goal, Type goal, ruleExpr]
  guard (not (isCached goal))
  return do
    trace @"Simpl" $
      hang (text "Applying Simpl") 4 (ppr rule) $$
      text "with" <+> ppr σ'
    cachedExpr ← cached goal
    emitGivens ruleLoc [openExpr, cachedExpr] wanteds

instantiate ∷ TCvSubst → [Var] → TcPluginM TCvSubst
instantiate σ [] = return σ
instantiate σ (x:xs)
  | isJust (lookupTyVar σ x) = instantiate σ xs
  | otherwise = do
      x' ← newFlexiTyVar (substTyAddInScope σ (varType x))
      instantiate (extendTvSubstAndInScope σ x (mkTyVarTy x')) xs

children ∷ Type → [Type]
children = \t → t : go t where
  go ∷ Type → [Type]
  go (splitAppTy_maybe    → Just (t, u))  = children t ++ children u
  go (splitTyConApp_maybe → Just (_, ts)) = concatMap children ts
  go (splitFunTy_maybe    → Just (t, u))  = children t ++ children u
  go (splitCastTy_maybe   → Just (t, _))  = children t
  go _                                    = []

emitGivens ∷ (Messages, TraceKeys) ⇒ CtLoc → [EvExpr] → [Ct] → TcPluginM TcPluginResult
emitGivens loc givens [] = do
  evs ← mapM (\e → newGiven loc (exprType e) e) givens
  return (TcPluginOk [] (map mkNonCanonical evs))
emitGivens loc givens wanteds = do
  let remap (EqPrimTy [x, y, z, w]) = HEqTy [x, y, z, w]
      remap t                       = t
      unmap (EqPrimTy tys) e = Var (classSCSelId heqClass 0) `mkTyApps` tys `mkApps` [e]
      unmap _ e = e
      outputs = map (remap . ctPred) wanteds
  (output, sel) ←
    case outputs of
      [t] → return (t, \e _ _ → e)
      _   → do
        cls ← tcLookupClass (cTupleTyConName (length wanteds))
        return (mkClassPred cls outputs, \e xs x →
          mkSingleAltCase
            e (mkWildValBinder (exprType e))
            (DataAlt (classDataCon cls)) xs
            (Var x))

  ev   ← newWanted loc (mkInvisFunTys (map exprType givens) output)
  vars ← mapM (\t → (`mkLocalId` t) <$> newName (mkVarOcc "x")) outputs
  let app = mkApps (ctEvExpr ev) givens
      evs = zipWith (\ct x → unmap (ctPred ct) (sel app vars x)) wanteds vars
  trace @"EmitGivens" $
    hang (text "Adding")    4 (ppr ev) $$
    hang (text "Replacing") 4 (ppr (map (\e → ppr e <+> text "∷" <+> ppr (exprType e)) evs))

  return (TcPluginOk (zip (map EvExpr evs) wanteds) [mkNonCanonical ev])
