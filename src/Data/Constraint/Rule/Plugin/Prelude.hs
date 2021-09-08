{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module Data.Constraint.Rule.Plugin.Prelude (
  module GHC,
  addErr,
  addUsedGREs,
  getCtLocM,
  grePrintableName,
  isHoleCt,
  mkInvisFunTys,
  mkLocalId,
  mkWildValBinder,
  newName,
  newWanted,
  splitFunTy_maybe,
  tcSplitForAllTys
) where

import GHC.Plugins as GHC (AltCon (..), AnnTarget (..), Coercion, Expr (..),
                           FastString, GlobalRdrElt, Name, OccName,
                           Outputable (..), Plugin (..), Role (..), SDoc,
                           TCvSubst, TyCon, Type, TypeOrKind, Var,
                           cTupleTyConName, classDataCon, coercionKind,
                           defaultPlugin, deserializeWithData, emptyTCvSubst,
                           eqTyCon, eqType, exprType, extendTvSubstAndInScope,
                           findAnns, getTyVar_maybe, hang, heqClass, heqTyCon,
                           isLocalGRE, isNumLitTy, isStrLitTy, lookupTyVar,
                           mkAppCo, mkAppTy, mkApps, mkCast, mkModuleNameFS,
                           mkNomReflCo, mkNumLitTy, mkSingleAltCase, mkSubCo,
                           mkSymCo, mkTransCo, mkTvSubstPrs, mkTyApps,
                           mkTyConApp, mkTyConAppCo, mkTyVarTy, mkVarOcc,
                           mkVarOccFS, occEnvElts, pprTrace, pprTraceM,
                           prepareAnnotations, promotedConsDataCon,
                           promotedNilDataCon, purePlugin, quotes,
                           splitAppTy_maybe, splitCastTy_maybe, splitTyConApp,
                           splitTyConApp_maybe, substTyAddInScope, text,
                           typeKind, varName, varType, ($$), (<+>))

import GHC.Tc.Plugin as GHC (FindResult (..), TcPluginM, findImportedModule,
                             getEnvs, getTopEnv, lookupOrig, newFlexiTyVar,
                             newGiven, tcLookupClass, tcPluginIO, tcPluginTrace,
                             unsafeTcPluginTcM)

import GHC.Tc.Types.Constraint as GHC (Ct, CtEvidence (..), CtLoc,
                                       bumpCtLocDepth, ctEvCoercion, ctEvExpr,
                                       ctEvidence, ctLoc, ctPred,
                                       mkNonCanonical)

import GHC.Tc.Utils.Monad as GHC (TcGblEnv (..), TcPlugin (..),
                                  TcPluginResult (..), TcTyThing (..),
                                  mapMaybeM)

import GHC.Builtin.Types.Prim as GHC (eqPrimTyCon)
import GHC.Core.Class         as GHC (classSCSelId)
import GHC.Core.Predicate     as GHC (getEqPredRole, getEqPredTys, isEqPrimPred,
                                      mkClassPred)
import GHC.Data.Maybe         as GHC (MaybeErr (..))
import GHC.Data.Pair          as GHC (Pair (..))
import GHC.Iface.Load         as GHC (tcLookupImported_maybe)
import GHC.Tc.Types.Evidence  as GHC (EvExpr, EvTerm (..))
import GHC.Tc.Types.Origin    as GHC (CtOrigin (..))
import GHC.Tc.Utils.Env       as GHC (tcLookupLcl_maybe)
import GHC.Tc.Utils.TcType    as GHC (tcSplitPhiTy)
import GHC.Types.TyThing      as GHC (TyThing (..))

import qualified GHC.Plugins         as GHC.Internal
import qualified GHC.Rename.Env      as GHC.Internal
import qualified GHC.Tc.Plugin       as GHC.Internal
import qualified GHC.Tc.Utils.Monad  as GHC.Internal
import qualified GHC.Tc.Utils.TcType as GHC.Internal

#if !MIN_VERSION_ghc(9, 0, 1)
import qualified GHC.Tc.Types.Constraint as GHC.Internal
#endif

addErr ∷ SDoc → TcPluginM ()
addErr msg = unsafeTcPluginTcM (GHC.Internal.addErr msg)

addUsedGREs ∷ [GlobalRdrElt] → TcPluginM ()
addUsedGREs xs = unsafeTcPluginTcM (GHC.Internal.addUsedGREs xs)

getCtLocM ∷ CtOrigin → Maybe TypeOrKind → TcPluginM CtLoc
getCtLocM x y = unsafeTcPluginTcM (GHC.Internal.getCtLocM x y)

grePrintableName ∷ GlobalRdrElt → Name
#if MIN_VERSION_ghc(9, 2, 0)
grePrintableName = GHC.Internal.grePrintableName
#else
grePrintableName = GHC.Internal.gre_name
#endif

isHoleCt ∷ Ct → Bool
#if MIN_VERSION_ghc(9, 0, 1)
isHoleCt _ = False
#else
isHoleCt = GHC.Internal.isHoleCt
#endif

mkInvisFunTys ∷ [Type] → Type → Type
#if MIN_VERSION_ghc(9, 0, 1)
mkInvisFunTys = GHC.Internal.mkInvisFunTysMany
#else
mkInvisFunTys = GHC.Internal.mkInvisFunTys
#endif

mkLocalId ∷ Name → Type → Var
#if MIN_VERSION_ghc(9, 0, 1)
mkLocalId n = GHC.Internal.mkLocalId n GHC.Internal.Many
#else
mkLocalId = GHC.Internal.mkLocalId
#endif

mkWildValBinder ∷ Type → Var
#if MIN_VERSION_ghc(9, 0, 1)
mkWildValBinder = GHC.Internal.mkWildValBinder GHC.Internal.Many
#else
mkWildValBinder = GHC.Internal.mkWildValBinder
#endif

newName ∷ OccName → TcPluginM Name
newName x = unsafeTcPluginTcM (GHC.Internal.newName x)

newWanted ∷ CtLoc → Type → TcPluginM CtEvidence
newWanted loc ty = (\ev → ev { ctev_loc = loc }) <$> GHC.Internal.newWanted loc ty

{-# ANN splitFunTy_maybe "HLint: ignore Use camelCase" #-}
splitFunTy_maybe ∷ Type → Maybe (Type, Type)
#if MIN_VERSION_ghc(9, 0, 1)
splitFunTy_maybe t = (\(_, arg, res) → (arg, res)) <$> GHC.Internal.splitFunTy_maybe t
#else
splitFunTy_maybe = GHC.Internal.splitFunTy_maybe
#endif

tcSplitForAllTys ∷ Type → ([Var], Type)
#if MIN_VERSION_ghc(9, 2, 0)
tcSplitForAllTys = GHC.Internal.tcSplitForAllTyVars
#else
tcSplitForAllTys = GHC.Internal.tcSplitForAllTys
#endif
