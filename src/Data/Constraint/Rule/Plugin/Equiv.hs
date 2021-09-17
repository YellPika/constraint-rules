{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE UnicodeSyntax   #-}
{-# LANGUAGE ViewPatterns    #-}

module Data.Constraint.Rule.Plugin.Equiv (
  Equalities, findEqualities,
  EquivClass, equivClass, contains,
  match, matchAny
) where

import Data.Constraint.Rule.Plugin.Prelude hiding (empty)

import Control.Applicative       (Alternative (..))
import Control.Monad             (guard, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (..), get, put)
import Data.Bifunctor            (first)
import Data.Constraint           (Dict (..))
import Data.Foldable             (asum, toList)
import Data.List.NonEmpty        (NonEmpty (..), head)
import Data.Maybe                (isJust)
import Prelude                   hiding (head, reverse)

type Equalities = (?equalities ∷ [Coercion])

findEqualities ∷ [Ct] → Dict Equalities
findEqualities cts = let ?equalities = eqs in Dict where
  eqs =
    map (ctEvCoercion . ctEvidence) .
    filter ((== Nominal) . getEqPredRole . ctPred) .
    filter (isEqPrimPred . ctPred) $
      cts

type EquivClass = NonEmpty (EquivInst, Coercion)

data EquivInst
  = EApp EquivClass EquivClass
  | ETyCon TyCon [EquivClass]
  | EAtomic Type

repr ∷ EquivClass → Type
repr = fst . head . flatten

flatten ∷ EquivClass → NonEmpty (Type, Coercion)
flatten t = do
  (t₀, coe₀) ← t
  (t₁, coe₁) ← flattenInst t₀
  return (t₁, mkTransCo coe₀ coe₁)

flattenInst ∷ EquivInst → NonEmpty (Type, Coercion)
flattenInst (EApp t u) = do
  (t', coet) ← flatten t
  (u', coeu) ← flatten u
  return (mkAppTy t' u', mkAppCo coet coeu)
flattenInst (ETyCon c ts) = do
  ts' ← mapM flatten ts
  return (mkTyConApp c (map fst ts'), mkTyConAppCo Nominal c (map snd ts'))
flattenInst (EAtomic t) = (t, mkNomReflCo t) :| []

containsCo ∷ Alternative f ⇒ EquivClass → Type → f Coercion
containsCo ty' ty = asum (fmap go ty') where
  go (EApp t' u', coe) | Just (t, u) ← splitAppTy_maybe ty = do
    coe₁ ← containsCo t' t
    coe₂ ← containsCo u' u
    return (mkTransCo coe (mkAppCo coe₁ coe₂))
  go (ETyCon c' ts', coe) | Just (c, ts) ← splitTyConApp_maybe ty = do
    guard (c == c' && length ts == length ts')
    coes ← zipWithM containsCo ts' ts
    return (mkTransCo coe (mkTyConAppCo Nominal c coes))
  go (EAtomic t, coe) = do
    guard (eqType t ty)
    return coe
  go _ = empty

contains ∷ EquivClass → Type → Bool
contains ty' ty = isJust (containsCo ty' ty)

sing ∷ Type → EquivClass
sing ty = (ty', mkNomReflCo ty) :| [] where
  ty' | Just (t, u) ← splitAppTy_maybe ty = EApp (sing t) (sing u)
      | Just (c, ts) ← splitTyConApp_maybe ty = ETyCon c (map sing ts)
      | otherwise = EAtomic ty

close ∷ Coercion → EquivClass → EquivClass → EquivClass → EquivClass
close co lhs rhs = go where
  go ∷ EquivClass → EquivClass
  go c@(t :| ts) =
    first goInst t :|
    map (first goInst) (ts ++ lhs'' ++ rhs'')
    where clhs = c `containsCo` repr lhs
          crhs = c `containsCo` repr rhs
          lhs'' | Just co' ← crhs
                , Nothing  ← clhs
                = map (fmap (mkTransCo (mkTransCo co' (mkSymCo co)))) (toList lhs)
                | otherwise = []
          rhs'' | Just co' ← clhs
                , Nothing  ← crhs
                = map (fmap (mkTransCo (mkTransCo co' co))) (toList rhs)
                | otherwise = []

  goInst ∷ EquivInst → EquivInst
  goInst (EApp t u)    = EApp (go t) (go u)
  goInst (ETyCon c ts) = ETyCon c (map go ts)
  goInst (EAtomic t)   = EAtomic t

equivClass ∷ Equalities ⇒ Type → EquivClass
equivClass = go ?equalities where
  go [] t = sing t
  go (eq@(coercionKind → Pair lhs rhs):eqs) t =
    close eq (go eqs lhs) (go eqs rhs) (go eqs t)

match ∷ Type → EquivClass → StateT TCvSubst [] Coercion
match template goal | Just x ← getTyVar_maybe template = do
  σ ← get
  case lookupTyVar σ x of
    Just t  → maybe empty return (goal `containsCo` t)
    Nothing → do
      let (t, coe) = head (flatten goal)
      put (extendTvSubstAndInScope σ x t)
      return coe
match template goal = asum (fmap go goal) where
  go (EApp t' u', coe) | Just (t, u) ← splitAppTy_maybe template = do
    coe₁ ← match t t'
    coe₂ ← match u u'
    return (mkTransCo coe (mkAppCo coe₁ coe₂))
  go (ETyCon c' ts', coe) | Just (c, ts) ← splitTyConApp_maybe template = do
    guard (c == c' && length ts == length ts')
    coes ← zipWithM match ts ts'
    return (mkTransCo coe (mkTyConAppCo Nominal c coes))
  go (EAtomic t, coe) = do
    guard (eqType t template)
    return coe
  go _ = empty

matchAny ∷ [Type] → [(EquivClass, a)] → StateT TCvSubst [] [(a, Coercion)]
matchAny [] _ = return []
matchAny (template:templates) goals = do
  ((goal, x), goals') ← lift (select goals)
  coe ← match template goal
  xs ← matchAny templates goals'
  return ((x, coe):xs)

select ∷ [a] → [(a, [a])]
select [] = []
select (x:xs) = (x, xs) : do
  (y, ys) ← select xs
  return (y, x:ys)
