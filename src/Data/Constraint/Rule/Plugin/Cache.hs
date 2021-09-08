{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE UnicodeSyntax   #-}
{-# LANGUAGE ViewPatterns    #-}

module Data.Constraint.Rule.Plugin.Cache where

import Data.Constraint.Rule.Plugin.Definitions
import Data.Constraint.Rule.Plugin.Prelude

import Data.Constraint (Dict (..))
import Data.IORef      (IORef, newIORef, readIORef, writeIORef)
import Data.List       (findIndex)

type Cache = (?cache ∷ IORef [Type])

newCache ∷ TcPluginM (Dict Cache)
newCache = tcPluginIO do
  ref ← newIORef []
  let ?cache = ref
  return Dict

typeIndex ∷ Cache ⇒ Type → TcPluginM Integer
typeIndex t = tcPluginIO do
  cache ← readIORef ?cache
  case findIndex (eqType t) cache of
    Just i  → return (fromIntegral i)
    Nothing → do
      writeIORef ?cache (cache ++ [t])
      return (fromIntegral (length cache))

indexType ∷ Cache ⇒ Integer → TcPluginM Type
indexType i = tcPluginIO do
  cache ← readIORef ?cache
  return (cache !! fromIntegral i)

cached ∷ (Definitions, Cache) ⇒ Type → TcPluginM EvExpr
cached t = do
  i ← typeIndex t
  return (CachedExpr [Type (CachedTy [NumLitTy i])])

type Cached = (Cache, ?cached ∷ [Type])

isCached ∷ Cached ⇒ Type → Bool
isCached t = any (eqType t) ?cached

findCached ∷ (Cache, Definitions) ⇒ [Ct] → TcPluginM (Dict Cached, [Ct])
findCached = fmap (\(tys, cts) → let ?cached = tys in (Dict, cts)) . go where
  go [] = return ([], [])
  go ((ctPred → CachedTy [NumLitTy i]) : cts) = do
    t ← indexType i
    (ts, cts') ← go cts
    return (t:ts, cts')
  go (ct:cts) = do
    (ts, cts') ← go cts
    return (ts, ct:cts')
