{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE ViewPatterns        #-}

module Data.Constraint.Rule.Plugin.Message where

import Data.Constraint.Rule.Plugin.Definitions
import Data.Constraint.Rule.Plugin.Prelude
import Data.Constraint.Rule.Trace

import Data.Bifunctor  (first, second)
import Data.Constraint (Dict (..))
import Data.IORef      (IORef, modifyIORef', newIORef, readIORef)
import Data.Proxy      (Proxy (..))
import GHC.TypeLits    (KnownSymbol, symbolVal)

type Messages = ?messages ∷ IORef ([(SrcSpan, SDoc)], [(SrcSpan, SDoc)])

newMessages ∷ TcPluginM (Dict Messages)
newMessages = do
  ref ← tcPluginIO (newIORef ([], []))
  let ?messages = ref
  return Dict

addErrorMessage ∷ Messages ⇒ SDoc → TcPluginM ()
addErrorMessage doc = do
  spn ← getSrcSpanM
  tcPluginIO (modifyIORef' ?messages (first ((spn, doc):)))

addWarningMessage ∷ Messages ⇒ SDoc → TcPluginM ()
addWarningMessage doc = do
  spn ← getSrcSpanM
  tcPluginIO (modifyIORef' ?messages (second ((spn, doc):)))

reportMessages ∷ Messages ⇒ TcPluginM ()
reportMessages = do
  flags ← getDynFlags
  unqual ← getPrintUnqualified flags
  let undup = foldr insert []

      pick (srcSpanStart → toRealSrcLoc → Just loc, _) msg
        | srcLocLine loc == 1 && srcLocCol loc == 1 = [msg]
      pick msg (srcSpanStart → toRealSrcLoc → Just loc, _)
        | srcLocLine loc == 1 && srcLocCol loc == 1 = [msg]
      pick msg@(loc, _) msg'@(loc', _)
        | loc == loc' = [msg]
        | otherwise   = [msg, msg']

      insert msg [] = [msg]
      insert msg@(spn, doc) (msg'@(spn', doc'):msgs)
        | show (mkLongErrMsg flags spn unqual doc  empty) ==
          show (mkLongErrMsg flags spn unqual doc' empty) = pick msg msg' ++ msgs
        | otherwise = (spn', doc') : insert msg msgs

  (errs, warns) ← tcPluginIO (readIORef ?messages)
  mapM_ (uncurry addErrAt) (undup errs)
  mapM_ (uncurry (addWarnAt NoReason)) (undup warns)

type TraceKeys = (?traceKeys ∷ [FastString])

findTraceKeys ∷ Definitions ⇒ [Ct] → Dict TraceKeys
findTraceKeys cts = let ?traceKeys = go cts in Dict where
  go []                                       = []
  go ((ctPred → TraceTy [StrLitTy key]) : xs) = key : go xs
  go (_ : xs)                                 = go xs

trace ∷ ∀key. (KnownSymbol key, TraceKey key, Messages, TraceKeys) ⇒ SDoc → TcPluginM ()
trace doc
  | key `elem` ?traceKeys = addWarningMessage doc
  | otherwise             = return ()
  where key = fsLit (symbolVal (Proxy @key))
