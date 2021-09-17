{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE UnicodeSyntax   #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Constraint.Rule.Plugin.Message where

import Data.Constraint.Rule.Plugin.Prelude

import Data.Bifunctor  (first, second)
import Data.Constraint (Dict (..))
import Data.IORef      (IORef, modifyIORef', newIORef, readIORef)

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
