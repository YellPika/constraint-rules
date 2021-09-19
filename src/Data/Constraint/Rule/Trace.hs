{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UnicodeSyntax           #-}

module Data.Constraint.Rule.Trace (TraceKey, Trace, withTrace) where

import Data.Class.Closed.TH (close)
import Data.Kind            (Constraint)
import GHC.TypeLits         (ErrorMessage (..), Symbol, TypeError)

type family TraceKey (key ∷ Symbol) ∷ Constraint where
  TraceKey "Constraints" = ()
  TraceKey "Equalities"  = ()
  TraceKey "Cached"      = ()
  TraceKey "Rules"       = ()
  TraceKey "Intro"       = ()
  TraceKey "Deriv"       = ()
  TraceKey "Simpl"       = ()
  TraceKey "EmitGivens"  = ()
  TraceKey "Annotations" = ()
  TraceKey "Lookup"      = ()
  TraceKey key           = TypeError ('Text "Invalid trace key " ':<>: 'ShowType key)

close [d|
  class    TraceKey key ⇒ Trace key
  instance TraceKey key ⇒ Trace key
 |]

withTrace ∷ ∀key a. TraceKey key ⇒ (Trace key ⇒ a) → a
withTrace x = x
