{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Data.Constraint.Rule.Plugin.Runtime (Cached, cached, unsafeOpenDict) where

import Data.Constraint (Dict, withDict)
import GHC.TypeNats    (Nat)
import Unsafe.Coerce   (unsafeCoerce)

class Cached (s ∷ Nat)

cached ∷ a
cached = error "cached: impossible"

{-# INLINE unsafeOpenDict #-}
unsafeOpenDict ∷ ∀a b. Dict a → b
unsafeOpenDict d = withDict d f
  where Magic f = unsafeCoerce (id ∷ b → b) ∷ Magic a b

newtype Magic a b = Magic (a ⇒ b)
