{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Data.Constraint.Rule.Symbol where

import           Data.Constraint        (Dict (..), HasDict (..))
import           Data.Constraint.Nat    (Min)
import           Data.Constraint.Rule   (RuleUsage (..))
import           Data.Constraint.Symbol (Drop, Length, Take, type (++))
import qualified Data.Constraint.Symbol as Symbol
import           GHC.TypeLits           (KnownNat, KnownSymbol, type (+),
                                         type (<=))

{-# ANN appendSymbol Intro #-}
appendSymbol ∷ ∀m n. (KnownSymbol m, KnownSymbol n) ⇒ Dict (KnownSymbol (m ++ n))
appendSymbol = evidence (Symbol.appendSymbol @m @n)

{-# ANN takeSymbol Intro #-}
takeSymbol ∷ ∀n a. (KnownNat n, KnownSymbol a) ⇒ Dict (KnownSymbol (Take n a))
takeSymbol = evidence (Symbol.takeSymbol @n @a)

{-# ANN dropSymbol Intro #-}
dropSymbol ∷ ∀n a. (KnownNat n, KnownSymbol a) ⇒ Dict (KnownSymbol (Drop n a))
dropSymbol = evidence (Symbol.dropSymbol @n @a)

{-# ANN takeAppendDrop Simpl #-}
takeAppendDrop ∷ ∀n a. Dict ((Take n a ++ Drop n a) ~ a)
takeAppendDrop = evidence (Symbol.takeAppendDrop @n @a)

{-# ANN lengthSymbol Intro #-}
lengthSymbol ∷ ∀a. KnownSymbol a ⇒ Dict (KnownNat (Length a))
lengthSymbol = evidence (Symbol.lengthSymbol @a)

{-# ANN takeLength Simpl #-}
takeLength ∷ ∀n a. (Length a <= n) ⇒ Dict (Take n a ~ a)
takeLength = evidence (Symbol.takeLength @n @a)

{-# ANN take0 Simpl #-}
take0 ∷ ∀a. Dict (Take 0 a ~ "")
take0 = evidence (Symbol.take0 @a)

{-# ANN takeEmpty Simpl #-}
takeEmpty ∷ ∀n. Dict (Take n "" ~ "")
takeEmpty = evidence (Symbol.takeEmpty @n)

{-# ANN dropLength Simpl #-}
dropLength ∷ ∀n a. (Length a <= n) ⇒ Dict (Drop n a ~ "")
dropLength = evidence (Symbol.dropLength @n @a)

{-# ANN drop0 Simpl #-}
drop0 ∷ ∀a. Dict (Drop 0 a ~ a)
drop0 = evidence (Symbol.drop0 @a)

{-# ANN dropEmpty Simpl #-}
dropEmpty ∷ ∀n. Dict (Drop n "" ~ "")
dropEmpty = evidence (Symbol.dropEmpty @n)

{-# ANN lengthTake Simpl #-}
lengthTake ∷ ∀n a. Dict (Length (Take n a) <= n)
lengthTake = evidence (Symbol.lengthTake @n @a)

{-# ANN lengthDrop Simpl #-}
lengthDrop ∷ ∀n a. Dict (Length a <= (Length (Drop n a) + n))
lengthDrop = evidence (Symbol.lengthDrop @n @a)

{-# ANN dropDrop Simpl #-}
dropDrop ∷ ∀n m a. Dict (Drop n (Drop m a) ~ Drop (n + m) a)
dropDrop = evidence (Symbol.dropDrop @n @m @a)

{-# ANN takeTake Simpl #-}
takeTake ∷ ∀n m a. Dict (Take n (Take m a) ~ Take (Min n m) a)
takeTake = evidence (Symbol.takeTake @n @m @a)
