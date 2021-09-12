# constraint-rules

This package provides a GHC plugin to facilitate the implementation of custom type checking rules, _without_ the need to implement a new type checker plugin every time. To use this plugin, add the following line to the top of the file for which you would like to use the extension:

```haskell
{-# OPTIONS_GHC -fplugin=Data.Constraint.Rule.Plugin #-}
```

Three types of rules are supported:

* _Introduction_ rules replace wanted constraints with other wanted constraints. They are declared as

  ```haskell
  myIntroRule ∷ (C₁, ..., Cₙ) ⇒ Dict C
  myIntroRule = ...
  ```

  Whenever `C` appears as a wanted contraint, it is replaced by the set of constraints `C₁, ..., Cₙ`.

* _Derivation_ rules add additional constraints to the set of given constraints. They are declared as

  ```haskell
  myDerivRule ∷ (C₁, ..., Cₙ) ⇒ Dict C
  myDerivRule = ...
  ```

  Any type variables appearing in `C` must also appear in at least one of `C₁, ..., Cₙ`. Whenever the constraints `C₁, ..., Cₙ` appear as given constraints, the constraint `C` is added to set of given constraints.

* _Simplification_ rules add new equalities to the set of given constraints. They are declared as

  ```haskell
  mySimplRule ∷ (C₁, ..., Cₙ) ⇒ Dict (P ~ Q)
  mySimplRule = ...
  ```

  Whenever the constraints `C₁, ..., Cₙ` appear as given constraints and the pattern `P` appears in a given _or_ wanted constraint, the constraint `P ~ Q` is added to the set of given constraints.

## Usage

Suppose we have the introduction rule `plusNat`, and we'd like to use it to type check `example`:

```haskell
import Data.Constraint (Dict (..))
import GHC.TypeNats (KnownNat, type (+))

plusNat ∷ (KnownNat m, KnownNat n) ⇒ Dict (KnownNat (m + n))
plusNat = ...

example ∷ KnownNat n ⇒ Dict (KnownNat (5 + n))
example = Dict -- Error: unsatisfied `KnownNat (5 + n)` constraint.
```

By default, the plugin does not apply a rule unless explicitly told to. We can instruct the plugin solve the unsatisfied `KnownNat (5 + n)` constraint using `plusNat` by adding an `Intro` constraint to the context:

```haskell
import Data.Constraint.Rule (Intro)
import Data.Constraint.Rule.TH (spec)

example ∷ (Intro $(spec 'plusNat), KnownNat n) ⇒ Dict (KnownNat (5 + n))
example = Dict -- Works!
```

`Intro` constraints have trivial instances, so any code calling `example` need not worry about providing an instance. Another way to do this without changing the type signature of `example` is to use the `withIntro` function, which has the signature `withIntro ∷ Proxy a → (Intro a ⇒ r) → r`:

```haskell
import Data.Constraint.Rule (withIntro)
import Data.Constraint.Rule.TH (spec)

example ∷ KnownNat n ⇒ Dict (KnownNat (5 + n))
example = withIntro $(spec 'plusNat) Dict -- Works!
```

Lastly, we can instruct the plugin to use `plusNat` _by default_ by adding an annotation to `plusNat`:

```haskell
import Data.Constraint.Rule (RuleUsage (..))

{-# ANN plusNat Intro #-}
plusNat ∷ (KnownNat m, KnownNat n) ⇒ Dict (KnownNat (m + n))
plusNat = ...
```

Note that rules annotated in this manner are only applied automatically when they are imported:

```haskell
import MyRules (plusNat)

example ∷ KnownNat n ⇒ Dict (KnownNat (5 + n))
example = Dict -- Works!
```

This restriction exists to prevent rule implementors from accidentally creating an infinite loop. If the restriction didn't exist, the following code would pass the type checker but cause an infinite loop at runtime:

```haskell
{-# ANN plusNat Intro #-}
plusNat ∷ (KnownNat m, KnownNat n) ⇒ Dict (KnownNat (m + n))
plusNat = Dict
```

If you would like to prevent a rule from being applied by default, you must introduce a `NoIntro` constraint into the context of the expression being type checked:

```haskell
import MyRules (plusNat)
import Data.Constraint.Rule (Ignore)

example ∷ (NoIntro $(ref 'plusNat), KnownNat n) ⇒ Dict (KnownNat (5 + n))
example = Dict -- Unsatisfied `KnownNat (5 + n)` constraint.
```

This can also be achieved using the `ignoreIntro` function:

```haskell
import MyRules (plusNat)
import Data.Constraint.Rule (ignoreIntro)

example ∷ KnownNat n ⇒ Dict (KnownNat (5 + n))
example = ignoreIntro $(ref 'plusNat) Dict -- Unsatisfied `KnownNat (5 + n)` constraint.
```

## Use Cases

**Automatically Solving Known\* Constraints.** As shown in the example, this plugin can be used to automatically solve `KnownNat` constraints. Unlike the [ghc-typelits-knownnat](https://hackage.haskell.org/package/ghc-typelits-knownnat) package, this plugin can also be used to solve `KnownSymbol` constraints, and any other constraints of a similar nature.

**Automatically Solving Type Equalities.** Simplification rules can be used to automatically solve type equality constraints. For example, one can define a rule for the commutativity of addition:

```haskell
{-# ANN plusCommutes Simpl #-}
plusCommutes ∷ Dict ((x + y) ~ (y + x))
plusCommutes = ...
```

Unlike the [typelevel-rewrite-rules](https://hackage.haskell.org/package/typelevel-rewrite-rules) package, simplification rules do not modify any existing constraints and merely add additional equality constraints to the context. For example, if we want to solve the constraint `x + 5 ~ y`, the plugin will generate the constraints `x + 5 ~ 5 + x` and `5 + x ~ x + 5` and then stop (instead of endlessly rewriting `x + 5 → 5 + x → x + 5 → ...`). Thus this plugin terminates in many cases where `typelevel-rewrite-rules` won't.

Note that a full set of rules for associativity and commutativity might cause a blowup in the number of given constraints, so this approach should only be used for small problems. For a more robust solution, see the [ghc-typelits-natnormalise](https://hackage.haskell.org/package/ghc-typelits-natnormalise) package.

# Q & A

**What happens if I give a 'bad' rule implementation?** If your rule implementation evaluates to `⊥`, then this will manifest _at runtime_ whenever a constraint produced by your rule is used.

```haskell
badEq ∷ Dict (Eq a)
badEq = error "badEq"

uhOh ∷ a → Bool
uhOh x = withIntro $(spec 'badEq) (x == x)

> uhOh "OH NO"
*** Exception: badEq
```

**Can this break class coherence?** Yes and no.

We can easily break coherence if we use `unsafe*` functions to implement a rule that synthesizes a new class instance.

If we ignore `unsafe*` functions, it is possible to implement a rule that evaluates to `⊥`:

```haskell
{-# ANN badEq Intro #-}
badEq ∷ Dict (Eq a)
badEq = error "badEq"
```

However, this (should be) the _only_ way in which incoherence arises. If your rule implementations are total, then they will always evaluate to existing instances, and thus coherence is preserved. Otherwise, we have "coherence up to `⊥`".

**What order are rules applied?** The order is unspecified, so be careful about what rules you annotate! If in doubt, choose opt-in over opt-out.

Do note that this plugin runs _after_ the type-checker has already tried its hardest to solve your constraints, so existing type class instances take priority over introduction rules.

Additionally, only one rule is applied at a time. Every time a rule is applied, control is given back to the type checker, which then tries to make progress with the new constraints. If the type checker gets stuck again, the plugin is invoked once more and this process repeats until the constraint is solved or no more rules can be applied.

**Is termination guaranteed?** Not at all! If you have a rule that continuously generates larger and larger constraints, e.g.

```
{-# ANN plusZero Intro #-}
plusZero ∷ KnownNat (n + 1) ⇒ Dict (KnownNat n)
plusZero = ...
```

then type checking can absolutely loop forever. However, non-termination is not guaranteed either in these cases since GHC takes over in between every rule application and might shrink or solve all the remaining constraints.
