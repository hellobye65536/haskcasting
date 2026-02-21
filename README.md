# Haskcasting

A typechecked Haskell based Domain Specific Language for Hexcasting.

Pattern definitions (modules in `Haskcasting.Patterns.*`) were AI-generated and hand verified using data from the [pattern registry](https://book.hexxy.media/v/2.7.4/registry.json)
(except for Hexical 1.5.0, which was scraped from the [docs](https://hexical.hexxy.media/v/1.5.0/1.0.0/en_us)).

## Usage Guide

You should be familiar with Hexcasting, and I recommend that you have some familiarity with Haskell, especially if you aim to write spells involving meta-evaluation.
This DSL *does* typecheck, but prioritizes expressiveness over strictness, so you will end up having to wrangle with Haskell to get the types to check.

### Iotas

Iotas are defined in [Haskcasting.Iota](haskcasting/src/Haskcasting/Iota.hs).
Of note is the type `IotaAny`, a wrapper for a dynamically typed iota, and the typeclass `IotaCast`, representing valid iota casts.
All iotas are castable to `IotaAny`, which will be important later.

### The stack

The type of a stack in Haskcasting is represented using a type-level list (using `DataKinds`) in Haskell, with the top of the stack as the head of the list.

As an example, the type
```hs
'[IotaNumber, IotaNumber, IotaList IotaNumber]
```
would describe a stack of the form (INSERT IMAGE HERE)

Notably, this is the reverse of the order that arguments are defined in the Hex book,
so a spell like `Explosion (vector, number →)` would accept a stack of type `'[IotaNumber, IotaVector]`.

### Fragments

A Fragment (defined in [Haskcasting.Fragment](haskcasting/src/Haskcasting/Fragment.hs#L22)) is a sequence of patterns that affect the stack in specific ways.
A fragment of type `Fragment a b` expects a stack of type `a`, and after running, leaves a stack of type `b`.

### A simple example spell

The following is a spell that breaks the block you are looking at. (This code is available at [Ex1.hs](./example-spells/src/Ex1.hs))

```hs
breakBlock :: Fragment s s
breakBlock =
  fragMindsReflection
    +.+ fragCompassPurification
    +.+ fragMindsReflection
    +.+ fragAlidadesPurification
    +.+ fragArchersDistillation
    +.+ fragBreakBlock
```

Notice the generic type signature `Fragment a a`, meaning that this spell can accept any existing stack, and leaves it as is.

### A slightly more complicated spell

The following is a spell that when given a location, safely spawns an explosion at that location. (This code is available at [Ex2.hs](./example-spells/src/Ex2.hs))

```hs
explodeLocation :: Fragment (IotaVector ': s) s
explodeLocation =
  dupN @5
    +.+ fragBreakBlock
    +.+ fragCreateWater
    +.+ fragNumericalReflection @10
    +.+ fragExplosion
    +.+ fragConjureBlock
    +.+ fragBreakBlock
```

Notice the generic type signature `Fragment (IotaVector ': a) a`.
Of note is the type level operator `':`, that prepends a type to a type level list.
Then, this spell expects a stack with a vector at the top, and ends with the stack without that vector.
