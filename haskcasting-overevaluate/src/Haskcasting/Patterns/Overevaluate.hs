{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Patterns.Overevaluate where

import Data.Sequence qualified as Seq
import GHC.TypeNats (KnownNat)

import Haskcasting.ExprLang.TH (mkIotaFragExpr)
import Haskcasting.Fragment (Fragment (Fragment), fragSingleton)
import Haskcasting.Iota (
  IotaAny,
  IotaBoolean,
  IotaCast (iotaCast),
  IotaExec,
  IotaList,
  IotaNumber,
  IotaPattern (IotaPattern),
 )
import Haskcasting.Iota.Overevaluate (IotaJumble)
import Haskcasting.Pattern (Direction (..), Pattern (..), angles, pattern)
import Haskcasting.Util (natValInt)

-- Moves the first element to the end of the list that many times; negative numbers do the opposite.
$( mkIotaFragExpr
     "CyclingDistillation"
     [pattern| EAST wwaadaqadae |]
     [[t|forall a. '[IotaNumber, IotaList a] -> '[IotaList a]|]]
     -- ['number', 'list'] -> ['list']
 )

-- Cuts a list into three parts: what came before the given index, the iota at the given index, and what comes after that iota.
$( mkIotaFragExpr
     "ExtirpatingGambit"
     [pattern| EAST eawdq |]
     [[t|forall a. '[IotaNumber, IotaList a] -> '[IotaList a, a, IotaList a]|]]
     -- ['number', 'list'] -> ['list', 'any', 'list']
 )

-- Reflects the order of the top three iota, turning A, B, C into C, B, A. Essentially swaps the top and the third-from-the-top iota.
$( mkIotaFragExpr
     "ReflectingGambit"
     [pattern| NORTH_EAST ddwqaq |]
     [[t|forall a b c. '[a, b, c] -> '[c, b, a]|]]
     -- ['any', 'any', 'any'] -> ['any', 'any', 'any']
 )

-- Causes the third-from-the-top iota to bubble up over the second-from-the-top iota, essentially swapping them.
$( mkIotaFragExpr
     "BubblingGambit"
     [pattern| EAST aawede |]
     [[t|forall a b c. '[a, b, c] -> '[a, c, b]|]]
     -- ['any', 'any', 'any'] -> ['any', 'any', 'any']
 )

-- Takes a number and duplicates that many elements of the stack while preserving order.
-- ['number', 'many'] -> ['many']
iotaDioscuriGambitII :: IotaPattern
iotaDioscuriGambitII = IotaPattern [pattern| EAST waadadaa |]

-- Creates a jumble iota corresponding to the argument count and the list of numbers.
$( mkIotaFragExpr
     "JumblingGambit"
     [pattern| WEST deaqd |]
     [[t|'[IotaList IotaNumber, IotaNumber] -> '[IotaJumble]|]]
     -- ['list of numbers', 'number'] -> ['jumble']
 )

-- Dissolves a jumble iota back into a number and a list of numbers.
$( mkIotaFragExpr
     "JumblingDecomposition"
     [pattern| SOUTH_WEST aedqa |]
     [[t|'[IotaJumble] -> '[IotaList IotaNumber, IotaNumber]|]]
     -- ['jumble'] -> ['list of numbers', 'number']
 )

-- Pushes soroban's current value and then increases it. Starts at 0.
$( mkIotaFragExpr
     "SorobanReflection"
     [pattern| NORTH_EAST wdeaqq |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

-- Resets the soroban to 0.
$( mkIotaFragExpr
     "SorobanGambit"
     [pattern| NORTH_EAST qdeeaae |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Pops a number from the stack and sets the soroban to it.
$( mkIotaFragExpr
     "SorobanGambitII"
     [pattern| SOUTH_EAST waqdee |]
     [[t|'[IotaNumber] -> '[]|]]
     -- ['number'] -> ['']
 )

-- Casts a list of patterns similar to Hermes' Gambit but allows for handling of mishaps.
iotaAthenasGambit :: IotaPattern
iotaAthenasGambit = IotaPattern [pattern| SOUTH_EAST dweaqqw |]

fragAthenasGambit :: Fragment (IotaExec s s' ': s) (IotaBoolean ': s')
fragAthenasGambit = fragSingleton iotaAthenasGambit

-- Reveals to me the last mishap caught by Athena's Gambit within this Hex.
$( mkIotaFragExpr
     "AthenasRevelation"
     [pattern| SOUTH_EAST dweaqqqqa |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Pushes the last mishap caught by Athena's Gambit as text if the appropriate powers are active in this world.
$( mkIotaFragExpr
     "AthenasReflection"
     [pattern| SOUTH_EAST dweaqqqqd |]
     [[t|'[] -> '[IotaAny]|]]
     -- [''] -> ['any']
 )

-- -- Folds across the second list using the Hex or pattern in the first argument.

-- $( mkIotaFragExpr
--      "ApepsGambit"
--      [pattern| EAST dqd |]
--      [ [t|forall a. '[IotaList a, IotaPattern] -> '[IotaAny]|]
--      , [t|forall a. '[IotaList a, IotaList a] -> '[IotaAny]|]
--      ]
--      -- ['list', 'pattern/list'] -> ['any']
--  )

-- Does nothing.
$( mkIotaFragExpr
     "TutusGambit"
     [pattern| WEST eedqa |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Instantly terminates the Hex regardless of current circumstances.
iotaJanusGambit :: IotaPattern
iotaJanusGambit = IotaPattern [pattern| SOUTH_WEST aadee |]

fragJanusGambit :: Fragment as bs
fragJanusGambit = fragSingleton iotaJanusGambit

-- Takes a boolean and any iota. If the boolean is false, mishaps and prints the iota.
$( mkIotaFragExpr
     "MaatsGambit"
     [pattern| NORTH_EAST qed |]
     [[t|forall a. '[a, IotaBoolean] -> '[]|]]
     -- ['any', 'boolean'] -> ['']
 )

-- Should only be run inside Thoth's or Sisyphus' Gambit. If boolean is false, skips the rest of the current iteration and jumps to the next iteration.
$( mkIotaFragExpr
     "AtalantasGambit"
     [pattern| SOUTH_WEST aqdea |]
     [[t|'[IotaBoolean] -> '[]|]]
     -- ['boolean'] -> ['']
 )

-- Should only be run within a Thoth's. Details on the next page. Beware of triggering the Delve Too Deep mishap.
$( mkIotaFragExpr
     "CastorsGambit"
     [pattern| NORTH_WEST adadee |]
     [[t|forall a. '[a] -> '[]|]]
     -- ['any'] -> ['']
 )

-- Sibling pattern to Castor's Gambit, this pattern schedules the additional iteration for the end instead. Carries the same risk of encountering the Delve Too Deep mishap.
$( mkIotaFragExpr
     "PolluxsGambit"
     [pattern| NORTH_EAST dadaqq |]
     [[t|forall a. '[a] -> '[]|]]
     -- ['any'] -> ['']
 )

-- -- Takes a pattern list and casts it over and over forever until it encounters either a Charon's Gambit or a Janus' Gambit.

-- $( mkIotaFragExpr
--      "SisyphusGambit"
--      [pattern| NORTH_EAST qaqwede |]
--      [[t|forall a. '[IotaList a] -> '[]|]]
--      -- ['list'] -> ['']
--  )

-- -- Takes a pattern or pattern list and casts over every element the second list similar to Thoth's Gambit in order to sort it.

-- $( mkIotaFragExpr
--      "ThemisGambit"
--      [pattern| WEST dwaad |]
--      [ [t|forall a. '[IotaList a, IotaPattern] -> '[IotaList a]|]
--      , [t|forall a. '[IotaList a, IotaList a] -> '[IotaList a]|]
--      ]
--      -- ['list', 'pattern/list'] -> ['list']
--  )

--- special

-- Voids the entire stack except for the top n iota, determined by tail length.
iotaSekhmetsGambit :: Int -> IotaPattern
iotaSekhmetsGambit n = IotaPattern $ Pattern DirectionSE ([angles| qaqdd |] <> take n (cycle [angles| qe |]))

fragSekhmetsGambit :: forall n as. KnownNat n => Fragment as as
fragSekhmetsGambit =
  Fragment $ Seq.singleton (iotaCast (iotaSekhmetsGambit (natValInt @n)))

-- Yanks the iota n from the top up to the top, determined by tail length.
iotaGebsGambit :: Int -> IotaPattern
iotaGebsGambit n = IotaPattern $ Pattern DirectionE ([angles| aaeaa |] <> take n ([angles| d |] <> cycle [angles| w |]))

fragGebsGambit :: forall n as. KnownNat n => Fragment as as
fragGebsGambit =
  Fragment $ Seq.singleton (iotaCast (iotaGebsGambit (natValInt @n)))

-- Sinks the top iota n from the top, determined by tail length.
iotaNutsGambit :: Int -> IotaPattern
iotaNutsGambit n = IotaPattern $ Pattern DirectionE ([angles| aawdd |] <> take n ([angles| e |] <> cycle [angles| w |]))

fragNutsGambit :: forall n as. KnownNat n => Fragment as as
fragNutsGambit =
  Fragment $ Seq.singleton (iotaCast (iotaNutsGambit (natValInt @n)))

-- Pops a pattern or pattern list and dives down an amount corresponding to the tail length to cast the pattern list. Useful for surgery-like operations.
-- iotaNephthysGambit
