{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Patterns.Caduceus where

import Haskcasting.ExprLang.TH (mkIotaFragExpr)
import Haskcasting.Iota (IotaPattern (IotaPattern), IotaAny)
import Haskcasting.Pattern (pattern)

-- Cast a pattern or list of patterns from the stack exactly like Hermes' Gambit, except that anything outside of this cast will not be captured by an Arke's Gambit within it.
iotaThetisGambit :: IotaPattern
iotaThetisGambit = IotaPattern [pattern| EAST wdeaqe |]

-- Like Thetis' Gambit, but also sets the jump tag of the cast to the given iota. If the iota is Null, this is equivalent to Thetis' Gambit.
iotaThetisGambitII :: IotaPattern
iotaThetisGambitII = IotaPattern [pattern| NORTH_WEST qedqaw |]

-- Cast a pattern or list of patterns from the stack similarly to Iris' Gambit. Must be drawn within Thetis' Gambit.
iotaArkesGambit :: IotaPattern
iotaArkesGambit = IotaPattern [pattern| WEST waqdeq |]

-- Like Arke's Gambit, but only patterns within an enclosing Thetis' Gambit II with a jump tag matching the given iota are captured. If the iota is Null, this is equivalent to Arke's Gambit.
iotaArkesGambitII :: IotaPattern
iotaArkesGambitII = IotaPattern [pattern| NORTH_EAST eqaedw |]

-- Copy the iota stored in the jump tag of the current evaluation and add it to the stack.
$( mkIotaFragExpr
     "EwersReflection"
     [pattern| EAST aeaaqawd |]
     [[t|'[] -> '[IotaAny]|]]
     -- [''] -> ['any']
 )

-- -- Like Ewer's Reflection, but the iota is read out of the jump tag of the top frame in a Jump iota.
-- $( mkIotaFragExpr
--      "EwersPurification"
--      [pattern| EAST adaaddad |]
--      [[t|'[jump] -> '[IotaAny]|]]
--      -- ['jump'] -> ['any']
--  )

-- Remove the top iota from the stack, and write it to the jump tag of the current evaluation.As with Chronicler's Gambit, I cannot write my own Name using this spell.
$( mkIotaFragExpr
     "EwersGambit"
     [pattern| WEST dqddedwa |]
     [[t|forall a. '[a] -> '[]|]]
     -- ['any'] -> ['']
 )

-- -- Cast a list of patterns from the stack exactly like Hermes' Gambit, except that a Prometheus' Gambit within this cast will jump to an index within this list of patterns.
-- $( mkIotaFragExpr
--      "ElbrusGambit"
--      [pattern| EAST qqdqdqdqq |]
--      [[t|'[[pattern]] -> '[many]|]]
--      -- ['[pattern]'] -> ['many']
--  )

-- -- Remove a number from the stack, then jump to that index in the list cast by the enclosing Elbrus' Gambit. If the number is negative, the index counts from the end of the list instead.
-- $( mkIotaFragExpr
--      "PrometheusGambit"
--      [pattern| NORTH_WEST wdaawdaw |]
--      [[t|'[IotaNumber] -> '[many]|]]
--      -- ['number'] -> ['many']
--  )
