{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Patterns.SlateWorks where

import Haskcasting.ExprLang.TH (mkGreatIotaFragExpr, mkIotaFragExpr)
import Haskcasting.Fragment (Fragment, fragSingleton)
import Haskcasting.Iota (
  IotaAny,
  IotaAnyList,
  IotaBoolean,
  IotaEntity,
  IotaExec,
  IotaList,
  IotaNumber,
  IotaPattern (IotaPattern),
  IotaVector,
 )
import Haskcasting.Iota.Moreiotas (IotaItemStack)
import Haskcasting.Pattern (pattern)

-- This takes a number between 0 and 5 (inclusive), and sets the held slot of the bound Pocket Simulator. Free.
$( mkIotaFragExpr
     "SetSlot"
     [pattern| SOUTH_WEST eaqwqaeqawawa |]
     [[t|'[IotaNumber] -> '[]|]]
     -- ['number'] -> ['']
 )

-- This returns all the items held within the Pocket Simulator as Item Stack Iotas. Free.
$( mkIotaFragExpr
     "ListItems"
     [pattern| SOUTH_WEST eaqwqaeqawawaedd |]
     [[t|'[] -> '[IotaList IotaItemStack]|]]
     -- [''] -> ['[item stack]']
 )

-- Adds the current position of the Media Wave to the top of the stack.
$( mkIotaFragExpr
     "WaveLocationReflection"
     [pattern| SOUTH_WEST eaqdaadqaeeaa |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

-- Adds the current facing of the Media Wave to the top of the stack. If the current block does not have a facing, it returns a vector of [0,0,0].
$( mkIotaFragExpr
     "WaveFacingReflection"
     [pattern| SOUTH_WEST eaqdaadqaeewa |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

-- Adds the current speed of the Media Wave to the top of the stack. This is measured in how many 20ths of a second the Media Wave waits until going to the next Slate.
$( mkIotaFragExpr
     "WaveSpeedReflection"
     [pattern| SOUTH_WEST eaqdaadqaeewq |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

-- Adds the current amount of Media in the Impetus to the stack, in units of dust.
$( mkIotaFragExpr
     "CircleMediaReflection"
     [pattern| SOUTH_WEST eaqdaadqae |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

-- It is much easier to store an item into a Vessel than to pull it out. This pattern simply takes an Item Entity, and neatly sorts it into the activated Storage Vessels. Costs an 1/8th of a dust per activated Vessel.
$( mkIotaFragExpr
     "LayItem"
     [pattern| SOUTH_WEST eaqwqaeqwaeaeqqeaeaw |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['entity'] -> ['']
 )

-- This simply returns what Vessels the current Spell Circle has activated. I should likely use this to check if the Spell Circle has collected any Vessels.
$( mkIotaFragExpr
     "GetVessels"
     [pattern| SOUTH_WEST eaqwqaeqqdeewweedq |]
     [[t|'[] -> '[IotaList IotaVector]|]]
     -- [''] -> ['[vec]']
 )

-- This takes a Hex and runs it on every Item Stack stored (with it on top of the stack). Once the Hex is executed, it requires a Integer, Vector, and Boolean, left on the stack, in that order.
-- ['[patterns]'] -> []
iotaReawakenItem :: IotaPattern
iotaReawakenItem = IotaPattern [pattern| SOUTH_WEST eaqwqaeqwqqwqwwqwqqweqwaweadwawwwawdaewawq |]

fragReawakenItem :: Fragment (IotaExec (IotaItemStack ': s) (IotaBoolean ': IotaVector ': IotaNumber ': s') ': s) s
fragReawakenItem = fragSingleton iotaReawakenItem

-- Check Item functions much like Reawaken Item; however, it only requires a Boolean from the inputted Hex. If the Boolean is ever True, the spell ends, and returns True.
-- ['[patterns]'] -> ['bool']
iotaCheckItem :: IotaPattern
iotaCheckItem = IotaPattern [pattern| SOUTH_WEST eaqwqaeqqddqeeqddq |]

fragCheckItem :: Fragment (IotaExec (IotaItemStack ': s) (IotaBoolean ': s') ': s) (IotaBoolean ': s)
fragCheckItem = fragSingleton iotaReawakenItem

-- In some rare scenarios, Vessels can get jumbled up and have items of the same type spread across different  Vessels. This is a neat spell to resort them at a cost. That cost being 5 Charged Amethyst.
$( mkIotaFragExpr
     "ReorientItems"
     [pattern| SOUTH_WEST eaqwqaeqqwaeadaeawq |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- This binds both a pattern and an iota to a Spell Imprinter, which is at the targeted vector.
$( mkIotaFragExpr
     "BindMacro"
     [pattern| WEST qqqwqqqqqaqeeaqwqae |]
     [[t|forall a. '[IotaPattern, a, IotaVector] -> '[]|]]
     -- ['pattern', 'any iota', 'vec'] -> ['']
 )

-- Attempts to read an Iotic Door at the given vector. If there is not a Door, returns garbage. Does not mishap if casted outside of a Spell Circle, free, and does not require ambit.
$( mkIotaFragExpr
     "ReviewBroadcast"
     [pattern| WEST aqwqaweeeeewwaaw |]
     [[t|'[IotaVector] -> '[IotaAny]|]]
     -- ['vec'] -> ['any']
 )

-- Despite the return of this spell, its not extremely complicated... I think. This takes a Merchant Directrix, and returns: [[Item Stack, Item Stack, Item Stack, Number]...]. Free to cast.
$( mkIotaFragExpr
     "MerchantsPurification"
     [pattern| SOUTH_WEST eaqwqaewedeadwdwd |]
     [[t|'[IotaVector] -> '[IotaAny]|]]
     -- ['vec'] -> ['complicated']
 )

-- This feels... familiar. This spell takes a Villager, and a Merchant Directrix and exchanges their Jobs. Costs 1 Charged Amethyst to reapply the knowledge of Working.
$( mkIotaFragExpr
     "ExchangeMind"
     [pattern| SOUTH_WEST eaqwqaeqawawaddwwdqeeqdwwd |]
     [[t|'[IotaEntity, IotaVector] -> '[]|]]
     -- ['entity', 'vector'] -> ['']
 )

-- While digging amongst archives, I had oddly found this Great Spell. It takes a position of a Merchant Directrix, and forces it to restock; like the day had began or ended. Costs 5 Charged Amethyst.
$( mkGreatIotaFragExpr
     "InduceRestock"
     "Induce Restock"
     [pattern| SOUTH_WEST eaqwqaeqwaeaeqqeaeawedaawqwawqa |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Sets the crafting recipe of a Patterned Assembler. Takes a list of Nulls, items, or item variants, and applies it to the targeted Assembler.
$( mkIotaFragExpr
     "SetRecipe"
     [pattern| SOUTH_WEST eaqwqaeqwaeadawwadaeaw |]
     [[t|'[IotaAnyList, IotaVector] -> '[]|]]
     -- ['[item stack|item type|null]', 'vec'] -> ['']
 )

-- This sets how many times the Patterned Assembler will craft a recipe.
$( mkIotaFragExpr
     "SetCraftCount"
     [pattern| SOUTH_WEST eaqwqaeqwaeadawwadaeawedd |]
     [[t|'[IotaNumber, IotaVector] -> '[]|]]
     -- ['number', 'vec'] -> ['']
 )

-- This gets how many times the Patterned Assembler will craft a recipe.
$( mkIotaFragExpr
     "GetCraftCount"
     [pattern| SOUTH_WEST eaqwqaeqwaeadawwadaeawdaa |]
     [[t|'[IotaVector] -> '[IotaNumber]|]]
     -- ['vec'] -> ['number']
 )

-- This takes a list of vectors, and attempts to move the Pseudosentinels to the positions.
$( mkIotaFragExpr
     "ApplyPseudosentinels"
     [pattern| EAST waeawaewawwa |]
     [[t|'[IotaList IotaVector] -> '[]|]]
     -- ['[vec]'] -> ['']
 )

-- This retrieves the current positions of all Pseudosentinels in the order they were awoken. Free to cast.
$( mkIotaFragExpr
     "LocatePseudosentinels"
     [pattern| EAST waeawaewawwaeq |]
     [[t|'[] -> '[IotaList IotaVector]|]]
     -- [''] -> ['[vec]']
 )
