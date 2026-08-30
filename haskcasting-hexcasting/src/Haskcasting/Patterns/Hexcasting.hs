{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskcasting.Patterns.Hexcasting where

import Data.ByteString.Char8 qualified as BC
import Data.FileEmbed (embedFileRelative)
import Data.Foldable (toList)
import Data.HList (HAppendFD, HAppendListR, HReverse)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Haskcasting.ExprLang.Core (Expr)
import Haskcasting.ExprLang.Core qualified as E
import Haskcasting.ExprLang.TH (mkFragExprInstance, mkGreatIotaFragExpr, mkIotaFragExpr)
import Haskcasting.Fragment (Fragment, fragSingleton)
import Haskcasting.Iota (
  IotaAny,
  IotaBoolean,
  IotaEntity,
  IotaExec,
  IotaHList,
  IotaList,
  IotaNull,
  IotaNumber,
  IotaPattern (IotaPattern),
  IotaVector, IotaAnyList,
 )
import Haskcasting.Pattern (Angle, Pattern (Pattern), angleParse, angles, pattern)
import Haskcasting.Util (HListLen)

--- Cross-Mod Compatibility

-- Pehkui

-- Get the scale of the entity, as a proportion of their normal size. For most entities, this will be 1.
$( mkIotaFragExpr
     "GulliversPurification"
     [pattern| NORTH_WEST aawawwawwa |]
     [[t|'[IotaEntity] -> '[IotaNumber]|]]
     -- ['entity'] -> ['num']
 )

-- Set the scale of the entity, passing in a proportion of their normal size. Costs about 1 Amethyst Shard.
$( mkIotaFragExpr
     "AlterScale"
     [pattern| NORTH_EAST ddwdwwdwwd |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['num', 'entity'] -> ['']
 )

-- Adds me, the caster, to the stack.
$( mkIotaFragExpr
     "MindsReflection"
     [pattern| NORTH_EAST qaq |]
     [ [t|'[] -> '[IotaEntity]|]
     ]
     -- [''] -> ['entity | null']
 )

--- Patterns

-- Basic Patterns

-- Transforms an entity on the stack into the position of its eyes. I should probably use this on myself.
$( mkIotaFragExpr
     "CompassPurification"
     [pattern| EAST aa |]
     [[t|'[IotaEntity] -> '[IotaVector]|]]
     -- ['entity'] -> ['vector']
 )

-- Transforms an entity on the stack into the position it is standing. I should probably use this on other entities.
$( mkIotaFragExpr
     "CompassPurificationII"
     [pattern| NORTH_EAST dd |]
     [[t|'[IotaEntity] -> '[IotaVector]|]]
     -- ['entity'] -> ['vector']
 )

-- Transforms an entity on the stack into the direction it's looking in, as a unit vector.
$( mkIotaFragExpr
     "AlidadesPurification"
     [pattern| EAST wa |]
     [[t|'[IotaEntity] -> '[IotaVector]|]]
     -- ['entity'] -> ['vector']
 )

-- Combines two vectors (a position and a direction) into the answer to the question: If I stood at the position and looked in the direction, what block would I be looking at? Costs a negligible amount of media.
$( mkIotaFragExpr
     "ArchersDistillation"
     [pattern| EAST wqaawdd |]
     [ [t|'[IotaVector, IotaVector] -> '[IotaVector]|]
     ]
     -- ['vector', 'vector'] -> ['vector | null']
 )

-- Like Archer's Distillation, but instead returns a vector representing the answer to the question: Which side of the block am I looking at? Costs a negligible amount of media.
$( mkIotaFragExpr
     "ArchitectsDistillation"
     [pattern| EAST weddwaa |]
     [ [t|'[IotaVector, IotaVector] -> '[IotaVector]|]
     ]
     -- ['vector', 'vector'] -> ['vector | null']
 )

-- Like Archer's Distillation, but instead returns the entity I am looking at. Costs a negligible amount of media.
$( mkIotaFragExpr
     "ScoutsDistillation"
     [pattern| EAST weaqa |]
     [ [t|'[IotaVector, IotaVector] -> '[IotaEntity]|]
     ]
     -- ['vector', 'vector'] -> ['entity | null']
 )

-- Displays the top iota of the stack to me.
$( mkIotaFragExpr
     "Reveal"
     [pattern| NORTH_EAST de |]
     [[t|forall a. '[a] -> '[a]|]]
     -- ['any'] -> ['any']
 )

-- Transforms an entity on the stack into its height.
$( mkIotaFragExpr
     "StadiometersPurification"
     [pattern| NORTH_EAST awq |]
     [[t|'[IotaEntity] -> '[IotaNumber]|]]
     -- ['entity'] -> ['num']
 )

-- Transforms an entity on the stack into the direction in which it's moving, with the speed of that movement as that direction's magnitude.
$( mkIotaFragExpr
     "PacePurification"
     [pattern| EAST wq |]
     [[t|'[IotaEntity] -> '[IotaVector]|]]
     -- ['entity'] -> ['vector']
 )

-- Mathematics

-- Perform addition.
$( mkIotaFragExpr
     "AdditiveDistillation"
     [pattern| NORTH_EAST waaw |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
     -- ['num|vec', 'num|vec'] -> ['num|vec']
 )

-- Perform subtraction.
$( mkIotaFragExpr
     "SubtractiveDistillation"
     [pattern| NORTH_WEST wddw |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
     -- ['num|vec', 'num|vec'] -> ['num|vec']
 )

-- Perform multiplication or the dot product.
$( mkIotaFragExpr
     "MultiplicativeDistillation"
     [pattern| SOUTH_EAST waqaw |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaNumber]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
     -- ['num|vec', 'num|vec'] -> ['num|vec']
 )

-- Perform division or the cross product.
$( mkIotaFragExpr
     "DivisionDistillation"
     [pattern| NORTH_EAST wdedw |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
     -- ['num|vec', 'num|vec'] -> ['num|vec']
 )

-- Compute the absolute value or length.
$( mkIotaFragExpr
     "LengthPurification"
     [pattern| NORTH_EAST wqaqw |]
     [ [t|'[IotaNumber] -> '[IotaNumber]|]
     , [t|'[IotaVector] -> '[IotaNumber]|]
     ]
     -- ['num|vec'] -> ['number']
 )

-- Perform exponentiation or vector projection.
$( mkIotaFragExpr
     "PowerDistillation"
     [pattern| NORTH_WEST wedew |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
     -- ['num|vec', 'num|vec'] -> ['num|vec']
 )

-- "Floors" a number, cutting off the fractional component and leaving an integer value. If passed a vector, instead floors each of its components.
$( mkIotaFragExpr
     "FloorPurification"
     [pattern| EAST ewq |]
     [ [t|Fragment '[IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector] '[IotaVector]|]
     ]
     -- ['num|vec'] -> ['num|vec']
 )

-- "Ceilings" a number, raising it to the next integer value if it has a fractional component. If passed a vector, instead ceils each of its components.
$( mkIotaFragExpr
     "CeilingPurification"
     [pattern| EAST qwe |]
     [ [t|Fragment '[IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector] '[IotaVector]|]
     ]
     -- ['num|vec'] -> ['num|vec']
 )

-- Combine three numbers at the top of the stack into a vector's X, Y, and Z components (bottom to top).
$( mkIotaFragExpr
     "VectorExaltation"
     [pattern| EAST eqqqqq |]
     [[t|'[IotaNumber, IotaNumber, IotaNumber] -> '[IotaVector]|]]
     -- ['num', 'num', 'num'] -> ['vector']
 )

-- Split a vector into its X, Y, and Z components (bottom to top).
$( mkIotaFragExpr
     "VectorDisintegration"
     [pattern| EAST qeeeee |]
     [[t|'[IotaVector] -> '[IotaNumber, IotaNumber, IotaNumber]|]]
     -- ['vector'] -> ['num', 'num', 'num']
 )

-- Takes the modulus of two numbers. This is the amount remaining after division - for example, 5 % 2 is 1, and 5 % 3 is 2. When applied on vectors, performs the above operation elementwise.
$( mkIotaFragExpr
     "ModulusDistillation"
     [pattern| NORTH_EAST addwaad |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
     -- ['num|vec', 'num|vec'] -> ['num|vec']
 )

-- For a vector, coerce it to its nearest axial direction, a unit vector. For a number, return the sign of the number; 1 if positive, -1 if negative. In both cases, zero is unaffected.
$( mkIotaFragExpr
     "AxialPurification"
     [pattern| NORTH_WEST qqqqqaww |]
     [ [t|Fragment '[IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector] '[IotaVector]|]
     ]
     -- ['vec|num'] -> ['vec|num']
 )

-- Creates a random number between 0 and 1.
$( mkIotaFragExpr
     "EntropyReflection"
     [pattern| NORTH_WEST eqqq |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['num']
 )

-- Constants

-- Adds True to the top of the stack.
$( mkIotaFragExpr
     "TrueReflection"
     [pattern| SOUTH_EAST aqae |]
     [[t|'[] -> '[IotaBoolean]|]]
     -- [''] -> ['bool']
 )

-- Adds False to the top of the stack.
$( mkIotaFragExpr
     "FalseReflection"
     [pattern| NORTH_EAST dedq |]
     [[t|'[] -> '[IotaBoolean]|]]
     -- [''] -> ['bool']
 )

-- Adds the Null influence to the top of the stack.
$( mkIotaFragExpr
     "NullaryReflection"
     [pattern| EAST d |]
     [[t|'[] -> '[IotaNull]|]]
     -- [''] -> ['null']
 )

-- Adds (0, 0, 0) to the stack.
$( mkIotaFragExpr
     "VectorReflectionZero"
     [pattern| NORTH_WEST qqqqq |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

-- Adds (1, 0, 0) to the stack.
$( mkIotaFragExpr
     "VectorReflectionPX"
     [pattern| NORTH_WEST qqqqqea |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

-- Adds (0, 1, 0) to the stack.
$( mkIotaFragExpr
     "VectorReflectionPY"
     [pattern| NORTH_WEST qqqqqew |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

-- Adds (0, 0, 1) to the stack.
$( mkIotaFragExpr
     "VectorReflectionPZ"
     [pattern| NORTH_WEST qqqqqed |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

-- Adds (-1, 0, 0) to the stack.
$( mkIotaFragExpr
     "VectorReflectionNX"
     [pattern| SOUTH_WEST eeeeeqa |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

-- Adds (0, -1, 0) to the stack.
$( mkIotaFragExpr
     "VectorReflectionNY"
     [pattern| SOUTH_WEST eeeeeqw |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

-- Adds (0, 0, -1) to the stack.
$( mkIotaFragExpr
     "VectorReflectionNZ"
     [pattern| SOUTH_WEST eeeeeqd |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

-- Adds τ, the radial representation of a complete circle, to the stack.
$( mkIotaFragExpr
     "CirclesReflection"
     [pattern| NORTH_WEST eawae |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['num']
 )

-- Adds π, the radial representation of half a circle, to the stack.
$( mkIotaFragExpr
     "ArcsReflection"
     [pattern| NORTH_EAST qdwdq |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['num']
 )

-- Adds e, the base of natural logarithms, to the stack.
$( mkIotaFragExpr
     "EulersReflection"
     [pattern| EAST aaq |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['num']
 )

-- Stack Manipulation

-- Swaps the top two iotas of the stack.
$( mkIotaFragExpr
     "JestersGambit"
     [pattern| EAST aawdd |]
     [[t|forall a b. '[a, b] -> '[b, a]|]]
     -- ['any', 'any'] -> ['any', 'any']
 )

-- Yanks the iota third from the top of the stack to the top. [0, 1, 2] becomes [1, 2, 0].
$( mkIotaFragExpr
     "RotationGambit"
     [pattern| EAST aaeaa |]
     [[t|forall a b c. '[a, b, c] -> '[c, a, b]|]]
     -- ['any', 'any', 'any'] -> ['any', 'any', 'any']
 )

-- Yanks the top iota to the third position. [0, 1, 2] becomes [2, 0, 1].
$( mkIotaFragExpr
     "RotationGambitII"
     [pattern| NORTH_EAST ddqdd |]
     [[t|forall a b c. '[a, b, c] -> '[b, c, a]|]]
     -- ['any', 'any', 'any'] -> ['any', 'any', 'any']
 )

-- Duplicates the top iota of the stack.
$( mkIotaFragExpr
     "GeminiDecomposition"
     [pattern| EAST aadaa |]
     [[t|forall a. '[a] -> '[a, a]|]]
     -- ['any'] -> ['any', 'any']
 )

-- Copy the second-to-last iota of the stack to the top. [0, 1] becomes [0, 1, 0].
$( mkIotaFragExpr
     "ProspectorsGambit"
     [pattern| EAST aaedd |]
     [[t|forall a b. '[a, b] -> '[b, a, b]|]]
     -- ['any', 'any'] -> ['any', 'any', 'any']
 )

-- Copy the top iota of the stack, then put it under the second iota. [0, 1] becomes [1, 0, 1].
$( mkIotaFragExpr
     "UndertakersGambit"
     [pattern| EAST ddqaa |]
     [[t|forall a b. '[a, b] -> '[a, b, a]|]]
     -- ['any', 'any'] -> ['any', 'any', 'any']
 )

-- Removes the number at the top of the stack, then copies the top iota of the stack that number of times. (A count of 2 results in two of the iota on the stack, not three.)
iotaGeminiGambit :: IotaPattern
iotaGeminiGambit = IotaPattern [pattern| EAST aadaadaa |]

-- Copy the top two iotas of the stack. [0, 1] becomes [0, 1, 0, 1].
$( mkIotaFragExpr
     "DioscuriGambit"
     [pattern| EAST aadadaaw |]
     [[t|forall a b. '[a, b] -> '[a, b, a, b]|]]
     -- ['any', 'any'] -> ['any', 'any', 'any', 'any']
 )

-- Pushes the size of the stack as a number to the top of the stack. (For example, a stack of [0, 1] will become [0, 1, 2].)
$( mkIotaFragExpr
     "FlocksReflection"
     [pattern| NORTH_WEST qwaeawqaeaqa |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

iotaFishermansGambit, iotaFishermansGambitII, iotaSwindlersGambit :: IotaPattern

-- Grabs the element in the stack indexed by the number and brings it to the top. If the number is negative, instead moves the top element of the stack down that many elements.
iotaFishermansGambit = IotaPattern [pattern| WEST ddad |]

-- Like Fisherman's Gambit, but instead of moving the iota, copies it.
iotaFishermansGambitII = IotaPattern [pattern| EAST aada |]

-- Rearranges the top elements of the stack based on the given numerical code, which is the index of the permutation wanted.
iotaSwindlersGambit = IotaPattern [pattern| SOUTH_EAST qaawdde |]

-- Logical Operators

-- Convert an argument to a boolean. The number 0, Null, False, and the empty list become False; everything else becomes True.
$( mkIotaFragExpr
     "AugursPurification"
     [pattern| NORTH_EAST aw |]
     [[t|forall a. '[a] -> '[IotaBoolean]|]]
     -- ['any'] -> ['bool']
 )

-- Convert a boolean to a number; True becomes 1, and False becomes 0.
$( mkFragExprInstance
     "LengthPurification"
     [[t|'[IotaBoolean] -> '[IotaNumber]|]]
     -- ['bool'] -> ['number']
 )

-- If the argument is True, return False; if it is False, return True.
$( mkIotaFragExpr
     "NegationPurification"
     [pattern| NORTH_WEST dw |]
     [[t|'[IotaBoolean] -> '[IotaBoolean]|]]
     -- ['bool'] -> ['bool']
 )

-- Returns True if at least one of the arguments are True; otherwise returns False.
$( mkIotaFragExpr
     "DisjunctionDistillation"
     [pattern| SOUTH_EAST waw |]
     [[t|'[IotaBoolean, IotaBoolean] -> '[IotaBoolean]|]]
     -- ['bool', 'bool'] -> ['bool']
 )

-- Returns True if both arguments are true; otherwise returns False.
$( mkIotaFragExpr
     "ConjunctionDistillation"
     [pattern| NORTH_EAST wdw |]
     [[t|'[IotaBoolean, IotaBoolean] -> '[IotaBoolean]|]]
     -- ['bool', 'bool'] -> ['bool']
 )

-- Returns True if exactly one of the arguments is true; otherwise returns False.
$( mkIotaFragExpr
     "ExclusionDistillation"
     [pattern| NORTH_WEST dwa |]
     [[t|'[IotaBoolean, IotaBoolean] -> '[IotaBoolean]|]]
     -- ['bool', 'bool'] -> ['bool']
 )

-- If the first argument is True, keeps the second and discards the third; otherwise discards the second and keeps the third.
iotaAugursExaltation :: IotaPattern
iotaAugursExaltation = IotaPattern [pattern| SOUTH_EAST awdd |]

fragAugursExaltation :: forall a s. Fragment (a ': a ': IotaBoolean ': s) (a ': s)
fragAugursExaltation = fragSingleton iotaAugursExaltation

exprAugursExaltation :: Expr blk '[a, a, IotaBoolean] -> Expr blk '[a]
exprAugursExaltation = E.call fragAugursExaltation

$( mkIotaFragExpr
     "EqualityDistillation"
     [pattern| EAST ad |]
     [[t|forall a b. '[a, b] -> '[IotaBoolean]|]]
     -- ['any', 'any'] -> ['bool']
 )

-- If the first argument does not equal the second (outside a small tolerance), return True. Otherwise, return False.
$( mkIotaFragExpr
     "InequalityDistillation"
     [pattern| EAST da |]
     [[t|forall a b. '[a, b] -> '[IotaBoolean]|]]
     -- ['any', 'any'] -> ['bool']
 )

-- If the first argument is greater than the second, return True. Otherwise, return False.
$( mkIotaFragExpr
     "MaximusDistillation"
     [pattern| SOUTH_EAST e |]
     [[t|'[IotaNumber, IotaNumber] -> '[IotaBoolean]|]]
     -- ['number', 'number'] -> ['bool']
 )

-- If the first argument is less than the second, return True. Otherwise, return False.
$( mkIotaFragExpr
     "MinimusDistillation"
     [pattern| SOUTH_WEST q |]
     [[t|'[IotaNumber, IotaNumber] -> '[IotaBoolean]|]]
     -- ['number', 'number'] -> ['bool']
 )

-- If the first argument is greater than or equal to the second, return True. Otherwise, return False.
$( mkIotaFragExpr
     "MaximusDistillationII"
     [pattern| SOUTH_EAST ee |]
     [[t|'[IotaNumber, IotaNumber] -> '[IotaBoolean]|]]
     -- ['number', 'number'] -> ['bool']
 )

-- If the first argument is less than or equal to the second, return True. Otherwise, return False.
$( mkIotaFragExpr
     "MinimusDistillationII"
     [pattern| SOUTH_WEST qq |]
     [[t|'[IotaNumber, IotaNumber] -> '[IotaBoolean]|]]
     -- ['number', 'number'] -> ['bool']
 )

-- Entities

-- Transform the position on the stack into the entity at that location (or Null if there isn't one).
$( mkIotaFragExpr
     "EntityPurification"
     [pattern| SOUTH_EAST qqqqqdaqa |]
     [[t|'[IotaVector] -> '[IotaEntity]|]]
     -- ['vector'] -> ['entity or null']
 )

-- Transform the position on the stack into the animal at that location (or Null if there isn't one).
$( mkIotaFragExpr
     "EntityPurificationAnimal"
     [pattern| SOUTH_EAST qqqqqdaqaawa |]
     [[t|'[IotaVector] -> '[IotaEntity]|]]
     -- ['vector'] -> ['entity or null']
 )

-- Transform the position on the stack into the monster at that location (or Null if there isn't one).
$( mkIotaFragExpr
     "EntityPurificationMonster"
     [pattern| SOUTH_EAST qqqqqdaqaawq |]
     [[t|'[IotaVector] -> '[IotaEntity]|]]
     -- ['vector'] -> ['entity or null']
 )

-- Transform the position on the stack into the dropped item at that location (or Null if there isn't one).
$( mkIotaFragExpr
     "EntityPurificationItem"
     [pattern| SOUTH_EAST qqqqqdaqaaww |]
     [[t|'[IotaVector] -> '[IotaEntity]|]]
     -- ['vector'] -> ['entity or null']
 )

-- Transform the position on the stack into the player at that location (or Null if there isn't one).
$( mkIotaFragExpr
     "EntityPurificationPlayer"
     [pattern| SOUTH_EAST qqqqqdaqaawe |]
     [[t|'[IotaVector] -> '[IotaEntity]|]]
     -- ['vector'] -> ['entity or null']
 )

-- Transform the position on the stack into the living creature at that location (or Null if there isn't one).
$( mkIotaFragExpr
     "EntityPurificationLiving"
     [pattern| SOUTH_EAST qqqqqdaqaawd |]
     [[t|'[IotaVector] -> '[IotaEntity]|]]
     -- ['vector'] -> ['entity or null']
 )

-- Take a position and maximum distance on the stack, and combine them into a list of animals near the position.
$( mkIotaFragExpr
     "ZoneDistillationAnimal"
     [pattern| SOUTH_EAST qqqqqwdeddwa |]
     [[t|'[IotaNumber, IotaVector] -> '[IotaList IotaEntity]|]]
     -- ['number', 'vector'] -> ['list']
 )

-- Take a position and maximum distance on the stack, and combine them into a list of non-animal entities near the position.
$( mkIotaFragExpr
     "ZoneDistillationNonAnimal"
     [pattern| NORTH_EAST eeeeewaqaawa |]
     [[t|'[IotaNumber, IotaVector] -> '[IotaList IotaEntity]|]]
     -- ['number', 'vector'] -> ['list']
 )

-- Take a position and maximum distance on the stack, and combine them into a list of monsters near the position.
$( mkIotaFragExpr
     "ZoneDistillationMonster"
     [pattern| SOUTH_EAST qqqqqwdeddwq |]
     [[t|'[IotaNumber, IotaVector] -> '[IotaList IotaEntity]|]]
     -- ['number', 'vector'] -> ['list']
 )

-- Take a position and maximum distance on the stack, and combine them into a list of non-monster entities near the position.
$( mkIotaFragExpr
     "ZoneDistillationNonMonster"
     [pattern| NORTH_EAST eeeeewaqaawq |]
     [[t|'[IotaNumber, IotaVector] -> '[IotaList IotaEntity]|]]
     -- ['number', 'vector'] -> ['list']
 )

-- Take a position and maximum distance on the stack, and combine them into a list of dropped items near the position.
$( mkIotaFragExpr
     "ZoneDistillationItem"
     [pattern| SOUTH_EAST qqqqqwdeddww |]
     [[t|'[IotaNumber, IotaVector] -> '[IotaList IotaEntity]|]]
     -- ['number', 'vector'] -> ['list']
 )

-- Take a position and maximum distance on the stack, and combine them into a list of non-dropped-item entities near the position.
$( mkIotaFragExpr
     "ZoneDistillationNonItem"
     [pattern| NORTH_EAST eeeeewaqaaww |]
     [[t|'[IotaNumber, IotaVector] -> '[IotaList IotaEntity]|]]
     -- ['number', 'vector'] -> ['list']
 )

-- Take a position and maximum distance on the stack, and combine them into a list of players near the position.
$( mkIotaFragExpr
     "ZoneDistillationPlayer"
     [pattern| SOUTH_EAST qqqqqwdeddwe |]
     [[t|'[IotaNumber, IotaVector] -> '[IotaList IotaEntity]|]]
     -- ['number', 'vector'] -> ['list']
 )

-- Take a position and maximum distance on the stack, and combine them into a list of non-player characters near the position.
$( mkIotaFragExpr
     "ZoneDistillationNonPlayer"
     [pattern| NORTH_EAST eeeeewaqaawe |]
     [[t|'[IotaNumber, IotaVector] -> '[IotaList IotaEntity]|]]
     -- ['number', 'vector'] -> ['list']
 )

-- Take a position and maximum distance on the stack, and combine them into a list of living creatures near the position.
$( mkIotaFragExpr
     "ZoneDistillationLiving"
     [pattern| SOUTH_EAST qqqqqwdeddwd |]
     [[t|'[IotaNumber, IotaVector] -> '[IotaList IotaEntity]|]]
     -- ['number', 'vector'] -> ['list']
 )

-- Take a position and maximum distance on the stack, and combine them into a list of non-living entities near the position.
$( mkIotaFragExpr
     "ZoneDistillationNonLiving"
     [pattern| NORTH_EAST eeeeewaqaawd |]
     [[t|'[IotaNumber, IotaVector] -> '[IotaList IotaEntity]|]]
     -- ['number', 'vector'] -> ['list']
 )

-- Take a position and maximum distance on the stack, and combine them into a list of all entities near the position.
$( mkIotaFragExpr
     "ZoneDistillationAny"
     [pattern| SOUTH_EAST qqqqqwded |]
     [[t|'[IotaNumber, IotaVector] -> '[IotaList IotaEntity]|]]
     -- ['number', 'vector'] -> ['list']
 )

-- List Manipulation

-- Remove the number at the top of the stack, then replace the list at the top with the nth element of that list (where n is the number you removed). Replaces the list with Null if the number is out of bounds.
$( mkIotaFragExpr
     "SelectionDistillation"
     [pattern| NORTH_WEST deeed |]
     [[t|forall a. '[IotaNumber, IotaList a] -> '[a]|]]
     -- ['number', 'list'] -> ['any']
 )

-- Remove the two numbers at the top of the stack, then take a sublist of the list at the top of the stack between those indices, lower bound inclusive, upper bound exclusive. For example, the 0, 2 sublist of [0, 1, 2, 3, 4] would be [0, 1].
$( mkIotaFragExpr
     "SelectionExaltation"
     [pattern| NORTH_WEST qaeaqwded |]
     [[t|forall a. '[IotaNumber, IotaNumber, IotaList a] -> '[IotaList a]|]]
     -- ['num', 'num', 'list'] -> ['list']
 )

-- Remove the top of the stack, then add it to the end of the list at the top of the stack.
$( mkIotaFragExpr
     "IntegrationDistillation"
     [pattern| SOUTH_WEST edqde |]
     [ [t|forall a. '[a, IotaList a] -> '[IotaList a]|]
     , [t|forall a as as' p. (HReverse (a : p) as', HReverse p as) => '[a, IotaHList as] -> '[IotaHList as']|]
     ]
     -- ['any', 'list'] -> ['list']
 )

-- Remove the iota on the end of the list at the top of the stack, and add it to the top of the stack.
$( mkIotaFragExpr
     "DerivationDecomposition"
     [pattern| NORTH_WEST qaeaq |]
     [ [t|forall a. '[IotaList a] -> '[a, IotaList a]|]
     , [t|forall a as as' p. (HReverse (a : p) as', HReverse p as) => '[IotaHList as'] -> '[a, IotaHList as]|]
     ]
     -- ['list'] -> ['any', 'list']
 )

-- Remove the list at the top of the stack, then add all its elements to the end of the list at the top of the stack.
$( mkFragExprInstance
     "AdditiveDistillation"
     [ [t|forall as bs asbs. HAppendListR as bs ~ asbs => '[IotaHList bs, IotaHList as] -> '[IotaHList asbs]|]
     , [t|forall a. '[IotaList a, IotaList a] -> '[IotaList a]|]
     ]
     -- ['list', 'list'] -> ['list']
 )

-- Push an empty list to the top of the stack.
$( mkIotaFragExpr
     "VacantReflection"
     [pattern| NORTH_EAST qqaeaae |]
     [[t|'[] -> '[IotaHList '[]]|]]
     -- [''] -> ['list']
 )

-- Remove the top of the stack, then push a list containing only that element.
$( mkIotaFragExpr
     "SinglesPurification"
     [pattern| EAST adeeed |]
     [[t|forall a. '[a] -> '[IotaHList '[a]]|]]
     -- ['any'] -> ['list']
 )

-- Remove the list at the top of the stack, then push the number of elements in the list to the stack.
$( mkFragExprInstance
     "LengthPurification"
     [ [t|forall a. '[IotaList a] -> '[IotaNumber]|]
     , [t|forall as. '[IotaHList as] -> '[IotaNumber]|]
     ]
     -- ['list'] -> ['num']
 )

-- Reverse the list at the top of the stack.
$( mkIotaFragExpr
     "RetrogradePurification"
     [pattern| EAST qqqaede |]
     [ [t|forall a. '[IotaList a] -> '[IotaList a]|]
     , [t|forall as as'. HReverse as as' => '[IotaHList as] -> '[IotaHList as']|]
     ]
     -- ['list'] -> ['list']
 )

-- Remove the iota at the top of the stack, then replace the list at the top with the first index of that iota within the list (starting from 0). Replaces the list with -1 if the iota doesn't exist in the list.
$( mkIotaFragExpr
     "LocatorsDistillation"
     [pattern| EAST dedqde |]
     [[t|forall a. '[a, IotaList a] -> '[IotaNumber]|]]
     -- ['any', 'list'] -> ['num']
 )

-- Remove the number at the top of the stack, then remove the nth element of the list at the top of the stack (where n is the number you removed).
$( mkIotaFragExpr
     "ExcisorsDistillation"
     [pattern| SOUTH_WEST edqdewaqa |]
     [[t|forall a. '[IotaNumber, IotaList a] -> '[IotaList a]|]]
     -- ['num', 'list'] -> ['list']
 )

-- Remove the top iota of the stack and the number at the top, then set the nth element of the list at the top of the stack to that iota (where n is the number you removed). Does nothing if the number is out of bounds.
$( mkIotaFragExpr
     "SurgeonsExaltation"
     [pattern| NORTH_WEST wqaeaqw |]
     [[t|forall a. '[a, IotaNumber, IotaList a] -> '[IotaList a]|]]
     -- ['any', 'num', 'list'] -> ['list']
 )

-- Remove num elements from the stack, then add them to a list at the top of the stack.
iotaFlocksGambit :: IotaPattern
iotaFlocksGambit = IotaPattern [pattern| SOUTH_WEST ewdqdwe |]

-- Remove the list at the top of the stack, then push its contents to the stack.
$( mkIotaFragExpr
     "FlocksDisintegration"
     [pattern| NORTH_WEST qwaeawq |]
     []
 )

instance (HReverse as ras, HAppendFD ras bs rasbs) => FragFlocksDisintegration (IotaHList as ': bs) rasbs
instance (HReverse as ras, HListLen ras) => ExprFlocksDisintegration '[IotaHList as] ras

-- Remove the top iota, then add it as the first element to the list at the top of the stack.
$( mkIotaFragExpr
     "SpeakersDistillation"
     [pattern| SOUTH_EAST ddewedd |]
     [ [t|forall a. Fragment '[a, IotaList a] '[IotaList a]|]
     , [t|forall a as. Fragment '[a, IotaHList as] '[IotaHList (a ': as)]|]
     ]
 )

-- Remove the first iota from the list at the top of the stack, then push that iota to the stack.
$( mkIotaFragExpr
     "SpeakersDecomposition"
     [pattern| SOUTH_WEST aaqwqaa |]
     [ [t|forall a. '[IotaList a] -> '[a, IotaList a]|]
     , [t|forall a as as'. as' ~ a ': as => '[IotaHList as'] -> '[a, IotaHList as]|]
     ]
     -- ['list'] -> ['any', 'list']
 )

-- Reading and Writing

-- Copy the iota stored in the item in my other hand and add it to the stack.
$( mkIotaFragExpr
     "ScribesReflection"
     [pattern| EAST aqqqqq |]
     [[t|'[] -> '[IotaAny]|]]
     -- [''] -> ['any']
 )

-- Remove the top iota from the stack, and save it into the item in my other hand.
$( mkIotaFragExpr
     "ScribesGambit"
     [pattern| EAST deeeee |]
     [[t|forall a. '[a] -> '[]|]]
     -- ['any'] -> ['']
 )

-- Like Scribe's Reflection, but the iota is read out of an entity instead of my other hand.
$( mkIotaFragExpr
     "ChroniclersPurification"
     [pattern| EAST wawqwqwqwqwqw |]
     [[t|'[IotaEntity] -> '[IotaAny]|]]
     -- ['entity'] -> ['any']
 )

-- Like Scribe's Gambit, but the iota is written to an entity instead of my other hand.
$( mkIotaFragExpr
     "ChroniclersGambit"
     [pattern| EAST wdwewewewewew |]
     [[t|forall a. '[a, IotaEntity] -> '[]|]]
     -- ['any', 'entity'] -> ['']
 )

-- If the item in my other hand holds an iota I can read, returns True. Otherwise, returns False.
$( mkIotaFragExpr
     "AuditorsReflection"
     [pattern| EAST aqqqqqe |]
     [[t|'[] -> '[IotaBoolean]|]]
     -- [''] -> ['bool']
 )

-- Like Auditor's Reflection, but the readability of an entity is checked instead of my other hand.
$( mkIotaFragExpr
     "AuditorsPurification"
     [pattern| EAST wawqwqwqwqwqwew |]
     [[t|'[IotaEntity] -> '[IotaBoolean]|]]
     -- ['entity'] -> ['bool']
 )

-- If I could save an iota into the item in my other hand, returns True. Otherwise, returns False.
$( mkIotaFragExpr
     "AssessorsReflection"
     [pattern| EAST deeeeeq |]
     [[t|'[] -> '[IotaBoolean]|]]
     -- [''] -> ['bool']
 )

-- Like Assessor's Reflection, but the writability of an entity is checked instead of my other hand.
$( mkIotaFragExpr
     "AssessorsPurification"
     [pattern| EAST wdwewewewewewqw |]
     [[t|'[IotaEntity] -> '[IotaBoolean]|]]
     -- ['entity'] -> ['bool']
 )

-- Removes the top iota from the stack, and saves it to my ravenmind, storing it there until I stop casting the Hex.
$( mkIotaFragExpr
     "HuginnsGambit"
     [pattern| NORTH_WEST eqqwawqaaw |]
     [[t|forall a. '[a] -> '[]|]]
     -- ['any'] -> ['']
 )

-- Copy the iota out of my ravenmind, which I likely just wrote with Huginn's Gambit.
$( mkIotaFragExpr
     "MuninnsReflection"
     [pattern| NORTH_EAST qeewdweddw |]
     [[t|'[] -> '[IotaAny]|]]
     -- [''] -> ['any']
 )

-- Advanced Mathematics

-- Takes the sine of an angle in radians, yielding the vertical component of that angle drawn on a unit circle. Related to the values of π and τ.
$( mkIotaFragExpr
     "SinePurification"
     [pattern| SOUTH_EAST qqqqqaa |]
     [[t|'[IotaNumber] -> '[IotaNumber]|]]
     -- ['num'] -> ['num']
 )

-- Takes the cosine of an angle in radians, yielding the horizontal component of that angle drawn on a unit circle. Related to the values of π and τ.
$( mkIotaFragExpr
     "CosinePurification"
     [pattern| SOUTH_EAST qqqqqad |]
     [[t|'[IotaNumber] -> '[IotaNumber]|]]
     -- ['num'] -> ['num']
 )

-- Takes the tangent of an angle in radians, yielding the slope of that angle drawn on a circle. Related to the values of π and τ.
$( mkIotaFragExpr
     "TangentPurification"
     [pattern| SOUTH_WEST wqqqqqadq |]
     [[t|'[IotaNumber] -> '[IotaNumber]|]]
     -- ['num'] -> ['num']
 )

-- Takes the inverse sine of a value with absolute value 1 or less, yielding the angle whose sine is that value. Related to the values of π and τ.
$( mkIotaFragExpr
     "InverseSinePurification"
     [pattern| SOUTH_EAST ddeeeee |]
     [[t|'[IotaNumber] -> '[IotaNumber]|]]
     -- ['num'] -> ['num']
 )

-- Takes the inverse cosine of a value with absolute value 1 or less, yielding the angle whose cosine is that value. Related to the values of π and τ.
$( mkIotaFragExpr
     "InverseCosinePurification"
     [pattern| NORTH_EAST adeeeee |]
     [[t|'[IotaNumber] -> '[IotaNumber]|]]
     -- ['num'] -> ['num']
 )

-- Takes the inverse tangent of a value, yielding the angle whose tangent is that value. Related to the values of π and τ.
$( mkIotaFragExpr
     "InverseTangentPurification"
     [pattern| NORTH_EAST eadeeeeew |]
     [[t|'[IotaNumber] -> '[IotaNumber]|]]
     -- ['num'] -> ['num']
 )

-- Takes the inverse tangent of a Y and X value, yielding the angle between the X-axis and a line from the origin to that point.
$( mkIotaFragExpr
     "InverseTangentDistillation"
     [pattern| WEST deadeeeeewd |]
     [[t|'[IotaNumber, IotaNumber] -> '[IotaNumber]|]]
     -- ['num', 'num'] -> ['num']
 )

-- Removes the number at the top of the stack, then takes the logarithm of the number at the top using the other number as its base. Related to the value of e.
$( mkIotaFragExpr
     "LogarithmicDistillation"
     [pattern| NORTH_WEST eqaqe |]
     [[t|'[IotaNumber, IotaNumber] -> '[IotaNumber]|]]
     -- ['num', 'num'] -> ['num']
 )

-- Sets

-- Unifies two sets.
$( mkFragExprInstance
     "DisjunctionDistillation"
     [ [t|'[IotaNumber, IotaNumber] -> '[IotaNumber]|]
     , [t|forall a. '[IotaList a, IotaList a] -> '[IotaList a]|]
     -- , IotaHList
     ]
     -- ['num', 'num']|['list', 'list'] -> ['num|list']
 )

-- Takes the intersection of two sets.
$( mkFragExprInstance
     "ConjunctionDistillation"
     [ [t|'[IotaNumber, IotaNumber] -> '[IotaNumber]|]
     , [t|forall a. '[IotaList a, IotaList a] -> '[IotaList a]|]
     -- , IotaHList
     ]
     -- ['num', 'num']|['list', 'list'] -> ['num|list']
 )

-- Takes the exclusive disjunction of two sets.
$( mkFragExprInstance
     "ExclusionDistillation"
     [ [t|'[IotaNumber, IotaNumber] -> '[IotaNumber]|]
     , [t|forall a. '[IotaList a, IotaList a] -> '[IotaList a]|]
     -- , IotaHList
     ]
     -- ['num', 'num']|['list', 'list'] -> ['num|list']
 )

-- Takes the inversion of a bitset, changing all "on" bits to "off" and vice versa. In my experience, this will take the form of that number negated and decreased by one. For example, 0 will become -1, and -100 will become 99.
$( mkFragExprInstance
     "NegationPurification"
     [[t|'[IotaNumber] -> '[IotaNumber]|]]
     -- ['num'] -> ['num']
 )

-- Removes duplicate entries from a list.
$( mkIotaFragExpr
     "UniquenessPurification"
     [pattern| NORTH_EAST aweaqa |]
     [[t|forall a. '[IotaList a] -> '[IotaList a]|]]
     -- ['list'] -> ['list']
 )

-- Meta-Evaluation

-- Remove a pattern or list of patterns from the stack, then cast them as if I had drawn them myself with my Staff (until a Charon's Gambit is encountered). If an iota is escaped with Consideration or its ilk, it will be pushed to the stack. Otherwise, non-patterns will fail.
iotaHermesGambit :: IotaPattern
iotaHermesGambit = IotaPattern [pattern| SOUTH_EAST deaqq |]

fragHermesGambit :: Fragment (IotaExec as as' ': as) as'
fragHermesGambit = fragSingleton iotaHermesGambit

-- Cast a pattern or list of patterns from the stack exactly like Hermes' Gambit, except that a unique "Jump" iota is pushed to the stack beforehand.
iotaIrisGambit :: IotaPattern
iotaIrisGambit = IotaPattern [pattern| NORTH_WEST qwaqde |]

fragIrisGambit :: Fragment (IotaExec (IotaExec as' bs ': as) as' ': as) as'
fragIrisGambit = fragSingleton iotaIrisGambit

-- Remove a list of patterns and a list from the stack, then cast the given pattern over each element of the second list.
$( mkIotaFragExpr
     "ThothsGambit"
     [pattern| NORTH_EAST dadad |]
     []
 )

instance FragThothsGambit (IotaList a ': IotaExec (a ': s) '[] ': s) (IotaHList '[] ': s)
instance FragThothsGambit (IotaList a ': IotaExec (a ': s) '[a'] ': s) (IotaList a' ': s)

type family FragThothsGambitHList a a' as where
  FragThothsGambitHList a a' '[] = '[]
  FragThothsGambitHList a a' (a ': as) = HAppendListR a' (FragThothsGambitHList a a' as)

instance (FragThothsGambitHList a a' as ~ r) => FragThothsGambit (IotaHList as ': IotaExec (a ': s) a' ': s) (IotaHList r ': s)

-- | More strictly typed version for better type deduction
fragThothsGambitEmpty :: Fragment (IotaList a ': IotaExec (a ': s) '[] ': s) (IotaHList '[] ': s)
fragThothsGambitEmpty = fragThothsGambit

-- | More strictly typed version for better type deduction
fragThothsGambitSingle :: Fragment (IotaList a ': IotaExec (a ': s) '[a'] ': s) (IotaList a' ': s)
fragThothsGambitSingle = fragThothsGambit

-- | More strictly typed version for better type deduction
fragThothsGambitHList :: Fragment (IotaHList as ': IotaExec (a ': s) a' ': s) (IotaHList (FragThothsGambitHList a a' as) ': s)
fragThothsGambitHList = fragThothsGambit

fragThothsGambitAny :: Fragment (IotaAnyList ': IotaExec (IotaAny ': s) a' ': s) (IotaAnyList ': s)
fragThothsGambitAny = fragSingleton iotaThothsGambit

-- This pattern forcibly halts a Hex. This is mostly useless on its own, as I could simply just stop writing patterns, or put down my staff.
iotaCharonsGambit :: IotaPattern
iotaCharonsGambit = IotaPattern [pattern| SOUTH_WEST aqdee |]

fragCharonsGambit :: Fragment as bs
fragCharonsGambit = fragSingleton iotaCharonsGambit

-- Adds the number of patterns a Hex is still capable of evaluating to the stack. This is reduced by one for each pattern cast by the Hex.
$( mkIotaFragExpr
     "ThanatosReflection"
     [pattern| SOUTH_EAST qqaed |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

-- Spell Circle Patterns

-- Returns the position of the Impetus of this spell circle.
$( mkIotaFragExpr
     "WaystoneReflection"
     [pattern| SOUTH_WEST eaqwqae |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

-- Returns the direction the Impetus of this spell circle is facing as a unit vector.
$( mkIotaFragExpr
     "LodestoneReflection"
     [pattern| SOUTH_WEST eaqwqaewede |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

-- Returns the position of the lower-north-west corner of the bounds of this spell circle.
$( mkIotaFragExpr
     "LesserFoldReflection"
     [pattern| SOUTH_WEST eaqwqaewdd |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

-- Returns the position of the upper-south-east corner of the bounds of this spell circle.
$( mkIotaFragExpr
     "GreaterFoldReflection"
     [pattern| WEST aqwqawaaqa |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

-- Akashic Patterns

-- Read the iota associated with the given pattern out of the Akashic Library with its Record at the given position. This has no range limit. Costs about one Amethyst Dust.
$( mkIotaFragExpr
     "AkashasDistillation"
     [pattern| WEST qqqwqqqqqaq |]
     [[t|'[IotaPattern, IotaVector] -> '[IotaAny]|]]
     -- ['pattern', 'vector'] -> ['any']
 )

-- Associate the iota with the given pattern in the Akashic Library with its Record at the given position. This does have a range limit. Costs about one Amethyst Dust.
$( mkIotaFragExpr
     "AkashasGambit"
     [pattern| EAST eeeweeeeede |]
     [[t|forall a. '[a, IotaPattern, IotaVector] -> '[]|]]
     -- ['any', 'pattern', 'vector'] -> ['']
 )

--- Spells

-- Basic Spells

-- Remove a number and vector from the stack, then create an explosion at the given location with the given power.
$( mkIotaFragExpr
     "Explosion"
     [pattern| EAST aawaawaa |]
     [[t|'[IotaNumber, IotaVector] -> '[]|]]
     -- ['number', 'vector'] -> ['']
 )

-- Remove a number and vector from the stack, then create a fiery explosion at the given location with the given power.
$( mkIotaFragExpr
     "Fireball"
     [pattern| EAST ddwddwdd |]
     [[t|'[IotaNumber, IotaVector] -> '[]|]]
     -- ['number', 'vector'] -> ['']
 )

-- Remove an entity and direction from the stack, then give a shove to the given entity in the given direction. The strength of the impulse is determined by the length of the vector.Costs units of Amethyst Dust equal to the square of the length of the vector, plus one for every Impulse except the first targeting an entity.
$( mkIotaFragExpr
     "Impulse"
     [pattern| SOUTH_WEST awqqqwaqw |]
     [[t|'[IotaVector, IotaEntity] -> '[]|]]
     -- ['vector', 'entity'] -> ['']
 )

-- Remove an entity and length from the stack, then teleport the given entity along its look vector by the given length.Costs about one Amethyst Shard per two blocks travelled.
$( mkIotaFragExpr
     "Blink"
     [pattern| SOUTH_WEST awqqqwaq |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'entity'] -> ['']
 )

-- Remove a vector and two numbers from the stack. Plays an instrument defined by the first number at the given location, with a note defined by the second number. Costs a negligible amount of media.
$( mkIotaFragExpr
     "MakeNote"
     [pattern| WEST adaa |]
     [[t|'[IotaNumber, IotaNumber, IotaVector] -> '[]|]]
     -- ['number', 'number', 'vector'] -> ['']
 )

-- Block Manipulation

-- Remove a location from the stack, then pick a block item and place it at the given location.Costs about an eighth of one Amethyst Dust.
$( mkIotaFragExpr
     "PlaceBlock"
     [pattern| SOUTH_WEST eeeeede |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Remove a location from the stack, then break the block at the given location. This spell can break nearly anything a Diamond Pickaxe can break.Costs about an eighth of one Amethyst Dust, or a negligible amount if breaking a Conjured Block or Conjured Light.
$( mkIotaFragExpr
     "BreakBlock"
     [pattern| EAST qaqqqqq |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Summon a block of water (or insert up to a bucket's worth) into a block at the given position. Costs about one Amethyst Dust.
$( mkIotaFragExpr
     "CreateWater"
     [pattern| SOUTH_EAST aqawqadaq |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Drains either a liquid container at, or a body of liquid around, the given position. Costs about two Charged Amethyst.
$( mkIotaFragExpr
     "DestroyLiquid"
     [pattern| SOUTH_WEST dedwedade |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Conjure an ethereal, but solid, block that sparkles with my pigment at the given position. Costs about one Amethyst Dust.
$( mkIotaFragExpr
     "ConjureBlock"
     [pattern| NORTH_EAST qqa |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Conjure a magical light that softly glows with my pigment at the given position. Costs about one Amethyst Dust.
$( mkIotaFragExpr
     "ConjureLight"
     [pattern| NORTH_EAST qqd |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Encourage a plant or sapling at the target position to grow, as if Bonemeal was applied. Costs a bit more than one Amethyst Dust.
$( mkIotaFragExpr
     "Overgrow"
     [pattern| NORTH_EAST wqaqwawqaqw |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Forcibly infuse media into the sapling at the target position, causing it to grow into an Edified Tree. Costs about one Charged Amethyst.
$( mkIotaFragExpr
     "EdifySapling"
     [pattern| NORTH_EAST wqaqwd |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Start a fire on top of the given location, as if a Fire Charge was applied, or set fire to an entity. Costs about one Amethyst Dust.
$( mkIotaFragExpr
     "Ignite"
     [pattern| SOUTH_EAST aaqawawa |]
     [ [t|'[IotaEntity] -> '[]|]
     , [t|'[IotaVector] -> '[]|]
     ]
     -- ['entity | vector'] -> ['']
 )

-- Extinguish blocks in a large area. Costs about six Amethyst Dust.
$( mkIotaFragExpr
     "ExtinguishArea"
     [pattern| SOUTH_WEST ddedwdwd |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Nadirs

-- Inflicts weakness. Base cost is one Amethyst Dust per 10 seconds.
$( mkIotaFragExpr
     "WhiteSunsNadir"
     [pattern| NORTH_WEST qqqqqaqwawaw |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

-- Inflicts levitation. Base cost is one Amethyst Dust per 5 seconds.
$( mkIotaFragExpr
     "BlueSunsNadir"
     [pattern| WEST qqqqqawwawawd |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'entity'] -> ['']
 )

-- Inflicts withering. Base cost is one Amethyst Dust per second.
$( mkIotaFragExpr
     "BlackSunsNadir"
     [pattern| SOUTH_WEST qqqqqaewawawe |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

-- Inflicts poison. Base cost is one Amethyst Dust per 3 seconds.
$( mkIotaFragExpr
     "RedSunsNadir"
     [pattern| SOUTH_EAST qqqqqadwawaww |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

-- Inflicts slowness. Base cost is one Amethyst Dust per 5 seconds.
$( mkIotaFragExpr
     "GreenSunsNadir"
     [pattern| SOUTH_EAST qqqqqadwawaw |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

-- Crafting Casting Items

-- Costs about one Charged Amethyst.
$( mkIotaFragExpr
     "CraftCypher"
     [pattern| EAST waqqqqq |]
     [[t|forall s. '[IotaExec '[] s, IotaEntity] -> '[]|]]
     -- ['[pattern]', 'entity'] -> ['']
 )

-- Costs about five Charged Amethysts.
$( mkIotaFragExpr
     "CraftTrinket"
     [pattern| EAST wwaqqqqqeaqeaeqqqeaeq |]
     [[t|forall s. '[IotaExec '[] s, IotaEntity] -> '[]|]]
     -- ['[pattern]', 'entity'] -> ['']
 )

-- Costs about ten Charged Amethysts.
$( mkIotaFragExpr
     "CraftArtifact"
     [pattern| EAST wwaqqqqqeawqwqwqwqwqwwqqeadaeqqeqqeadaeqq |]
     [[t|forall s. '[IotaExec '[] s, IotaEntity] -> '[]|]]
     -- ['[pattern]', 'entity'] -> ['']
 )

-- Recharge a media-containing item in my other hand. Costs about one Amethyst Shard per item.
$( mkIotaFragExpr
     "RechargeItem"
     [pattern| NORTH_WEST qqqqqwaeaeaeaeaea |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['entity'] -> ['']
 )

-- Clears Hex-containing or iota-containing items in my other hand. Costs about one Amethyst Dust per item.
$( mkIotaFragExpr
     "EraseItem"
     [pattern| EAST qdqawwaww |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Sentinels

-- Summons my sentinel at the given position. Costs about one Amethyst Dust.
$( mkIotaFragExpr
     "SummonSentinel"
     [pattern| EAST waeawae |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Banish my sentinel, and remove it from the world. Costs a negligible amount of media.
$( mkIotaFragExpr
     "BanishSentinel"
     [pattern| NORTH_EAST qdwdqdw |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Add the position of my sentinel to the stack, or Null if it isn't summoned. Costs a negligible amount of media.
$( mkIotaFragExpr
     "LocateSentinel"
     [pattern| EAST waeawaede |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

-- Transform the position vector on the top of the stack into a unit vector pointing from that position to my sentinel, or Null if it isn't summoned. Costs a negligible amount of media.
$( mkIotaFragExpr
     "WayfindSentinel"
     [pattern| EAST waeawaedwa |]
     [[t|'[IotaVector] -> '[IotaVector]|]]
     -- ['vector'] -> ['vector']
 )

-- Internalize Pigment

-- I must be holding a Pigment in my other hand to cast this spell. When I do, it will consume the dye and permanently change my mind's coloration (at least, until I cast the spell again). Costs about one Amethyst Dust.
$( mkIotaFragExpr
     "InternalizePigment"
     [pattern| EAST awddwqawqwawq |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Caster's Glamour

-- Certain items I create seem oddly receptive to the influence of media. By holding a Cypher, Trinket, Artifact, Focus, or Spellbook in my other hand, I can use this spell to change the appearance of the item. Costs about one Amethyst Dust.
$( mkIotaFragExpr
     "CastersGlamour"
     [pattern| WEST dwaawedwewdwe |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Flight

-- A flight limited in its range.
$( mkIotaFragExpr
     "AnchoritesFlight"
     [pattern| SOUTH_WEST awawaawq |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'entity'] -> ['']
 )

-- A flight limited in its duration.
$( mkIotaFragExpr
     "WayfarersFlight"
     [pattern| NORTH_EAST dwdwdewq |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'entity'] -> ['']
 )

-- Returns whether the given player is under the effects of Anchorite's or Wayfarer's Flight.
$( mkIotaFragExpr
     "AviatorsPurification"
     [pattern| NORTH_EAST dwdwdeweaqa |]
     [[t|'[IotaEntity] -> '[IotaBoolean]|]]
     -- ['entity'] -> ['boolean']
 )

--- Great Spells

-- Create Lava

-- Summon a block of lava (or insert up to a bucket's worth) into a block at the given position. Costs about one Charged Amethyst.
$( mkGreatIotaFragExpr
     "CreateLava"
     "Create Lava"
     [pattern| EAST eaqawqadaqd |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Weather Manipulation

-- I command the heavens! This spell will summon a bolt of lightning to strike the earth where I direct it. Costs about three Amethyst Shards.
$( mkGreatIotaFragExpr
     "SummonLightning"
     "Summon Lightning"
     [pattern| EAST waadwawdaaweewq |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- I control the clouds! This spell will summon rain across the world I cast it upon. Costs about one Charged Amethyst. Does nothing if it is already raining.
$( mkGreatIotaFragExpr
     "SummonRain"
     "Summon Rain"
     [pattern| WEST wwweeewwweewdawdwad |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- A counterpart to summoning rain. This spell will dispel rain across the world I cast it upon. Costs about one Amethyst Shard. Does nothing if the skies are already clear.
$( mkGreatIotaFragExpr
     "DispelRain"
     "Dispel Rain"
     [pattern| EAST eeewwweeewwaqqddqdqd |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Altiora

-- Summon a sheaf of media about me in the shape of wings, endowed with enough substance to allow gliding.
$( mkGreatIotaFragExpr
     "Altiora"
     "Altiora"
     [pattern| NORTH_WEST eawwaeawawaa |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['player'] -> ['']
 )

-- Greater Teleport

-- Far more powerful than Blink, this spell lets me teleport nearly anywhere in the entire world! There does seem to be a limit, but it is much greater than the normal radius of influence I am used to.
$( mkGreatIotaFragExpr
     "GreaterTeleport"
     "Greater Teleport"
     [pattern| EAST wwwqqqwwwqqeqqwwwqqwqqdqqqqqdqq |]
     [[t|'[IotaVector, IotaEntity] -> '[]|]]
     -- ['vector', 'entity'] -> ['']
 )

-- Zeniths

-- Bestows regeneration. Base cost is one Amethyst Dust per second.
$( mkGreatIotaFragExpr
     "WhiteSunsZenith"
     "White Sun's Zenith"
     [pattern| NORTH_WEST qqqqaawawaedd |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

-- Bestows night vision. Base cost is one Amethyst Dust per 5 seconds.
$( mkGreatIotaFragExpr
     "BlueSunsZenith"
     "Blue Sun's Zenith"
     [pattern| WEST qqqaawawaeqdd |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'entity'] -> ['']
 )

-- Bestows absorption. Base cost is one Amethyst Dust per second.
$( mkGreatIotaFragExpr
     "BlackSunsZenith"
     "Black Sun's Zenith"
     [pattern| SOUTH_WEST qqaawawaeqqdd |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

-- Bestows haste. Base cost is one Amethyst Dust per 3 seconds.
$( mkGreatIotaFragExpr
     "RedSunsZenith"
     "Red Sun's Zenith"
     [pattern| SOUTH_EAST qaawawaeqqqdd |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

-- Bestows strength. Base cost is one Amethyst Dust per 3 seconds.
$( mkGreatIotaFragExpr
     "GreenSunsZenith"
     "Green Sun's Zenith"
     [pattern| EAST aawawaeqqqqdd |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

-- Summon Greater Sentinel

-- Summon a greater version of my Sentinel. Costs about two Amethyst Dust.
$( mkGreatIotaFragExpr
     "SummonGreaterSentinel"
     "Summon Greater Sentinel"
     [pattern| EAST waeawaeqqqwqwqqwq |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Craft Phial

-- Infuse a bottle with media to form a Phial.
$( mkGreatIotaFragExpr
     "CraftPhial"
     "Craft Phial"
     [pattern| SOUTH_WEST aqqqaqwwaqqqqqeqaqqqawwqwqwqwqwqw |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['entity'] -> ['']
 )

-- Flay Mind

-- I cannot make heads or tails of this spell... To be honest, I'm not sure I want to know what it does.
$( mkGreatIotaFragExpr
     "FlayMind"
     "Flay Mind"
     [pattern| NORTH_EAST qeqwqwqwqwqeqaeqeaqeqaeqaqded |]
     [[t|'[IotaVector, IotaEntity] -> '[]|]]
     -- ['vector', 'entity'] -> ['']
 )

-- special

-- An infinite family of actions that keep or remove elements at the top of the stack based on the sequence of dips and lines.
--
-- For this DSL, the *first* element of the input list corresponds to the *top* element of the stack
iotaBookkeepersGambit :: NonEmpty Bool -> IotaPattern
iotaBookkeepersGambit (b :| bs) = IotaPattern $ go (b :| bs)
 where
  cat (Pattern dir ang) ang' = Pattern dir (ang <> ang')
  go (False :| []) = [pattern| SOUTH_EAST a |]
  go (True :| []) = [pattern| EAST |]
  go (False :| (False : as)) = go (False :| as) `cat` [angles| da |]
  go (True :| (False : as)) = go (False :| as) `cat` [angles| e |]
  go (False :| (True : as)) = go (True :| as) `cat` [angles| ea |]
  go (True :| (True : as)) = go (True :| as) `cat` [angles| w |]

class FragBookkeepersGambit keep as bs | keep as -> bs where
  fragBookkeepersGambitKeepList :: NonEmpty Bool
  fragBookkeepersGambit :: Fragment as bs
  fragBookkeepersGambit = fragSingleton $ iotaBookkeepersGambit $ fragBookkeepersGambitKeepList @keep @as @bs
instance {-# OVERLAPPING #-} FragBookkeepersGambit '[False] (a ': as) as where
  fragBookkeepersGambitKeepList = False :| []
instance {-# OVERLAPPING #-} FragBookkeepersGambit '[True] (a ': as) (a ': as) where
  fragBookkeepersGambitKeepList = True :| []
instance FragBookkeepersGambit keep as bs => FragBookkeepersGambit (False ': keep) (a ': as) bs where
  fragBookkeepersGambitKeepList = False :| (toList $ fragBookkeepersGambitKeepList @keep @as @bs)
instance FragBookkeepersGambit keep as bs => FragBookkeepersGambit (True ': keep) (a ': as) (a ': bs) where
  fragBookkeepersGambitKeepList = True :| (toList $ fragBookkeepersGambitKeepList @keep @as @bs)

precomputedNumericalReflectionSuffixes :: Seq [Angle]
precomputedNumericalReflectionSuffixes = Seq.fromList $ [] : suffixes
 where
  raw = $(embedFileRelative "precomputed_numbers.txt")
  rawLines = filter (not . BC.null) $ map (BC.strip) $ BC.split '\n' raw
  suffixes = map (parseAngles . BC.unpack) rawLines
  parseAngles as = fromMaybe (error $ "invalid angles: '" <> as <> "'") $ traverse angleParse as

-- Add a number to the stack
iotaMaybeNumericalReflection :: Int -> Maybe IotaPattern
iotaMaybeNumericalReflection n = IotaPattern . Pattern dir . (ang <>) <$> suffix
 where
  Pattern dirPos angPos = [pattern| NORTH_EAST aqaa |]
  Pattern dirNeg angNeg = [pattern| SOUTH_EAST dedd |]
  (dir, ang) = if n >= 0 then (dirPos, angPos) else (dirNeg, angNeg)
  suffix = precomputedNumericalReflectionSuffixes Seq.!? (abs n)

iotaNumericalReflection :: Int -> IotaPattern
iotaNumericalReflection = fromMaybe err . iotaMaybeNumericalReflection
 where
  err = error "number too large for numerical reflection"

fragNumericalReflection :: Int -> Fragment as (IotaNumber ': as)
fragNumericalReflection = fragSingleton . iotaNumericalReflection

exprNumericalReflection :: Int -> Expr blk '[IotaNumber]
exprNumericalReflection n = E.intro $ fragNumericalReflection n
