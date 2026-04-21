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
  IotaVector,
 )
import Haskcasting.Pattern (Angle, Pattern (Pattern), angleParse, angles, pattern)
import Haskcasting.Util (HListLen)

--- Cross-Mod Compatibility

-- Pehkui

$( mkIotaFragExpr
     "GulliversPurification"
     [pattern| NORTH_WEST aawawwawwa |]
     [[t|'[IotaEntity] -> '[IotaNumber]|]]
     -- ['entity'] -> ['num']
 )

$( mkIotaFragExpr
     "AlterScale"
     [pattern| NORTH_EAST ddwdwwdwwd |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['num', 'entity'] -> ['']
 )

$( mkIotaFragExpr
     "MindsReflection"
     [pattern| NORTH_EAST qaq |]
     [ [t|'[] -> '[IotaEntity]|]
     ]
     -- [''] -> ['entity | null']
 )

--- Patterns

-- Basic Patterns

$( mkIotaFragExpr
     "CompassPurification"
     [pattern| EAST aa |]
     [[t|'[IotaEntity] -> '[IotaVector]|]]
     -- ['entity'] -> ['vector']
 )

$( mkIotaFragExpr
     "CompassPurificationII"
     [pattern| NORTH_EAST dd |]
     [[t|'[IotaEntity] -> '[IotaVector]|]]
     -- ['entity'] -> ['vector']
 )

$( mkIotaFragExpr
     "AlidadesPurification"
     [pattern| EAST wa |]
     [[t|'[IotaEntity] -> '[IotaVector]|]]
     -- ['entity'] -> ['vector']
 )

$( mkIotaFragExpr
     "ArchersDistillation"
     [pattern| EAST wqaawdd |]
     [ [t|'[IotaVector, IotaVector] -> '[IotaVector]|]
     ]
     -- ['vector', 'vector'] -> ['vector | null']
 )

$( mkIotaFragExpr
     "ArchitectsDistillation"
     [pattern| EAST weddwaa |]
     [ [t|'[IotaVector, IotaVector] -> '[IotaVector]|]
     ]
     -- ['vector', 'vector'] -> ['vector | null']
 )

$( mkIotaFragExpr
     "ScoutsDistillation"
     [pattern| EAST weaqa |]
     [ [t|'[IotaVector, IotaVector] -> '[IotaEntity]|]
     ]
     -- ['vector', 'vector'] -> ['entity | null']
 )

$( mkIotaFragExpr
     "Reveal"
     [pattern| NORTH_EAST de |]
     [[t|forall a. '[a] -> '[a]|]]
     -- ['any'] -> ['any']
 )

$( mkIotaFragExpr
     "StadiometersPurification"
     [pattern| NORTH_EAST awq |]
     [[t|'[IotaEntity] -> '[IotaNumber]|]]
     -- ['entity'] -> ['num']
 )

$( mkIotaFragExpr
     "PacePurification"
     [pattern| EAST wq |]
     [[t|'[IotaEntity] -> '[IotaVector]|]]
     -- ['entity'] -> ['vector']
 )

-- Mathematics

$( mkIotaFragExpr
     "AdditiveDistillation"
     [pattern| NORTH_EAST waaw |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
 )

$( mkIotaFragExpr
     "SubtractiveDistillation"
     [pattern| NORTH_WEST wddw |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
 )

$( mkIotaFragExpr
     "MultiplicativeDistillation"
     [pattern| SOUTH_EAST waqaw |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaNumber]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
 )

$( mkIotaFragExpr
     "DivisionDistillation"
     [pattern| NORTH_EAST wdedw |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
 )

$( mkIotaFragExpr
     "LengthPurification"
     [pattern| NORTH_EAST wqaqw |]
     [ [t|'[IotaNumber] -> '[IotaNumber]|]
     , [t|'[IotaVector] -> '[IotaNumber]|]
     ]
     -- ['num|vec'] -> ['number']
 )

$( mkIotaFragExpr
     "PowerDistillation"
     [pattern| NORTH_WEST wedew |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
 )

$( mkIotaFragExpr
     "FloorPurification"
     [pattern| EAST ewq |]
     [ [t|Fragment '[IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector] '[IotaVector]|]
     ]
 )

$( mkIotaFragExpr
     "CeilingPurification"
     [pattern| EAST qwe |]
     [ [t|Fragment '[IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector] '[IotaVector]|]
     ]
 )

$( mkIotaFragExpr
     "VectorExaltation"
     [pattern| EAST eqqqqq |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaNumber] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "VectorDisintegration"
     [pattern| EAST qeeeee |]
     [[t|Fragment '[IotaVector] '[IotaNumber, IotaNumber, IotaNumber]|]]
 )

$( mkIotaFragExpr
     "ModulusDistillation"
     [pattern| NORTH_EAST addwaad |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
 )

$( mkIotaFragExpr
     "AxialPurification"
     [pattern| NORTH_WEST qqqqqaww |]
     [ [t|Fragment '[IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector] '[IotaVector]|]
     ]
 )

$( mkIotaFragExpr
     "EntropyReflection"
     [pattern| NORTH_WEST eqqq |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['num']
 )

-- Constants

$( mkIotaFragExpr
     "TrueReflection"
     [pattern| SOUTH_EAST aqae |]
     [[t|'[] -> '[IotaBoolean]|]]
     -- [''] -> ['bool']
 )

$( mkIotaFragExpr
     "FalseReflection"
     [pattern| NORTH_EAST dedq |]
     [[t|'[] -> '[IotaBoolean]|]]
     -- [''] -> ['bool']
 )

$( mkIotaFragExpr
     "NullaryReflection"
     [pattern| EAST d |]
     [[t|'[] -> '[IotaNull]|]]
     -- [''] -> ['null']
 )

$( mkIotaFragExpr
     "VectorReflectionZero"
     [pattern| NORTH_WEST qqqqq |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

$( mkIotaFragExpr
     "VectorReflectionPX"
     [pattern| NORTH_WEST qqqqqea |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "VectorReflectionPY"
     [pattern| NORTH_WEST qqqqqew |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "VectorReflectionPZ"
     [pattern| NORTH_WEST qqqqqed |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "VectorReflectionNX"
     [pattern| SOUTH_WEST eeeeeqa |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "VectorReflectionNY"
     [pattern| SOUTH_WEST eeeeeqw |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "VectorReflectionNZ"
     [pattern| SOUTH_WEST eeeeeqd |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "CirclesReflection"
     [pattern| NORTH_WEST eawae |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['num']
 )

$( mkIotaFragExpr
     "ArcsReflection"
     [pattern| NORTH_EAST qdwdq |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['num']
 )

$( mkIotaFragExpr
     "EulersReflection"
     [pattern| EAST aaq |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['num']
 )

-- Stack Manipulation

$( mkIotaFragExpr
     "JestersGambit"
     [pattern| EAST aawdd |]
     [[t|forall a b. Fragment '[a, b] '[b, a]|]]
 )

$( mkIotaFragExpr
     "RotationGambit"
     [pattern| EAST aaeaa |]
     [[t|forall a b c. Fragment '[a, b, c] '[c, a, b]|]]
 )

$( mkIotaFragExpr
     "RotationGambitII"
     [pattern| NORTH_EAST ddqdd |]
     [[t|forall a b c. Fragment '[a, b, c] '[b, c, a]|]]
 )

$( mkIotaFragExpr
     "GeminiDecomposition"
     [pattern| EAST aadaa |]
     [[t|forall a. Fragment '[a] '[a, a]|]]
 )

$( mkIotaFragExpr
     "ProspectorsGambit"
     [pattern| EAST aaedd |]
     [[t|forall a b. Fragment '[a, b] '[b, a, b]|]]
 )

$( mkIotaFragExpr
     "UndertakersGambit"
     [pattern| EAST ddqaa |]
     [[t|forall a b. Fragment '[a, b] '[a, b, a]|]]
 )

iotaGeminiGambit :: IotaPattern
iotaGeminiGambit = IotaPattern [pattern| EAST aadaadaa |]

$( mkIotaFragExpr
     "DioscuriGambit"
     [pattern| EAST aadadaaw |]
     [[t|forall a b. Fragment '[a, b] '[a, b, a, b]|]]
 )

$( mkIotaFragExpr
     "FlocksReflection"
     [pattern| NORTH_WEST qwaeawqaeaqa |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

iotaFishermansGambit, iotaFishermansGambitII, iotaSwindlersGambit :: IotaPattern
iotaFishermansGambit = IotaPattern [pattern| WEST ddad |]
iotaFishermansGambitII = IotaPattern [pattern| EAST aada |]
iotaSwindlersGambit = IotaPattern [pattern| SOUTH_EAST qaawdde |]

-- Logical Operators

$( mkIotaFragExpr
     "AugursPurification"
     [pattern| NORTH_EAST aw |]
     [[t|forall a. '[a] -> '[IotaBoolean]|]]
     -- ['any'] -> ['bool']
 )

$( mkFragExprInstance
     "LengthPurification"
     [[t|'[IotaBoolean] -> '[IotaNumber]|]]
     -- ['bool'] -> ['number']
 )

$( mkIotaFragExpr
     "NegationPurification"
     [pattern| NORTH_WEST dw |]
     [[t|'[IotaBoolean] -> '[IotaBoolean]|]]
     -- ['bool'] -> ['bool']
 )

$( mkIotaFragExpr
     "DisjunctionDistillation"
     [pattern| SOUTH_EAST waw |]
     [[t|'[IotaBoolean, IotaBoolean] -> '[IotaBoolean]|]]
     -- ['bool', 'bool'] -> ['bool']
 )

$( mkIotaFragExpr
     "ConjunctionDistillation"
     [pattern| NORTH_EAST wdw |]
     [[t|'[IotaBoolean, IotaBoolean] -> '[IotaBoolean]|]]
     -- ['bool', 'bool'] -> ['bool']
 )

$( mkIotaFragExpr
     "ExclusionDistillation"
     [pattern| NORTH_WEST dwa |]
     [[t|'[IotaBoolean, IotaBoolean] -> '[IotaBoolean]|]]
     -- ['bool', 'bool'] -> ['bool']
 )

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

$( mkIotaFragExpr
     "InequalityDistillation"
     [pattern| EAST da |]
     [[t|forall a b. '[a, b] -> '[IotaBoolean]|]]
     -- ['any', 'any'] -> ['bool']
 )

$( mkIotaFragExpr
     "MaximusDistillation"
     [pattern| SOUTH_EAST e |]
     [[t|'[IotaNumber, IotaNumber] -> '[IotaBoolean]|]]
     -- ['number', 'number'] -> ['bool']
 )

$( mkIotaFragExpr
     "MinimusDistillation"
     [pattern| SOUTH_WEST q |]
     [[t|'[IotaNumber, IotaNumber] -> '[IotaBoolean]|]]
     -- ['number', 'number'] -> ['bool']
 )

$( mkIotaFragExpr
     "MaximusDistillationII"
     [pattern| SOUTH_EAST ee |]
     [[t|'[IotaNumber, IotaNumber] -> '[IotaBoolean]|]]
     -- ['number', 'number'] -> ['bool']
 )

$( mkIotaFragExpr
     "MinimusDistillationII"
     [pattern| SOUTH_WEST qq |]
     [[t|'[IotaNumber, IotaNumber] -> '[IotaBoolean]|]]
     -- ['number', 'number'] -> ['bool']
 )

-- Entities

$( mkIotaFragExpr
     "EntityPurification"
     [pattern| SOUTH_EAST qqqqqdaqa |]
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkIotaFragExpr
     "EntityPurificationAnimal"
     [pattern| SOUTH_EAST qqqqqdaqaawa |]
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkIotaFragExpr
     "EntityPurificationMonster"
     [pattern| SOUTH_EAST qqqqqdaqaawq |]
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkIotaFragExpr
     "EntityPurificationItem"
     [pattern| SOUTH_EAST qqqqqdaqaaww |]
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkIotaFragExpr
     "EntityPurificationPlayer"
     [pattern| SOUTH_EAST qqqqqdaqaawe |]
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkIotaFragExpr
     "EntityPurificationLiving"
     [pattern| SOUTH_EAST qqqqqdaqaawd |]
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkIotaFragExpr
     "ZoneDistillationAnimal"
     [pattern| SOUTH_EAST qqqqqwdeddwa |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFragExpr
     "ZoneDistillationNonAnimal"
     [pattern| NORTH_EAST eeeeewaqaawa |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFragExpr
     "ZoneDistillationMonster"
     [pattern| SOUTH_EAST qqqqqwdeddwq |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFragExpr
     "ZoneDistillationNonMonster"
     [pattern| NORTH_EAST eeeeewaqaawq |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFragExpr
     "ZoneDistillationItem"
     [pattern| SOUTH_EAST qqqqqwdeddww |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFragExpr
     "ZoneDistillationNonItem"
     [pattern| NORTH_EAST eeeeewaqaaww |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFragExpr
     "ZoneDistillationPlayer"
     [pattern| SOUTH_EAST qqqqqwdeddwe |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFragExpr
     "ZoneDistillationNonPlayer"
     [pattern| NORTH_EAST eeeeewaqaawe |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFragExpr
     "ZoneDistillationLiving"
     [pattern| SOUTH_EAST qqqqqwdeddwd |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFragExpr
     "ZoneDistillationNonLiving"
     [pattern| NORTH_EAST eeeeewaqaawd |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFragExpr
     "ZoneDistillationAny"
     [pattern| SOUTH_EAST qqqqqwded |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

-- List Manipulation

$( mkIotaFragExpr
     "SelectionDistillation"
     [pattern| NORTH_WEST deeed |]
     [[t|forall a. Fragment '[IotaNumber, IotaList a] '[a]|]]
 )

$( mkIotaFragExpr
     "SelectionExaltation"
     [pattern| NORTH_WEST qaeaqwded |]
     [[t|forall a. '[IotaNumber, IotaNumber, IotaList a] -> '[IotaList a]|]]
 )

$( mkIotaFragExpr
     "IntegrationDistillation"
     [pattern| SOUTH_WEST edqde |]
     [ [t|forall a. Fragment '[a, IotaList a] '[IotaList a]|]
     , [t|forall a as as' p. (HReverse (a : p) as', HReverse p as) => Fragment '[a, IotaHList as] '[IotaHList as']|]
     ]
 )

$( mkIotaFragExpr
     "DerivationDecomposition"
     [pattern| NORTH_WEST qaeaq |]
     [ [t|forall a. Fragment '[IotaList a] '[a, IotaList a]|]
     , [t|forall a as as' p. (HReverse (a : p) as', HReverse p as) => Fragment '[IotaHList as'] '[a, IotaHList as]|]
     ]
 )

$( mkFragExprInstance
     "AdditiveDistillation"
     [ [t|forall as bs asbs. HAppendListR as bs ~ asbs => Fragment '[IotaHList bs, IotaHList as] '[IotaHList asbs]|]
     , [t|forall a. Fragment '[IotaList a, IotaList a] '[IotaList a]|]
     ]
 )

$( mkIotaFragExpr
     "VacantReflection"
     [pattern| NORTH_EAST qqaeaae |]
     [[t|'[] -> '[IotaHList '[]]|]]
 )

$( mkIotaFragExpr
     "SinglesPurification"
     [pattern| EAST adeeed |]
     [[t|forall a. Fragment '[a] '[IotaHList '[a]]|]]
 )

$( mkFragExprInstance
     "LengthPurification"
     [ [t|forall a. Fragment '[IotaList a] '[IotaNumber]|]
     , [t|forall as. Fragment '[IotaHList as] '[IotaNumber]|]
     ]
 )

$( mkIotaFragExpr
     "RetrogradePurification"
     [pattern| EAST qqqaede |]
     [ [t|forall a. Fragment '[IotaList a] '[IotaList a]|]
     , [t|forall as as'. HReverse as as' => Fragment '[IotaHList as] '[IotaHList as']|]
     ]
 )

$( mkIotaFragExpr
     "LocatorsDistillation"
     [pattern| EAST dedqde |]
     [[t|forall a. Fragment '[a, IotaList a] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "ExcisorsDistillation"
     [pattern| SOUTH_WEST edqdewaqa |]
     [[t|forall a. Fragment '[IotaNumber, IotaList a] '[IotaList a]|]]
 )

$( mkIotaFragExpr
     "SurgeonsExaltation"
     [pattern| NORTH_WEST wqaeaqw |]
     [[t|forall a. Fragment '[a, IotaNumber, IotaList a] '[IotaList a]|]]
 )

iotaFlocksGambit :: IotaPattern
iotaFlocksGambit = IotaPattern [pattern| SOUTH_WEST ewdqdwe |]

$( mkIotaFragExpr
     "FlocksDisintegration"
     [pattern| NORTH_WEST qwaeawq |]
     []
 )

instance (HReverse as ras, HAppendFD ras bs rasbs) => FragFlocksDisintegration (IotaHList as ': bs) rasbs
instance (HReverse as ras, HListLen ras) => ExprFlocksDisintegration '[IotaHList as] ras

$( mkIotaFragExpr
     "SpeakersDistillation"
     [pattern| SOUTH_EAST ddewedd |]
     [ [t|forall a. Fragment '[a, IotaList a] '[IotaList a]|]
     , [t|forall a as. Fragment '[a, IotaHList as] '[IotaHList (a ': as)]|]
     ]
 )

$( mkIotaFragExpr
     "SpeakersDecomposition"
     [pattern| SOUTH_WEST aaqwqaa |]
     [ [t|forall a. Fragment '[IotaList a] '[a, IotaList a]|]
     , [t|forall a as as'. as' ~ a ': as => Fragment '[IotaHList as'] '[a, IotaHList as]|]
     ]
 )

-- Reading and Writing

$( mkIotaFragExpr
     "ScribesReflection"
     [pattern| EAST aqqqqq |]
     [[t|'[] -> '[IotaAny]|]]
     -- [''] -> ['any']
 )

$( mkIotaFragExpr
     "ScribesGambit"
     [pattern| EAST deeeee |]
     [[t|forall a. '[a] -> '[]|]]
     -- ['any'] -> ['']
 )

$( mkIotaFragExpr
     "ChroniclersPurification"
     [pattern| EAST wawqwqwqwqwqw |]
     [[t|'[IotaEntity] -> '[IotaAny]|]]
     -- ['entity'] -> ['any']
 )

$( mkIotaFragExpr
     "ChroniclersGambit"
     [pattern| EAST wdwewewewewew |]
     [[t|forall a. '[a, IotaEntity] -> '[]|]]
     -- ['any', 'entity'] -> ['']
 )

$( mkIotaFragExpr
     "AuditorsReflection"
     [pattern| EAST aqqqqqe |]
     [[t|'[] -> '[IotaBoolean]|]]
     -- [''] -> ['bool']
 )

$( mkIotaFragExpr
     "AuditorsPurification"
     [pattern| EAST wawqwqwqwqwqwew |]
     [[t|'[IotaEntity] -> '[IotaBoolean]|]]
     -- ['entity'] -> ['bool']
 )

$( mkIotaFragExpr
     "AssessorsReflection"
     [pattern| EAST deeeeeq |]
     [[t|'[] -> '[IotaBoolean]|]]
     -- [''] -> ['bool']
 )

$( mkIotaFragExpr
     "AssessorsPurification"
     [pattern| EAST wdwewewewewewqw |]
     [[t|'[IotaEntity] -> '[IotaBoolean]|]]
     -- ['entity'] -> ['bool']
 )

$( mkIotaFragExpr
     "HuginnsGambit"
     [pattern| NORTH_WEST eqqwawqaaw |]
     [[t|forall a. '[a] -> '[]|]]
     -- ['any'] -> ['']
 )

$( mkIotaFragExpr
     "MuninnsReflection"
     [pattern| NORTH_EAST qeewdweddw |]
     [[t|'[] -> '[IotaAny]|]]
     -- [''] -> ['any']
 )

-- Advanced Mathematics

$( mkIotaFragExpr
     "SinePurification"
     [pattern| SOUTH_EAST qqqqqaa |]
     [[t|'[IotaNumber] -> '[IotaNumber]|]]
     -- ['num'] -> ['num']
 )

$( mkIotaFragExpr
     "CosinePurification"
     [pattern| SOUTH_EAST qqqqqad |]
     [[t|'[IotaNumber] -> '[IotaNumber]|]]
     -- ['num'] -> ['num']
 )

$( mkIotaFragExpr
     "TangentPurification"
     [pattern| SOUTH_WEST wqqqqqadq |]
     [[t|'[IotaNumber] -> '[IotaNumber]|]]
     -- ['num'] -> ['num']
 )

$( mkIotaFragExpr
     "InverseSinePurification"
     [pattern| SOUTH_EAST ddeeeee |]
     [[t|'[IotaNumber] -> '[IotaNumber]|]]
     -- ['num'] -> ['num']
 )

$( mkIotaFragExpr
     "InverseCosinePurification"
     [pattern| NORTH_EAST adeeeee |]
     [[t|'[IotaNumber] -> '[IotaNumber]|]]
     -- ['num'] -> ['num']
 )

$( mkIotaFragExpr
     "InverseTangentPurification"
     [pattern| NORTH_EAST eadeeeeew |]
     [[t|'[IotaNumber] -> '[IotaNumber]|]]
     -- ['num'] -> ['num']
 )

$( mkIotaFragExpr
     "InverseTangentDistillation"
     [pattern| WEST deadeeeeewd |]
     [[t|'[IotaNumber, IotaNumber] -> '[IotaNumber]|]]
     -- ['num', 'num'] -> ['num']
 )

$( mkIotaFragExpr
     "LogarithmicDistillation"
     [pattern| NORTH_WEST eqaqe |]
     [[t|'[IotaNumber, IotaNumber] -> '[IotaNumber]|]]
     -- ['num', 'num'] -> ['num']
 )

-- Sets

$( mkFragExprInstance
     "DisjunctionDistillation"
     [ [t|'[IotaNumber, IotaNumber] -> '[IotaNumber]|]
     , [t|forall a. '[IotaList a, IotaList a] -> '[IotaList a]|]
     -- , IotaHList
     ]
 )

$( mkFragExprInstance
     "ConjunctionDistillation"
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|forall a. Fragment '[IotaList a, IotaList a] '[IotaList a]|]
     -- , IotaHList
     ]
 )

$( mkFragExprInstance
     "ExclusionDistillation"
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|forall a. Fragment '[IotaList a, IotaList a] '[IotaList a]|]
     -- , IotaHList
     ]
 )

$( mkFragExprInstance
     "NegationPurification"
     [[t|'[IotaNumber] -> '[IotaNumber]|]]
     -- ['num'] -> ['num']
 )

$( mkIotaFragExpr
     "UniquenessPurification"
     [pattern| NORTH_EAST aweaqa |]
     [[t|forall a. Fragment '[IotaList a] '[IotaList a]|]]
 )

-- Meta-Evaluation

iotaHermesGambit :: IotaPattern
iotaHermesGambit = IotaPattern [pattern| SOUTH_EAST deaqq |]

fragHermesGambit :: Fragment (IotaExec as as' ': as) as'
fragHermesGambit = fragSingleton iotaHermesGambit

iotaIrisGambit :: IotaPattern
iotaIrisGambit = IotaPattern [pattern| NORTH_WEST qwaqde |]

fragIrisGambit :: Fragment (IotaExec (IotaExec as' bs ': as) as' ': as) as'
fragIrisGambit = fragSingleton iotaIrisGambit

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

iotaCharonsGambit :: IotaPattern
iotaCharonsGambit = IotaPattern [pattern| SOUTH_WEST aqdee |]

fragCharonsGambit :: Fragment as bs
fragCharonsGambit = fragSingleton iotaCharonsGambit

$( mkIotaFragExpr
     "ThanatosReflection"
     [pattern| SOUTH_EAST qqaed |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

-- Spell Circle Patterns

$( mkIotaFragExpr
     "WaystoneReflection"
     [pattern| SOUTH_WEST eaqwqae |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

$( mkIotaFragExpr
     "LodestoneReflection"
     [pattern| SOUTH_WEST eaqwqaewede |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

$( mkIotaFragExpr
     "LesserFoldReflection"
     [pattern| SOUTH_WEST eaqwqaewdd |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

$( mkIotaFragExpr
     "GreaterFoldReflection"
     [pattern| WEST aqwqawaaqa |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

-- Akashic Patterns

$( mkIotaFragExpr
     "AkashasDistillation"
     [pattern| WEST qqqwqqqqqaq |]
     [[t|'[IotaPattern, IotaVector] -> '[IotaAny]|]]
     -- ['pattern', 'vector'] -> ['any']
 )

$( mkIotaFragExpr
     "AkashasGambit"
     [pattern| EAST eeeweeeeede |]
     [[t|forall a. '[a, IotaPattern, IotaVector] -> '[]|]]
     -- ['any', 'pattern', 'vector'] -> ['']
 )

--- Spells

-- Basic Spells

$( mkIotaFragExpr
     "Explosion"
     [pattern| EAST aawaawaa |]
     [[t|'[IotaNumber, IotaVector] -> '[]|]]
     -- ['number', 'vector'] -> ['']
 )

$( mkIotaFragExpr
     "Fireball"
     [pattern| EAST ddwddwdd |]
     [[t|'[IotaNumber, IotaVector] -> '[]|]]
     -- ['number', 'vector'] -> ['']
 )

$( mkIotaFragExpr
     "Impulse"
     [pattern| SOUTH_WEST awqqqwaqw |]
     [[t|'[IotaVector, IotaEntity] -> '[]|]]
     -- ['vector', 'entity'] -> ['']
 )

$( mkIotaFragExpr
     "Blink"
     [pattern| SOUTH_WEST awqqqwaq |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'entity'] -> ['']
 )

$( mkIotaFragExpr
     "MakeNote"
     [pattern| WEST adaa |]
     [[t|'[IotaNumber, IotaNumber, IotaVector] -> '[]|]]
     -- ['number', 'number', 'vector'] -> ['']
 )

-- Block Manipulation

$( mkIotaFragExpr
     "PlaceBlock"
     [pattern| SOUTH_WEST eeeeede |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

$( mkIotaFragExpr
     "BreakBlock"
     [pattern| EAST qaqqqqq |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

$( mkIotaFragExpr
     "CreateWater"
     [pattern| SOUTH_EAST aqawqadaq |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

$( mkIotaFragExpr
     "DestroyLiquid"
     [pattern| SOUTH_WEST dedwedade |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

$( mkIotaFragExpr
     "ConjureBlock"
     [pattern| NORTH_EAST qqa |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

$( mkIotaFragExpr
     "ConjureLight"
     [pattern| NORTH_EAST qqd |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

$( mkIotaFragExpr
     "Overgrow"
     [pattern| NORTH_EAST wqaqwawqaqw |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

$( mkIotaFragExpr
     "EdifySapling"
     [pattern| NORTH_EAST wqaqwd |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

$( mkIotaFragExpr
     "Ignite"
     [pattern| SOUTH_EAST aaqawawa |]
     [ [t|'[IotaEntity] -> '[]|]
     , [t|'[IotaVector] -> '[]|]
     ]
     -- ['entity | vector'] -> ['']
 )

$( mkIotaFragExpr
     "ExtinguishArea"
     [pattern| SOUTH_WEST ddedwdwd |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Nadirs

$( mkIotaFragExpr
     "WhiteSunsNadir"
     [pattern| NORTH_WEST qqqqqaqwawaw |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

$( mkIotaFragExpr
     "BlueSunsNadir"
     [pattern| WEST qqqqqawwawawd |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'entity'] -> ['']
 )

$( mkIotaFragExpr
     "BlackSunsNadir"
     [pattern| SOUTH_WEST qqqqqaewawawe |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

$( mkIotaFragExpr
     "RedSunsNadir"
     [pattern| SOUTH_EAST qqqqqadwawaww |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

$( mkIotaFragExpr
     "GreenSunsNadir"
     [pattern| SOUTH_EAST qqqqqadwawaw |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

-- Crafting Casting Items

$( mkIotaFragExpr
     "CraftCypher"
     [pattern| EAST waqqqqq |]
     [[t|forall s. '[IotaExec '[] s, IotaEntity] -> '[]|]]
     -- ['[pattern]', 'entity'] -> ['']
 )

$( mkIotaFragExpr
     "CraftTrinket"
     [pattern| EAST wwaqqqqqeaqeaeqqqeaeq |]
     [[t|forall s. '[IotaExec '[] s, IotaEntity] -> '[]|]]
     -- ['[pattern]', 'entity'] -> ['']
 )

$( mkIotaFragExpr
     "CraftArtifact"
     [pattern| EAST wwaqqqqqeawqwqwqwqwqwwqqeadaeqqeqqeadaeqq |]
     [[t|forall s. '[IotaExec '[] s, IotaEntity] -> '[]|]]
     -- ['[pattern]', 'entity'] -> ['']
 )

$( mkIotaFragExpr
     "RechargeItem"
     [pattern| NORTH_WEST qqqqqwaeaeaeaeaea |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['entity'] -> ['']
 )

$( mkIotaFragExpr
     "EraseItem"
     [pattern| EAST qdqawwaww |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Sentinels

$( mkIotaFragExpr
     "SummonSentinel"
     [pattern| EAST waeawae |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

$( mkIotaFragExpr
     "BanishSentinel"
     [pattern| NORTH_EAST qdwdqdw |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

$( mkIotaFragExpr
     "LocateSentinel"
     [pattern| EAST waeawaede |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

$( mkIotaFragExpr
     "WayfindSentinel"
     [pattern| EAST waeawaedwa |]
     [[t|'[IotaVector] -> '[IotaVector]|]]
     -- ['vector'] -> ['vector']
 )

-- Internalize Pigment

$( mkIotaFragExpr
     "InternalizePigment"
     [pattern| EAST awddwqawqwawq |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Caster's Glamour

$( mkIotaFragExpr
     "CastersGlamour"
     [pattern| WEST dwaawedwewdwe |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Flight

$( mkIotaFragExpr
     "AnchoritesFlight"
     [pattern| SOUTH_WEST awawaawq |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'entity'] -> ['']
 )

$( mkIotaFragExpr
     "WayfarersFlight"
     [pattern| NORTH_EAST dwdwdewq |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'entity'] -> ['']
 )

$( mkIotaFragExpr
     "AviatorsPurification"
     [pattern| NORTH_EAST dwdwdeweaqa |]
     [[t|'[IotaEntity] -> '[IotaBoolean]|]]
     -- ['entity'] -> ['boolean']
 )

--- Great Spells

-- Create Lava

$( mkGreatIotaFragExpr
     "CreateLava"
     "Create Lava"
     [pattern| EAST eaqawqadaqd |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Weather Manipulation

$( mkGreatIotaFragExpr
     "SummonLightning"
     "Summon Lightning"
     [pattern| EAST waadwawdaaweewq |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

$( mkGreatIotaFragExpr
     "SummonRain"
     "Summon Rain"
     [pattern| WEST wwweeewwweewdawdwad |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

$( mkGreatIotaFragExpr
     "DispelRain"
     "Dispel Rain"
     [pattern| EAST eeewwweeewwaqqddqdqd |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Altiora

$( mkGreatIotaFragExpr
     "Altiora"
     "Altiora"
     [pattern| NORTH_WEST eawwaeawawaa |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['player'] -> ['']
 )

-- Greater Teleport

$( mkGreatIotaFragExpr
     "GreaterTeleport"
     "Greater Teleport"
     [pattern| EAST wwwqqqwwwqqeqqwwwqqwqqdqqqqqdqq |]
     [[t|'[IotaVector, IotaEntity] -> '[]|]]
     -- ['vector', 'entity'] -> ['']
 )

-- Zeniths

$( mkGreatIotaFragExpr
     "WhiteSunsZenith"
     "White Sun's Zenith"
     [pattern| NORTH_WEST qqqqaawawaedd |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

$( mkGreatIotaFragExpr
     "BlueSunsZenith"
     "Blue Sun's Zenith"
     [pattern| WEST qqqaawawaeqdd |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'entity'] -> ['']
 )

$( mkGreatIotaFragExpr
     "BlackSunsZenith"
     "Black Sun's Zenith"
     [pattern| SOUTH_WEST qqaawawaeqqdd |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

$( mkGreatIotaFragExpr
     "RedSunsZenith"
     "Red Sun's Zenith"
     [pattern| SOUTH_EAST qaawawaeqqqdd |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

$( mkGreatIotaFragExpr
     "GreenSunsZenith"
     "Green Sun's Zenith"
     [pattern| EAST aawawaeqqqqdd |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

-- Summon Greater Sentinel

$( mkGreatIotaFragExpr
     "SummonGreaterSentinel"
     "Summon Greater Sentinel"
     [pattern| EAST waeawaeqqqwqwqqwq |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Craft Phial

$( mkGreatIotaFragExpr
     "CraftPhial"
     "Craft Phial"
     [pattern| SOUTH_WEST aqqqaqwwaqqqqqeqaqqqawwqwqwqwqwqw |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['entity'] -> ['']
 )

-- Flay Mind

$( mkGreatIotaFragExpr
     "FlayMind"
     "Flay Mind"
     [pattern| NORTH_EAST qeqwqwqwqwqeqaeqeaqeqaeqaqded |]
     [[t|'[IotaVector, IotaEntity] -> '[]|]]
     -- ['vector', 'entity'] -> ['']
 )

-- special

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
