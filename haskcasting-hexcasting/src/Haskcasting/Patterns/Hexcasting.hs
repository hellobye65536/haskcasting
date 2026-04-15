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
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Haskcasting.ExprLang.Core (Expr)
import Haskcasting.ExprLang.Core qualified as E
import Haskcasting.ExprLang.TH (mkGreatIotaFragExpr, mkIotaFragExpr)
import Haskcasting.Fragment (Fragment, fragSingleton)
import Haskcasting.Iota (
  IotaAny,
  IotaAnyList,
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

$( mkIotaFragExpr
     "MindsReflection"
     [pattern| NORTH_EAST qaq |]
     [[t|Fragment '[] '[IotaEntity]|]]
 )

$( mkIotaFragExpr
     "CompassPurification"
     [pattern| EAST aa |]
     [[t|Fragment '[IotaEntity] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "CompassPurificationII"
     [pattern| NORTH_EAST dd |]
     [[t|Fragment '[IotaEntity] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "AlidadesPurification"
     [pattern| EAST wa |]
     [[t|Fragment '[IotaEntity] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "StadiometersPurification"
     [pattern| NORTH_EAST awq |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "PacePurification"
     [pattern| EAST wq |]
     [[t|Fragment '[IotaEntity] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "ArchersDistillation"
     [pattern| EAST wqaawdd |]
     [[t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "ArchitectsDistillation"
     [pattern| EAST weddwaa |]
     [[t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "ScoutsDistillation"
     [pattern| EAST weaqa |]
     [[t|Fragment '[IotaVector, IotaVector] '[IotaEntity]|]]
 )

$( mkIotaFragExpr
     "WaystoneReflection"
     [pattern| SOUTH_WEST eaqwqae |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "LodestoneReflection"
     [pattern| SOUTH_WEST eaqwqaewede |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "LesserFoldReflection"
     [pattern| SOUTH_WEST eaqwqaewdd |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "GreaterFoldReflection"
     [pattern| WEST aqwqawaaqa |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

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

$( mkIotaFragExpr
     "DioscuriGambit"
     [pattern| EAST aadadaaw |]
     [[t|forall a b. Fragment '[a, b] '[a, b, a, b]|]]
 )

$( mkIotaFragExpr
     "FlocksReflection"
     [pattern| NORTH_WEST qwaeawqaeaqa |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

iotaGeminiGambit, iotaFishermansGambit, iotaFishermansGambitII, iotaSwindlersGambit :: IotaPattern
iotaGeminiGambit = IotaPattern [pattern| EAST aadaadaa |]
iotaFishermansGambit = IotaPattern [pattern| WEST ddad |]
iotaFishermansGambitII = IotaPattern [pattern| EAST aada |]
iotaSwindlersGambit = IotaPattern [pattern| SOUTH_EAST qaawdde |]

$( mkIotaFragExpr
     "AdditiveDistillation"
     [pattern| NORTH_EAST waaw |]
     [ [t|forall as bs asbs. HAppendListR as bs ~ asbs => Fragment '[IotaHList bs, IotaHList as] '[IotaHList asbs]|]
     , [t|forall a. Fragment '[IotaList a, IotaList a] '[IotaList a]|]
     , [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
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
     [ [t|Fragment '[IotaBoolean] '[IotaNumber]|]
     , [t|forall a. Fragment '[IotaList a] '[IotaNumber]|]
     , [t|forall as. Fragment '[IotaHList as] '[IotaNumber]|]
     , [t|Fragment '[IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector] '[IotaNumber]|]
     ]
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
     "AxialPurification"
     [pattern| NORTH_WEST qqqqqaww |]
     [ [t|Fragment '[IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector] '[IotaVector]|]
     ]
 )

$( mkIotaFragExpr
     "ConjunctionDistillation"
     [pattern| NORTH_EAST wdw |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|forall a. Fragment '[IotaList a, IotaList a] '[IotaList a]|]
     , -- IotaHList
       [t|Fragment '[IotaBoolean, IotaBoolean] '[IotaBoolean]|]
     ]
 )

$( mkIotaFragExpr
     "DisjunctionDistillation"
     [pattern| SOUTH_EAST waw |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|forall a. Fragment '[IotaList a, IotaList a] '[IotaList a]|]
     , -- IotaHList
       [t|Fragment '[IotaBoolean, IotaBoolean] '[IotaBoolean]|]
     ]
 )

$( mkIotaFragExpr
     "NegationPurification"
     [pattern| NORTH_WEST dw |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaBoolean, IotaBoolean] '[IotaBoolean]|]
     ]
 )

$( mkIotaFragExpr
     "ExclusionDistillation"
     [pattern| NORTH_WEST dwa |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|forall a. Fragment '[IotaList a, IotaList a] '[IotaList a]|]
     , -- IotaHList
       [t|Fragment '[IotaBoolean, IotaBoolean] '[IotaBoolean]|]
     ]
 )

$( mkIotaFragExpr
     "MaximusDistillation"
     [pattern| SOUTH_EAST e |]
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaBoolean]|]]
 )

$( mkIotaFragExpr
     "MinimusDistillation"
     [pattern| SOUTH_WEST q |]
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaBoolean]|]]
 )

$( mkIotaFragExpr
     "MaximusDistillationII"
     [pattern| SOUTH_EAST ee |]
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaBoolean]|]]
 )

$( mkIotaFragExpr
     "MinimusDistillationII"
     [pattern| SOUTH_WEST qq |]
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaBoolean]|]]
 )

$( mkIotaFragExpr
     "EqualityDistillation"
     [pattern| EAST ad |]
     [[t|Fragment '[IotaAny, IotaAny] '[IotaBoolean]|]]
 )

$( mkIotaFragExpr
     "InequalityDistillation"
     [pattern| EAST da |]
     [[t|Fragment '[IotaAny, IotaAny] '[IotaBoolean]|]]
 )

$( mkIotaFragExpr
     "AugursPurification"
     [pattern| NORTH_EAST aw |]
     [[t|forall a. Fragment '[a] '[IotaBoolean]|]]
 )

iotaAugursExaltation :: IotaPattern
iotaAugursExaltation = IotaPattern [pattern| SOUTH_EAST awdd |]

fragAugursExaltation :: forall a s. Fragment (a ': a ': IotaBoolean ': s) (a ': s)
fragAugursExaltation = fragSingleton iotaAugursExaltation

exprAugursExaltation :: Expr blk '[a, a, IotaBoolean] -> Expr blk '[a]
exprAugursExaltation = E.call fragAugursExaltation

$( mkIotaFragExpr
     "EntropyReflection"
     [pattern| NORTH_WEST eqqq |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "SinePurification"
     [pattern| SOUTH_EAST qqqqqaa |]
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "CosinePurification"
     [pattern| SOUTH_EAST qqqqqad |]
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "TangentPurification"
     [pattern| SOUTH_WEST wqqqqqadq |]
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "InverseSinePurification"
     [pattern| SOUTH_EAST ddeeeee |]
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "InverseCosinePurification"
     [pattern| NORTH_EAST adeeeee |]
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "InverseTangentPurification"
     [pattern| NORTH_EAST eadeeeeew |]
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "InverseTangentDistillation"
     [pattern| WEST deadeeeeewd |]
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "LogarithmicDistillation"
     [pattern| NORTH_WEST eqaqe |]
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]]
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
     "UniquenessPurification"
     [pattern| NORTH_EAST aweaqa |]
     [[t|forall a. Fragment '[IotaList a] '[IotaList a]|]]
 )

$( mkIotaFragExpr
     "Reveal"
     [pattern| NORTH_EAST de |]
     [[t|forall a. Fragment '[a] '[a]|]]
 )

$( mkIotaFragExpr
     "Explosion"
     [pattern| EAST aawaawaa |]
     [[t|Fragment '[IotaNumber, IotaVector] '[]|]]
 )

$( mkIotaFragExpr
     "Fireball"
     [pattern| EAST ddwddwdd |]
     [[t|Fragment '[IotaVector, IotaNumber] '[]|]]
 )

$( mkIotaFragExpr
     "Impulse"
     [pattern| SOUTH_WEST awqqqwaqw |]
     [[t|Fragment '[IotaVector, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "Blink"
     [pattern| SOUTH_WEST awqqqwaq |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "BreakBlock"
     [pattern| EAST qaqqqqq |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFragExpr
     "PlaceBlock"
     [pattern| SOUTH_WEST eeeeede |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFragExpr
     "InternalizePigment"
     [pattern| EAST awddwqawqwawq |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFragExpr
     "CastersGlamour"
     [pattern| WEST dwaawedwewdwe |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFragExpr
     "CreateWater"
     [pattern| SOUTH_EAST aqawqadaq |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFragExpr
     "DestroyLiquid"
     [pattern| SOUTH_WEST dedwedade |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFragExpr
     "Ignite"
     [pattern| SOUTH_EAST aaqawawa |]
     [ [t|Fragment '[IotaEntity] '[]|]
     , [t|Fragment '[IotaVector] '[]|]
     ]
 )

$( mkIotaFragExpr
     "ExtinguishArea"
     [pattern| SOUTH_WEST ddedwdwd |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFragExpr
     "ConjureBlock"
     [pattern| NORTH_EAST qqa |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFragExpr
     "ConjureLight"
     [pattern| NORTH_EAST qqd |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFragExpr
     "Overgrow"
     [pattern| NORTH_EAST wqaqwawqaqw |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFragExpr
     "RechargeItem"
     [pattern| NORTH_WEST qqqqqwaeaeaeaeaea |]
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "EraseItem"
     [pattern| EAST qdqawwaww |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFragExpr
     "EdifySapling"
     [pattern| NORTH_EAST wqaqwd |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFragExpr
     "MakeNote"
     [pattern| WEST adaa |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaVector] '[]|]]
 )

$( mkIotaFragExpr
     "CraftCypher"
     [pattern| EAST waqqqqq |]
     [[t|Fragment '[IotaAnyList, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "CraftTrinket"
     [pattern| EAST wwaqqqqqeaqeaeqqqeaeq |]
     [[t|Fragment '[IotaAnyList, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "CraftArtifact"
     [pattern| EAST wwaqqqqqeawqwqwqwqwqwwqqeadaeqqeqqeadaeqq |]
     [[t|Fragment '[IotaAnyList, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "CraftPhial"
     [pattern| SOUTH_WEST aqqqaqwwaqqqqqeqaqqqawwqwqwqwqwqw |]
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "WhiteSunsNadir"
     [pattern| NORTH_WEST qqqqqaqwawaw |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "BlueSunsNadir"
     [pattern| WEST qqqqqawwawawd |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "BlackSunsNadir"
     [pattern| SOUTH_WEST qqqqqaewawawe |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "RedSunsNadir"
     [pattern| SOUTH_EAST qqqqqadwawaww |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "GreenSunsNadir"
     [pattern| SOUTH_EAST qqqqqadwawaw |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "WhiteSunsZenith"
     [pattern| NORTH_WEST qqqqaawawaedd |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "BlueSunsZenith"
     [pattern| WEST qqqaawawaeqdd |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "BlackSunsZenith"
     [pattern| SOUTH_WEST qqaawawaeqqdd |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "RedSunsZenith"
     [pattern| SOUTH_EAST qaawawaeqqqdd |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "GreenSunsZenith"
     [pattern| EAST aawawaeqqqqdd |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "AnchoritesFlight"
     [pattern| SOUTH_WEST awawaawq |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "WayfarersFlight"
     [pattern| NORTH_EAST dwdwdewq |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "AviatorsPurification"
     [pattern| NORTH_EAST dwdwdeweaqa |]
     [[t|Fragment '[IotaEntity] '[IotaBoolean]|]]
 )

$( mkIotaFragExpr
     "SummonSentinel"
     [pattern| EAST waeawae |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFragExpr
     "BanishSentinel"
     [pattern| NORTH_EAST qdwdqdw |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFragExpr
     "LocateSentinel"
     [pattern| EAST waeawaede |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "WayfindSentinel"
     [pattern| EAST waeawaedwa |]
     [[t|Fragment '[IotaVector] '[IotaVector]|]]
 )

$( mkGreatIotaFragExpr
     "Altiora"
     "Altiora"
     [pattern| NORTH_WEST eawwaeawawaa |]
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkGreatIotaFragExpr
     "CreateLava"
     "Create Lava"
     [pattern| EAST eaqawqadaqd |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkGreatIotaFragExpr
     "GreaterTeleport"
     "Greater Teleport"
     [pattern| EAST wwwqqqwwwqqeqqwwwqqwqqdqqqqqdqq |]
     [[t|Fragment '[IotaVector, IotaEntity] '[]|]]
 )

$( mkGreatIotaFragExpr
     "SummonGreaterSentinel"
     "Summon Greater Sentinel"
     [pattern| EAST waeawaeqqqwqwqqwq |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkGreatIotaFragExpr
     "DispelRain"
     "Dispel Rain"
     [pattern| EAST eeewwweeewwaqqddqdqd |]
     [[t|Fragment '[] '[]|]]
 )

$( mkGreatIotaFragExpr
     "SummonRain"
     "Summon Rain"
     [pattern| WEST wwweeewwweewdawdwad |]
     [[t|Fragment '[] '[]|]]
 )

$( mkGreatIotaFragExpr
     "FlayMind"
     "Flay Mind"
     [pattern| NORTH_EAST qeqwqwqwqwqeqaeqeaqeqaeqaqded |]
     [[t|Fragment '[IotaVector, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "AkashasDistillation"
     [pattern| WEST qqqwqqqqqaq |]
     [[t|Fragment '[IotaPattern, IotaVector] '[IotaAny]|]]
 )

$( mkIotaFragExpr
     "AkashasGambit"
     [pattern| EAST eeeweeeeede |]
     [[t|Fragment '[IotaAny, IotaPattern, IotaVector] '[]|]]
 )

iotaHermesGambit :: IotaPattern
iotaHermesGambit = IotaPattern [pattern| SOUTH_EAST deaqq |]

fragHermesGambit :: Fragment (IotaExec as as' ': as) as'
fragHermesGambit = fragSingleton iotaHermesGambit

iotaIrisGambit :: IotaPattern
iotaIrisGambit = IotaPattern [pattern| NORTH_WEST qwaqde |]

fragIrisGambit :: Fragment (IotaExec (IotaExec as' bs ': as) as' ': as) as'
fragIrisGambit = fragSingleton iotaIrisGambit

iotaCharonsGambit :: IotaPattern
iotaCharonsGambit = IotaPattern [pattern| SOUTH_WEST aqdee |]

fragCharonsGambit :: Fragment as bs
fragCharonsGambit = fragSingleton iotaCharonsGambit

$( mkIotaFragExpr
     "ScribesReflection"
     [pattern| EAST aqqqqq |]
     [[t|Fragment '[] '[IotaAny]|]]
 )

$( mkIotaFragExpr
     "ChroniclersPurification"
     [pattern| EAST wawqwqwqwqwqw |]
     [ [t|Fragment '[IotaVector] '[IotaAny]|]
     , [t|Fragment '[IotaEntity] '[IotaAny]|]
     ]
 )

$( mkIotaFragExpr
     "ScribesGambit"
     [pattern| EAST deeeee |]
     [[t|Fragment '[IotaAny] '[]|]]
 )

$( mkIotaFragExpr
     "ChroniclersGambit"
     [pattern| EAST wdwewewewewew |]
     [ [t|Fragment '[IotaAny, IotaVector] '[]|]
     , [t|Fragment '[IotaAny, IotaEntity] '[]|]
     ]
 )

$( mkIotaFragExpr
     "AuditorsReflection"
     [pattern| EAST aqqqqqe |]
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkIotaFragExpr
     "AuditorsPurification"
     [pattern| EAST wawqwqwqwqwqwew |]
     [ [t|Fragment '[IotaVector] '[IotaBoolean]|]
     , [t|Fragment '[IotaEntity] '[IotaBoolean]|]
     ]
 )

$( mkIotaFragExpr
     "AssessorsReflection"
     [pattern| EAST deeeeeq |]
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkIotaFragExpr
     "AssessorsPurification"
     [pattern| EAST wdwewewewewewqw |]
     [ [t|Fragment '[IotaVector] '[IotaBoolean]|]
     , [t|Fragment '[IotaEntity] '[IotaBoolean]|]
     ]
 )

$( mkIotaFragExpr
     "MuninnsReflection"
     [pattern| NORTH_EAST qeewdweddw |]
     [[t|Fragment '[] '[IotaAny]|]]
 )

$( mkIotaFragExpr
     "HuginnsGambit"
     [pattern| NORTH_WEST eqqwawqaaw |]
     [[t|forall a. Fragment '[a] '[]|]]
 )

$( mkIotaFragExpr
     "ThanatosReflection"
     [pattern| SOUTH_EAST qqaed |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "NullaryReflection"
     [pattern| EAST d |]
     [[t|Fragment '[] '[IotaNull]|]]
 )

$( mkIotaFragExpr
     "TrueReflection"
     [pattern| SOUTH_EAST aqae |]
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkIotaFragExpr
     "FalseReflection"
     [pattern| NORTH_EAST dedq |]
     [[t|Fragment '[] '[IotaBoolean]|]]
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
     "VectorReflectionZero"
     [pattern| NORTH_WEST qqqqq |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "ArcsReflection"
     [pattern| NORTH_EAST qdwdq |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "CirclesReflection"
     [pattern| NORTH_WEST eawae |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "EulersReflection"
     [pattern| EAST aaq |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

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
     "ZoneDistillationAny"
     [pattern| SOUTH_EAST qqqqqwded |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
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

$( mkIotaFragExpr
     "SelectionDistillation"
     [pattern| NORTH_WEST deeed |]
     [[t|forall a. Fragment '[IotaNumber, IotaList a] '[a]|]]
 )

iotaThothsGambit :: IotaPattern
iotaThothsGambit = IotaPattern [pattern| NORTH_EAST dadad |]

type family FragThothsGambitHList a a' as where
  FragThothsGambitHList a a' '[] = '[]
  FragThothsGambitHList a a' (a ': as) = HAppendListR a' (FragThothsGambitHList a a' as)

type family FragThothsGambit (s :: [Type]) where
  FragThothsGambit (IotaList a ': IotaExec (a ': s) '[] ': s) = (IotaHList '[] ': s)
  FragThothsGambit (IotaList a ': IotaExec (a ': s) '[a'] ': s) = (IotaList a' ': s)
  FragThothsGambit (IotaHList as ': IotaExec (a ': s) a' ': s) = (IotaHList (FragThothsGambitHList a a' as) ': s)

fragThothsGambit :: Fragment s (FragThothsGambit s)
fragThothsGambit = fragSingleton iotaThothsGambit

$( mkIotaFragExpr
     "SinglesPurification"
     [pattern| EAST adeeed |]
     [[t|forall a. Fragment '[a] '[IotaHList '[a]]|]]
 )

$( mkIotaFragExpr
     "VacantReflection"
     [pattern| NORTH_EAST qqaeaae |]
     [[t|Fragment '[] '[IotaHList '[]]|]]
 )

$( mkIotaFragExpr
     "RetrogradePurification"
     [pattern| EAST qqqaede |]
     [ [t|forall a. Fragment '[IotaList a] '[IotaList a]|]
     , [t|forall as as'. HReverse as as' => Fragment '[IotaHList as] '[IotaHList as']|]
     ]
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
     "SelectionExaltation"
     [pattern| NORTH_WEST qaeaqwded |]
     [[t|forall a. Fragment '[IotaNumber, IotaNumber, IotaList a] '[IotaList a]|]]
 )

$( mkIotaFragExpr
     "SurgeonsExaltation"
     [pattern| NORTH_WEST wqaeaqw |]
     [[t|forall a. Fragment '[a, IotaNumber, IotaList a] '[IotaList a]|]]
 )

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

$( mkIotaFragExpr
     "GulliversPurification"
     [pattern| NORTH_WEST aawawwawwa |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "AlterScale"
     [pattern| NORTH_EAST ddwdwwdwwd |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
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
