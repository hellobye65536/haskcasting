{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskcasting.Patterns.Hexcasting where

import Data.ByteString.Char8 qualified as BC
import Data.FileEmbed (embedFileRelative)
import Data.HList (HAppendFD, HAppendListR, HReverse, Proxy (Proxy))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import GHC.Natural (naturalToInteger)
import GHC.TypeNats (KnownNat, natVal)
import Haskcasting.Fragment (Fragment (Fragment))
import Haskcasting.Iota (
  Angle,
  Direction (..),
  IotaAny,
  IotaAnyList,
  IotaBoolean,
  IotaCast (iotaCast),
  IotaEntity,
  IotaGreatPattern (IotaGreatPattern),
  IotaHList,
  IotaList,
  IotaNull,
  IotaNumber,
  IotaPattern (IotaPattern),
  IotaVector,
  angleParse,
 )
import Haskcasting.TH (angles, mkGreatIotaFrag, mkIotaFrag, pattern)

$( mkIotaFrag
     "MindsReflection"
     [pattern| NORTH_EAST qaq |]
     [[t|Fragment '[] '[IotaEntity]|]]
 )

$( mkIotaFrag
     "CompassPurification"
     [pattern| EAST aa |]
     [[t|Fragment '[IotaEntity] '[IotaVector]|]]
 )

$( mkIotaFrag
     "CompassPurificationII"
     [pattern| NORTH_EAST dd |]
     [[t|Fragment '[IotaEntity] '[IotaVector]|]]
 )

$( mkIotaFrag
     "AlidadesPurification"
     [pattern| EAST wa |]
     [[t|Fragment '[IotaEntity] '[IotaVector]|]]
 )

$( mkIotaFrag
     "StadiometersPurification"
     [pattern| NORTH_EAST awq |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "PacePurification"
     [pattern| EAST wq |]
     [[t|Fragment '[IotaEntity] '[IotaVector]|]]
 )

$( mkIotaFrag
     "ArchersDistillation"
     [pattern| EAST wqaawdd |]
     [[t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]]
 )

$( mkIotaFrag
     "ArchitectsDistillation"
     [pattern| EAST weddwaa |]
     [[t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]]
 )

$( mkIotaFrag
     "ScoutsDistillation"
     [pattern| EAST weaqa |]
     [[t|Fragment '[IotaVector, IotaVector] '[IotaEntity]|]]
 )

$( mkIotaFrag
     "WaystoneReflection"
     [pattern| SOUTH_WEST eaqwqae |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFrag
     "LodestoneReflection"
     [pattern| SOUTH_WEST eaqwqaewede |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFrag
     "LesserFoldReflection"
     [pattern| SOUTH_WEST eaqwqaewdd |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFrag
     "GreaterFoldReflection"
     [pattern| WEST aqwqawaaqa |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFrag
     "JestersGambit"
     [pattern| EAST aawdd |]
     [[t|forall a b. Fragment '[a, b] '[b, a]|]]
 )

$( mkIotaFrag
     "RotationGambit"
     [pattern| EAST aaeaa |]
     [[t|forall a b c. Fragment '[a, b, c] '[c, a, b]|]]
 )

$( mkIotaFrag
     "RotationGambitII"
     [pattern| NORTH_EAST ddqdd |]
     [[t|forall a b c. Fragment '[a, b, c] '[b, c, a]|]]
 )

$( mkIotaFrag
     "GeminiDecomposition"
     [pattern| EAST aadaa |]
     [[t|forall a. Fragment '[a] '[a, a]|]]
 )

$( mkIotaFrag
     "ProspectorsGambit"
     [pattern| EAST aaedd |]
     [[t|forall a b. Fragment '[a, b] '[b, a, b]|]]
 )

$( mkIotaFrag
     "UndertakersGambit"
     [pattern| EAST ddqaa |]
     [[t|forall a b. Fragment '[a, b] '[a, b, a]|]]
 )

$( mkIotaFrag
     "DioscuriGambit"
     [pattern| EAST aadadaaw |]
     [[t|forall a b. Fragment '[a, b] '[a, b, a, b]|]]
 )

$( mkIotaFrag
     "FlocksReflection"
     [pattern| NORTH_WEST qwaeawqaeaqa |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

iotaGeminiGambit, iotaFishermansGambit, iotaFishermansGambitII, iotaSwindlersGambit :: IotaPattern
iotaGeminiGambit = [pattern| EAST aadaadaa |]
iotaFishermansGambit = [pattern| WEST ddad |]
iotaFishermansGambitII = [pattern| EAST aada |]
iotaSwindlersGambit = [pattern| SOUTH_EAST qaawdde |]

$( mkIotaFrag
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

$( mkIotaFrag
     "SubtractiveDistillation"
     [pattern| NORTH_WEST wddw |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
 )

$( mkIotaFrag
     "MultiplicativeDistillation"
     [pattern| SOUTH_EAST waqaw |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaNumber]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
 )

$( mkIotaFrag
     "DivisionDistillation"
     [pattern| NORTH_EAST wdedw |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
 )

$( mkIotaFrag
     "LengthPurification"
     [pattern| NORTH_EAST wqaqw |]
     [ [t|Fragment '[IotaBoolean] '[IotaNumber]|]
     , [t|forall a. Fragment '[IotaList a] '[IotaNumber]|]
     , [t|forall as. Fragment '[IotaHList as] '[IotaNumber]|]
     , [t|Fragment '[IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector] '[IotaNumber]|]
     ]
 )

$( mkIotaFrag
     "PowerDistillation"
     [pattern| NORTH_WEST wedew |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
 )

$( mkIotaFrag
     "FloorPurification"
     [pattern| EAST ewq |]
     [ [t|Fragment '[IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector] '[IotaVector]|]
     ]
 )

$( mkIotaFrag
     "CeilingPurification"
     [pattern| EAST qwe |]
     [ [t|Fragment '[IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector] '[IotaVector]|]
     ]
 )

$( mkIotaFrag
     "VectorExaltation"
     [pattern| EAST eqqqqq |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaNumber] '[IotaVector]|]]
 )

$( mkIotaFrag
     "VectorDisintegration"
     [pattern| EAST qeeeee |]
     [[t|Fragment '[IotaVector] '[IotaNumber, IotaNumber, IotaNumber]|]]
 )

$( mkIotaFrag
     "AxialPurification"
     [pattern| NORTH_WEST qqqqqaww |]
     [ [t|Fragment '[IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector] '[IotaVector]|]
     ]
 )

$( mkIotaFrag
     "ConjunctionDistillation"
     [pattern| NORTH_EAST wdw |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|forall a. Fragment '[IotaList a, IotaList a] '[IotaList a]|]
     , -- IotaHList
       [t|Fragment '[IotaBoolean, IotaBoolean] '[IotaBoolean]|]
     ]
 )

$( mkIotaFrag
     "DisjunctionDistillation"
     [pattern| SOUTH_EAST waw |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|forall a. Fragment '[IotaList a, IotaList a] '[IotaList a]|]
     , -- IotaHList
       [t|Fragment '[IotaBoolean, IotaBoolean] '[IotaBoolean]|]
     ]
 )

$( mkIotaFrag
     "NegationPurification"
     [pattern| NORTH_WEST dw |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaBoolean, IotaBoolean] '[IotaBoolean]|]
     ]
 )

$( mkIotaFrag
     "ExclusionDistillation"
     [pattern| NORTH_WEST dwa |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|forall a. Fragment '[IotaList a, IotaList a] '[IotaList a]|]
     , -- IotaHList
       [t|Fragment '[IotaBoolean, IotaBoolean] '[IotaBoolean]|]
     ]
 )

$( mkIotaFrag
     "MaximusDistillation"
     [pattern| SOUTH_EAST e |]
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "MinimusDistillation"
     [pattern| SOUTH_WEST q |]
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "MaximusDistillationII"
     [pattern| SOUTH_EAST ee |]
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "MinimusDistillationII"
     [pattern| SOUTH_WEST qq |]
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "EqualityDistillation"
     [pattern| EAST ad |]
     [[t|Fragment '[IotaAny, IotaAny] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "InequalityDistillation"
     [pattern| EAST da |]
     [[t|Fragment '[IotaAny, IotaAny] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "AugursPurification"
     [pattern| NORTH_EAST aw |]
     [[t|Fragment '[IotaAny] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "AugursExaltation"
     [pattern| SOUTH_EAST awdd |]
     [[t|forall a. Fragment '[a, a, IotaBoolean] '[a]|]]
 )

$( mkIotaFrag
     "EntropyReflection"
     [pattern| NORTH_WEST eqqq |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "SinePurification"
     [pattern| SOUTH_EAST qqqqqaa |]
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "CosinePurification"
     [pattern| SOUTH_EAST qqqqqad |]
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "TangentPurification"
     [pattern| SOUTH_WEST wqqqqqadq |]
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "InverseSinePurification"
     [pattern| SOUTH_EAST ddeeeee |]
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "InverseCosinePurification"
     [pattern| NORTH_EAST adeeeee |]
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "InverseTangentPurification"
     [pattern| NORTH_EAST eadeeeeew |]
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "InverseTangentDistillation"
     [pattern| WEST deadeeeeewd |]
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "LogarithmicDistillation"
     [pattern| NORTH_WEST eqaqe |]
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "ModulusDistillation"
     [pattern| NORTH_EAST addwaad |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
 )

$( mkIotaFrag
     "UniquenessPurification"
     [pattern| NORTH_EAST aweaqa |]
     [[t|forall a. Fragment '[IotaList a] '[IotaList a]|]]
 )

$( mkIotaFrag
     "Reveal"
     [pattern| NORTH_EAST de |]
     [[t|forall a. Fragment '[a] '[a]|]]
 )

$( mkIotaFrag
     "Explosion"
     [pattern| EAST aawaawaa |]
     [[t|Fragment '[IotaNumber, IotaVector] '[]|]]
 )

$( mkIotaFrag
     "Fireball"
     [pattern| EAST ddwddwdd |]
     [[t|Fragment '[IotaVector, IotaNumber] '[]|]]
 )

$( mkIotaFrag
     "Impulse"
     [pattern| SOUTH_WEST awqqqwaqw |]
     [[t|Fragment '[IotaVector, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "Blink"
     [pattern| SOUTH_WEST awqqqwaq |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "BreakBlock"
     [pattern| EAST qaqqqqq |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "PlaceBlock"
     [pattern| SOUTH_WEST eeeeede |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "InternalizePigment"
     [pattern| EAST awddwqawqwawq |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "CastersGlamour"
     [pattern| WEST dwaawedwewdwe |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "CreateWater"
     [pattern| SOUTH_EAST aqawqadaq |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "DestroyLiquid"
     [pattern| SOUTH_WEST dedwedade |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "Ignite"
     [pattern| SOUTH_EAST aaqawawa |]
     [ [t|Fragment '[IotaEntity] '[]|]
     , [t|Fragment '[IotaVector] '[]|]
     ]
 )

$( mkIotaFrag
     "ExtinguishArea"
     [pattern| SOUTH_WEST ddedwdwd |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "ConjureBlock"
     [pattern| NORTH_EAST qqa |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "ConjureLight"
     [pattern| NORTH_EAST qqd |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "Overgrow"
     [pattern| NORTH_EAST wqaqwawqaqw |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "RechargeItem"
     [pattern| NORTH_WEST qqqqqwaeaeaeaeaea |]
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "EraseItem"
     [pattern| EAST qdqawwaww |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "EdifySapling"
     [pattern| NORTH_EAST wqaqwd |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "MakeNote"
     [pattern| WEST adaa |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaVector] '[]|]]
 )

$( mkIotaFrag
     "CraftCypher"
     [pattern| EAST waqqqqq |]
     [[t|Fragment '[IotaAnyList, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "CraftTrinket"
     [pattern| EAST wwaqqqqqeaqeaeqqqeaeq |]
     [[t|Fragment '[IotaAnyList, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "CraftArtifact"
     [pattern| EAST wwaqqqqqeawqwqwqwqwqwwqqeadaeqqeqqeadaeqq |]
     [[t|Fragment '[IotaAnyList, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "CraftPhial"
     [pattern| SOUTH_WEST aqqqaqwwaqqqqqeqaqqqawwqwqwqwqwqw |]
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "WhiteSunsNadir"
     [pattern| NORTH_WEST qqqqqaqwawaw |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "BlueSunsNadir"
     [pattern| WEST qqqqqawwawawd |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "BlackSunsNadir"
     [pattern| SOUTH_WEST qqqqqaewawawe |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "RedSunsNadir"
     [pattern| SOUTH_EAST qqqqqadwawaww |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "GreenSunsNadir"
     [pattern| SOUTH_EAST qqqqqadwawaw |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "WhiteSunsZenith"
     [pattern| NORTH_WEST qqqqaawawaedd |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "BlueSunsZenith"
     [pattern| WEST qqqaawawaeqdd |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "BlackSunsZenith"
     [pattern| SOUTH_WEST qqaawawaeqqdd |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "RedSunsZenith"
     [pattern| SOUTH_EAST qaawawaeqqqdd |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "GreenSunsZenith"
     [pattern| EAST aawawaeqqqqdd |]
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "AnchoritesFlight"
     [pattern| SOUTH_WEST awawaawq |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "WayfarersFlight"
     [pattern| NORTH_EAST dwdwdewq |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "AviatorsPurification"
     [pattern| NORTH_EAST dwdwdeweaqa |]
     [[t|Fragment '[IotaEntity] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "SummonSentinel"
     [pattern| EAST waeawae |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "BanishSentinel"
     [pattern| NORTH_EAST qdwdqdw |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "LocateSentinel"
     [pattern| EAST waeawaede |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFrag
     "WayfindSentinel"
     [pattern| EAST waeawaedwa |]
     [[t|Fragment '[IotaVector] '[IotaVector]|]]
 )

$( mkGreatIotaFrag
     "Altiora"
     (IotaGreatPattern "Altiora" [pattern| NORTH_WEST eawwaeawawaa |])
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkGreatIotaFrag
     "CreateLava"
     (IotaGreatPattern "Create Lava" [pattern| EAST eaqawqadaqd |])
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkGreatIotaFrag
     "GreaterTeleport"
     (IotaGreatPattern "Greater Teleport" [pattern| EAST wwwqqqwwwqqeqqwwwqqwqqdqqqqqdqq |])
     [[t|Fragment '[IotaVector, IotaEntity] '[]|]]
 )

$( mkGreatIotaFrag
     "SummonGreaterSentinel"
     (IotaGreatPattern "Summon Greater Sentinel" [pattern| EAST waeawaeqqqwqwqqwq |])
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkGreatIotaFrag
     "DispelRain"
     (IotaGreatPattern "Dispel Rain" [pattern| EAST eeewwweeewwaqqddqdqd |])
     [[t|Fragment '[] '[]|]]
 )

$( mkGreatIotaFrag
     "SummonRain"
     (IotaGreatPattern "Summon Rain" [pattern| WEST wwweeewwweewdawdwad |])
     [[t|Fragment '[] '[]|]]
 )

$( mkGreatIotaFrag
     "FlayMind"
     (IotaGreatPattern "Flay Mind" [pattern| NORTH_EAST qeqwqwqwqwqeqaeqeaqeqaeqaqded |])
     [[t|Fragment '[IotaVector, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "AkashasDistillation"
     [pattern| WEST qqqwqqqqqaq |]
     [[t|Fragment '[IotaPattern, IotaVector] '[IotaAny]|]]
 )

$( mkIotaFrag
     "AkashasGambit"
     [pattern| EAST eeeweeeeede |]
     [[t|Fragment '[IotaAny, IotaPattern, IotaVector] '[]|]]
 )

iotaHermesGambit, iotaIrisGambit, iotaCharonsGambit :: IotaPattern
iotaHermesGambit = [pattern| SOUTH_EAST deaqq |]
iotaIrisGambit = [pattern| NORTH_WEST qwaqde |]
iotaCharonsGambit = [pattern| SOUTH_WEST aqdee |]

$( mkIotaFrag
     "ScribesReflection"
     [pattern| EAST aqqqqq |]
     [[t|Fragment '[] '[IotaAny]|]]
 )

$( mkIotaFrag
     "ChroniclersPurification"
     [pattern| EAST wawqwqwqwqwqw |]
     [ [t|Fragment '[IotaVector] '[IotaAny]|]
     , [t|Fragment '[IotaEntity] '[IotaAny]|]
     ]
 )

$( mkIotaFrag
     "ScribesGambit"
     [pattern| EAST deeeee |]
     [[t|Fragment '[IotaAny] '[]|]]
 )

$( mkIotaFrag
     "ChroniclersGambit"
     [pattern| EAST wdwewewewewew |]
     [ [t|Fragment '[IotaAny, IotaVector] '[]|]
     , [t|Fragment '[IotaAny, IotaEntity] '[]|]
     ]
 )

$( mkIotaFrag
     "AuditorsReflection"
     [pattern| EAST aqqqqqe |]
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "AuditorsPurification"
     [pattern| EAST wawqwqwqwqwqwew |]
     [ [t|Fragment '[IotaVector] '[IotaBoolean]|]
     , [t|Fragment '[IotaEntity] '[IotaBoolean]|]
     ]
 )

$( mkIotaFrag
     "AssessorsReflection"
     [pattern| EAST deeeeeq |]
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "AssessorsPurification"
     [pattern| EAST wdwewewewewewqw |]
     [ [t|Fragment '[IotaVector] '[IotaBoolean]|]
     , [t|Fragment '[IotaEntity] '[IotaBoolean]|]
     ]
 )

$( mkIotaFrag
     "MuninnsReflection"
     [pattern| NORTH_EAST qeewdweddw |]
     [[t|Fragment '[] '[IotaAny]|]]
 )

$( mkIotaFrag
     "HuginnsGambit"
     [pattern| NORTH_WEST eqqwawqaaw |]
     [[t|Fragment '[IotaAny] '[]|]]
 )

$( mkIotaFrag
     "ThanatosReflection"
     [pattern| SOUTH_EAST qqaed |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "NullaryReflection"
     [pattern| EAST d |]
     [[t|Fragment '[] '[IotaNull]|]]
 )

$( mkIotaFrag
     "TrueReflection"
     [pattern| SOUTH_EAST aqae |]
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "FalseReflection"
     [pattern| NORTH_EAST dedq |]
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "VectorReflectionPX"
     [pattern| NORTH_WEST qqqqqea |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFrag
     "VectorReflectionPY"
     [pattern| NORTH_WEST qqqqqew |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFrag
     "VectorReflectionPZ"
     [pattern| NORTH_WEST qqqqqed |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFrag
     "VectorReflectionNX"
     [pattern| SOUTH_WEST eeeeeqa |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFrag
     "VectorReflectionNY"
     [pattern| SOUTH_WEST eeeeeqw |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFrag
     "VectorReflectionNZ"
     [pattern| SOUTH_WEST eeeeeqd |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFrag
     "VectorReflectionZero"
     [pattern| NORTH_WEST qqqqq |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFrag
     "ArcsReflection"
     [pattern| NORTH_EAST qdwdq |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "CirclesReflection"
     [pattern| NORTH_WEST eawae |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "EulersReflection"
     [pattern| EAST aaq |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "EntityPurification"
     [pattern| SOUTH_EAST qqqqqdaqa |]
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkIotaFrag
     "EntityPurificationAnimal"
     [pattern| SOUTH_EAST qqqqqdaqaawa |]
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkIotaFrag
     "EntityPurificationMonster"
     [pattern| SOUTH_EAST qqqqqdaqaawq |]
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkIotaFrag
     "EntityPurificationItem"
     [pattern| SOUTH_EAST qqqqqdaqaaww |]
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkIotaFrag
     "EntityPurificationPlayer"
     [pattern| SOUTH_EAST qqqqqdaqaawe |]
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkIotaFrag
     "EntityPurificationLiving"
     [pattern| SOUTH_EAST qqqqqdaqaawd |]
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkIotaFrag
     "ZoneDistillationAny"
     [pattern| SOUTH_EAST qqqqqwded |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFrag
     "ZoneDistillationAnimal"
     [pattern| SOUTH_EAST qqqqqwdeddwa |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFrag
     "ZoneDistillationNonAnimal"
     [pattern| NORTH_EAST eeeeewaqaawa |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFrag
     "ZoneDistillationMonster"
     [pattern| SOUTH_EAST qqqqqwdeddwq |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFrag
     "ZoneDistillationNonMonster"
     [pattern| NORTH_EAST eeeeewaqaawq |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFrag
     "ZoneDistillationItem"
     [pattern| SOUTH_EAST qqqqqwdeddww |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFrag
     "ZoneDistillationNonItem"
     [pattern| NORTH_EAST eeeeewaqaaww |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFrag
     "ZoneDistillationPlayer"
     [pattern| SOUTH_EAST qqqqqwdeddwe |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFrag
     "ZoneDistillationNonPlayer"
     [pattern| NORTH_EAST eeeeewaqaawe |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFrag
     "ZoneDistillationLiving"
     [pattern| SOUTH_EAST qqqqqwdeddwd |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFrag
     "ZoneDistillationNonLiving"
     [pattern| NORTH_EAST eeeeewaqaawd |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFrag
     "IntegrationDistillation"
     [pattern| SOUTH_WEST edqde |]
     [ [t|forall a. Fragment '[a, IotaList a] '[IotaList a]|]
     , [t|forall a as as'. HReverse (a : as) as' => Fragment '[a, IotaHList as] '[IotaHList as']|]
     ]
 )

$( mkIotaFrag
     "DerivationDecomposition"
     [pattern| NORTH_WEST qaeaq |]
     [ [t|forall a. Fragment '[IotaList a] '[a, IotaList a]|]
     , [t|forall a as as'. HReverse (a : as) as' => Fragment '[IotaHList as'] '[a, IotaHList as]|]
     ]
 )

$( mkIotaFrag
     "SelectionDistillation"
     [pattern| NORTH_WEST deeed |]
     [[t|forall a. Fragment '[IotaNumber, IotaList a] '[a]|]]
 )

iotaThothsGambit :: IotaPattern
iotaThothsGambit = [pattern| NORTH_EAST dadad |]

$( mkIotaFrag
     "SinglesPurification"
     [pattern| EAST adeeed |]
     [[t|forall a. Fragment '[a] '[IotaHList '[a]]|]]
 )

$( mkIotaFrag
     "VacantReflection"
     [pattern| NORTH_EAST qqaeaae |]
     [[t|Fragment '[] '[IotaHList '[]]|]]
 )

$( mkIotaFrag
     "RetrogradePurification"
     [pattern| EAST qqqaede |]
     [ [t|forall a. Fragment '[IotaList a] '[IotaList a]|]
     , [t|forall as as'. HReverse as as' => Fragment '[IotaHList as] '[IotaHList as']|]
     ]
 )

iotaFlocksGambit :: IotaPattern
iotaFlocksGambit = [pattern| SOUTH_WEST ewdqdwe |]

iotaFlocksDisintegration :: IotaPattern
iotaFlocksDisintegration = [pattern| NORTH_WEST qwaeawq |]

class FragFlocksDisintegration as bs | as -> bs where
  fragFlocksDisintegration :: Fragment as bs
  fragFlocksDisintegration = Fragment $ Seq.singleton $ iotaCast iotaFlocksDisintegration
instance (HReverse as ras, HAppendFD ras bs rasbs) => FragFlocksDisintegration (IotaHList as ': bs) rasbs

$( mkIotaFrag
     "LocatorsDistillation"
     [pattern| EAST dedqde |]
     [[t|forall a. Fragment '[a, IotaList a] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "ExcisorsDistillation"
     [pattern| SOUTH_WEST edqdewaqa |]
     [[t|forall a. Fragment '[IotaNumber, IotaList a] '[IotaList a]|]]
 )

$( mkIotaFrag
     "SelectionExaltation"
     [pattern| NORTH_WEST qaeaqwded |]
     [[t|forall a. Fragment '[IotaNumber, IotaNumber, IotaList a] '[a]|]]
 )

$( mkIotaFrag
     "SurgeonsExaltation"
     [pattern| NORTH_WEST wqaeaqw |]
     [[t|forall a. Fragment '[a, IotaNumber, IotaList a] '[IotaList a]|]]
 )

$( mkIotaFrag
     "SpeakersDistillation"
     [pattern| SOUTH_EAST ddewedd |]
     [ [t|forall a. Fragment '[a, IotaList a] '[IotaList a]|]
     , [t|forall a as. Fragment '[a, IotaHList as] '[IotaHList (a ': as)]|]
     ]
 )

$( mkIotaFrag
     "SpeakersDecomposition"
     [pattern| SOUTH_WEST aaqwqaa |]
     [ [t|forall a. Fragment '[IotaList a] '[a, IotaList a]|]
     , [t|forall a as as'. as' ~ a ': as => Fragment '[IotaHList as'] '[a, IotaHList as]|]
     ]
 )

$( mkIotaFrag
     "GulliversPurification"
     [pattern| NORTH_WEST aawawwawwa |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "AlterScale"
     [pattern| NORTH_EAST ddwdwwdwwd |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

iotaConsideration, iotaIntrospection, iotaRetrospection, iotaEvanition :: IotaPattern
iotaConsideration = [pattern| WEST qqqaw |]
iotaIntrospection = [pattern| WEST qqq |]
iotaRetrospection = [pattern| EAST eee |]
iotaEvanition = [pattern| EAST eeedw |]

class IotaBookkeepersGambit keep where
  iotaBookkeepersGambit :: IotaPattern
instance IotaBookkeepersGambit '[False] where
  iotaBookkeepersGambit = [pattern| SOUTH_EAST a |]
instance IotaBookkeepersGambit '[True] where
  iotaBookkeepersGambit = [pattern| EAST |]
instance IotaBookkeepersGambit (False ': as) => IotaBookkeepersGambit (False ': False ': as) where
  iotaBookkeepersGambit = IotaPattern dir $ ang <> [angles| da |]
   where
    IotaPattern dir ang = iotaBookkeepersGambit @(False ': as)
instance IotaBookkeepersGambit (False ': as) => IotaBookkeepersGambit (True ': False ': as) where
  iotaBookkeepersGambit = IotaPattern dir $ ang <> [angles| e |]
   where
    IotaPattern dir ang = iotaBookkeepersGambit @(False ': as)
instance IotaBookkeepersGambit (True ': as) => IotaBookkeepersGambit (False ': True ': as) where
  iotaBookkeepersGambit = IotaPattern dir $ ang <> [angles| ea |]
   where
    IotaPattern dir ang = iotaBookkeepersGambit @(True ': as)
instance IotaBookkeepersGambit (True ': as) => IotaBookkeepersGambit (True ': True ': as) where
  iotaBookkeepersGambit = IotaPattern dir $ ang <> [angles| w |]
   where
    IotaPattern dir ang = iotaBookkeepersGambit @(True ': as)

class IotaBookkeepersGambit keep => FragBookkeepersGambit keep as bs | keep as -> bs where
  fragBookkeepersGambit :: Fragment as bs
  fragBookkeepersGambit = Fragment $ Seq.singleton $ iotaCast $ iotaBookkeepersGambit @keep
instance {-# OVERLAPPING #-} FragBookkeepersGambit '[False] (a ': as) as
instance {-# OVERLAPPING #-} FragBookkeepersGambit '[True] (a ': as) (a ': as)
instance
  (IotaBookkeepersGambit (False ': keep), FragBookkeepersGambit keep as bs) =>
  FragBookkeepersGambit (False ': keep) (a ': as) bs
instance
  (IotaBookkeepersGambit (True ': keep), FragBookkeepersGambit keep as bs) =>
  FragBookkeepersGambit (True ': keep) (a ': as) (a ': bs)

precomputedNumericalReflectionSuffixes :: Seq [Angle]
precomputedNumericalReflectionSuffixes = Seq.fromList $ [] : suffixes
 where
  raw = $(embedFileRelative "src/Haskcasting/Patterns/Hexcasting/precomputed_numbers.txt")
  rawLines = filter (not . BC.null) $ map (BC.strip) $ BC.split '\n' raw
  suffixes = map (parseAngles . BC.unpack) rawLines
  parseAngles as = fromMaybe (error $ "invalid angles: '" <> as <> "'") $ traverse angleParse as

iotaNumericalReflection :: forall n. KnownNat n => IotaPattern
iotaNumericalReflection = IotaPattern direction $ prefix <> suffix
 where
  prefix = [angles| aqaa |]
  direction = DirectionSE
  nInt :: Int
  nInt = fromInteger $ naturalToInteger $ natVal $ Proxy @n
  suffix = fromMaybe err $ precomputedNumericalReflectionSuffixes Seq.!? nInt
  err = error "number too large for numerical reflection"

iotaNegativeNumericalReflection :: forall n. KnownNat n => IotaPattern
iotaNegativeNumericalReflection = IotaPattern direction $ prefix <> suffix
 where
  prefix = [angles| dedd |]
  direction = DirectionNE
  nInt :: Int
  nInt = fromInteger $ naturalToInteger $ natVal $ Proxy @n
  suffix = fromMaybe err $ precomputedNumericalReflectionSuffixes Seq.!? nInt
  err = error "number too large for numerical reflection"

fragNumericalReflection :: forall n as. KnownNat n => Fragment as (IotaNumber ': as)
fragNumericalReflection = Fragment $ Seq.singleton $ iotaCast $ iotaNumericalReflection @n

fragNegativeNumericalReflection :: forall n as. KnownNat n => Fragment as (IotaNumber ': as)
fragNegativeNumericalReflection = Fragment $ Seq.singleton $ iotaCast $ iotaNegativeNumericalReflection @n
