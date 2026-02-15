{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskcasting.Patterns.Hexcasting where

import Data.HList (HAppendListR)
import Haskcasting.Fragment (Fragment)
import Haskcasting.Iota (
  IotaBoolean,
  IotaEntity,
  IotaHList,
  IotaList,
  IotaNumber,
  IotaPattern (..),
  IotaVector, IotaAny,
 )
import Haskcasting.Patterns.TH (angles, mkIotaFrag, pattern)

-- iotaMindsReflection :: IotaPattern
-- iotaMindsReflection = [pattern| NORTH_EAST qaq |]

-- fragMindsReflection :: Fragment '[] '[IotaEntity]
-- fragMindsReflection = Fragment $ Seq.singleton $ iotaCast iotaMindsReflection

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
-- $( mkIotaFrag
--      "GeminiGambit"
--      [pattern| EAST aadaadaa |]
--      [[t|Fragment '[IotaAny, IotaNumber] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "FishermansGambit"
--      [pattern| WEST ddad |]
--      [[t|Fragment '[IotaNumber] '[IotaAny]|]]
--  )

-- $( mkIotaFrag
--      "FishermansGambitII"
--      [pattern| EAST aada |]
--      [[t|Fragment '[IotaNumber] '[IotaAny]|]]
--  )

-- $( mkIotaFrag
--      "SwindlersGambit"
--      [pattern| SOUTH_EAST qaawdde |]
--      [[t|Fragment '[IotaAnyList, IotaNumber] '[IotaAnyList]|]]
--  )
$( mkIotaFrag
     "AdditiveDistillation"
     [pattern| NORTH_EAST waaw |]
     [ [t|forall as bs asbs. HAppendListR as bs ~ asbs => Fragment '[IotaHList as, IotaHList bs] '[IotaHList asbs]|]
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
     -- , [t|forall a. Fragment '[IotaList a, IotaList a] '[IotaList a]|]
     , [t|Fragment '[IotaBoolean, IotaBoolean] '[IotaBoolean]|]
     ]
 )
$( mkIotaFrag
     "DisjunctionDistillation"
     [pattern| SOUTH_EAST waw |]
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|forall a. Fragment '[IotaList a, IotaList a] '[IotaList a]|]
     -- , [t|forall a. Fragment '[IotaList a, IotaList a] '[IotaList a]|]
     , [t|Fragment '[IotaBoolean, IotaBoolean] '[IotaBoolean]|]
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
     -- , [t|forall a. Fragment '[IotaList a, IotaList a] '[IotaList a]|]
     , [t|Fragment '[IotaBoolean, IotaBoolean] '[IotaBoolean]|]
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

-- $( mkIotaFrag
--      "InequalityDistillation"
--      [pattern| EAST da |]
--      [[t|Fragment '[IotaAny, IotaAny] '[IotaBoolean]|]]
--  )

-- $( mkIotaFrag
--      "AugursPurification"
--      [pattern| NORTH_EAST aw |]
--      [[t|Fragment '[IotaAny] '[IotaBoolean]|]]
--  )

-- $( mkIotaFrag
--      "AugursExaltation"
--      [pattern| SOUTH_EAST awdd |]
--      [[t|Fragment '[IotaBoolean, IotaAny, IotaAny] '[IotaAny]|]]
--  )

-- $( mkIotaFrag
--      "EntropyReflection"
--      [pattern| NORTH_WEST eqqq |]
--      [[t|Fragment '[] '[IotaNumber]|]]
--  )

-- $( mkIotaFrag
--      "SinePurification"
--      [pattern| SOUTH_EAST qqqqqaa |]
--      [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
--  )

-- $( mkIotaFrag
--      "CosinePurification"
--      [pattern| SOUTH_EAST qqqqqad |]
--      [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
--  )

-- $( mkIotaFrag
--      "TangentPurification"
--      [pattern| SOUTH_WEST wqqqqqadq |]
--      [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
--  )

-- $( mkIotaFrag
--      "InverseSinePurification"
--      [pattern| SOUTH_EAST ddeeeee |]
--      [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
--  )

-- $( mkIotaFrag
--      "InverseCosinePurification"
--      [pattern| NORTH_EAST adeeeee |]
--      [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
--  )

-- $( mkIotaFrag
--      "InverseTangentPurification"
--      [pattern| NORTH_EAST eadeeeeew |]
--      [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
--  )

-- $( mkIotaFrag
--      "InverseTangentDistillation"
--      [pattern| WEST deadeeeeewd |]
--      [[t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]]
--  )

-- $( mkIotaFrag
--      "LogarithmicDistillation"
--      [pattern| NORTH_WEST eqaqe |]
--      [[t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]]
--  )

-- $( mkIotaFrag
--      "ModulusDistillation"
--      [pattern| NORTH_EAST addwaad |]
--      [[t|Fragment '[IotaAny, IotaAny] '[IotaAny]|]]
--  )

-- $( mkIotaFrag
--      "UniquenessPurification"
--      [pattern| NORTH_EAST aweaqa |]
--      [[t|Fragment '[IotaAny] '[IotaAny]|]]
--  )

-- $( mkIotaFrag
--      "Reveal"
--      [pattern| NORTH_EAST de |]
--      [[t|Fragment '[IotaAny] '[IotaAny]|]]
--  )

-- $( mkIotaFrag
--      "Explosion"
--      [pattern| EAST aawaawaa |]
--      [[t|Fragment '[IotaVector, IotaNumber] '[]|]]
--  )

-- $( mkIotaFrag
--      "Fireball"
--      [pattern| EAST ddwddwdd |]
--      [[t|Fragment '[IotaVector, IotaNumber] '[]|]]
--  )

-- $( mkIotaFrag
--      "Impulse"
--      [pattern| SOUTH_WEST awqqqwaqw |]
--      [[t|Fragment '[IotaEntity, IotaVector] '[]|]]
--  )

-- $( mkIotaFrag
--      "Blink"
--      [pattern| SOUTH_WEST awqqqwaq |]
--      [[t|Fragment '[IotaEntity, IotaNumber] '[]|]]
--  )

-- $( mkIotaFrag
--      "BreakBlock"
--      [pattern| EAST qaqqqqq |]
--      [[t|Fragment '[IotaVector] '[]|]]
--  )

-- $( mkIotaFrag
--      "PlaceBlock"
--      [pattern| SOUTH_WEST eeeeede |]
--      [[t|Fragment '[IotaVector] '[]|]]
--  )

-- $( mkIotaFrag
--      "InternalizePigment"
--      [pattern| EAST awddwqawqwawq |]
--      [[t|Fragment '[] '[]|]]
--  )

-- $( mkIotaFrag
--      "CastersGlamour"
--      [pattern| WEST dwaawedwewdwe |]
--      [[t|Fragment '[] '[]|]]
--  )

-- $( mkIotaFrag
--      "CreateWater"
--      [pattern| SOUTH_EAST aqawqadaq |]
--      [[t|Fragment '[IotaVector] '[]|]]
--  )

-- $( mkIotaFrag
--      "DestroyLiquid"
--      [pattern| SOUTH_WEST dedwedade |]
--      [[t|Fragment '[IotaVector] '[]|]]
--  )

-- $( mkIotaFrag
--      "Ignite"
--      [pattern| SOUTH_EAST aaqawawa |]
--      [[t|Fragment '[IotaAny] '[]|]]
--  )

-- $( mkIotaFrag
--      "ExtinguishArea"
--      [pattern| SOUTH_WEST ddedwdwd |]
--      [[t|Fragment '[IotaVector] '[]|]]
--  )

-- $( mkIotaFrag
--      "ConjureBlock"
--      [pattern| NORTH_EAST qqa |]
--      [[t|Fragment '[IotaVector] '[]|]]
--  )

-- $( mkIotaFrag
--      "ConjureLight"
--      [pattern| NORTH_EAST qqd |]
--      [[t|Fragment '[IotaVector] '[]|]]
--  )

-- $( mkIotaFrag
--      "Overgrow"
--      [pattern| NORTH_EAST wqaqwawqaqw |]
--      [[t|Fragment '[IotaVector] '[]|]]
--  )

-- $( mkIotaFrag
--      "RechargeItem"
--      [pattern| NORTH_WEST qqqqqwaeaeaeaeaea |]
--      [[t|Fragment '[IotaEntity] '[]|]]
--  )

-- $( mkIotaFrag
--      "EraseItem"
--      [pattern| EAST qdqawwaww |]
--      [[t|Fragment '[] '[]|]]
--  )

-- $( mkIotaFrag
--      "EdifySapling"
--      [pattern| NORTH_EAST wqaqwd |]
--      [[t|Fragment '[IotaVector] '[]|]]
--  )

-- $( mkIotaFrag
--      "MakeNote"
--      [pattern| WEST adaa |]
--      [[t|Fragment '[IotaVector, IotaNumber, IotaNumber] '[]|]]
--  )

-- $( mkIotaFrag
--      "CraftCypher"
--      [pattern| EAST waqqqqq |]
--      [[t|Fragment '[IotaEntity, IotaAnyList] '[]|]]
--  )

-- $( mkIotaFrag
--      "CraftTrinket"
--      [pattern| EAST wwaqqqqqeaqeaeqqqeaeq |]
--      [[t|Fragment '[IotaEntity, IotaAnyList] '[]|]]
--  )

-- $( mkIotaFrag
--      "CraftArtifact"
--      [pattern| EAST wwaqqqqqeawqwqwqwqwqwwqqeadaeqqeqqeadaeqq |]
--      [[t|Fragment '[IotaEntity, IotaAnyList] '[]|]]
--  )

-- $( mkIotaFrag
--      "CraftPhial"
--      [pattern| SOUTH_WEST aqqqaqwwaqqqqqeqaqqqawwqwqwqwqwqw |]
--      [[t|Fragment '[IotaEntity] '[]|]]
--  )

-- $( mkIotaFrag
--      "WhiteSunsNadir"
--      [pattern| NORTH_WEST qqqqqaqwawaw |]
--      [[t|Fragment '[IotaEntity, IotaNumber, IotaNumber] '[]|]]
--  )

-- $( mkIotaFrag
--      "BlueSunsNadir"
--      [pattern| WEST qqqqqawwawawd |]
--      [[t|Fragment '[IotaEntity, IotaNumber] '[]|]]
--  )

-- $( mkIotaFrag
--      "BlackSunsNadir"
--      [pattern| SOUTH_WEST qqqqqaewawawe |]
--      [[t|Fragment '[IotaEntity, IotaNumber, IotaNumber] '[]|]]
--  )

-- $( mkIotaFrag
--      "RedSunsNadir"
--      [pattern| SOUTH_EAST qqqqqadwawaww |]
--      [[t|Fragment '[IotaEntity, IotaNumber, IotaNumber] '[]|]]
--  )

-- $( mkIotaFrag
--      "GreenSunsNadir"
--      [pattern| SOUTH_EAST qqqqqadwawaw |]
--      [[t|Fragment '[IotaEntity, IotaNumber, IotaNumber] '[]|]]
--  )

-- $( mkIotaFrag
--      "WhiteSunsZenith"
--      [pattern| NORTH_WEST qqqqaawawaedd |]
--      [[t|Fragment '[IotaEntity, IotaNumber, IotaNumber] '[]|]]
--  )

-- $( mkIotaFrag
--      "BlueSunsZenith"
--      [pattern| WEST qqqaawawaeqdd |]
--      [[t|Fragment '[IotaEntity, IotaNumber] '[]|]]
--  )

-- $( mkIotaFrag
--      "BlackSunsZenith"
--      [pattern| SOUTH_WEST qqaawawaeqqdd |]
--      [[t|Fragment '[IotaEntity, IotaNumber, IotaNumber] '[]|]]
--  )

-- $( mkIotaFrag
--      "RedSunsZenith"
--      [pattern| SOUTH_EAST qaawawaeqqqdd |]
--      [[t|Fragment '[IotaEntity, IotaNumber, IotaNumber] '[]|]]
--  )

-- $( mkIotaFrag
--      "GreenSunsZenith"
--      [pattern| EAST aawawaeqqqqdd |]
--      [[t|Fragment '[IotaEntity, IotaNumber, IotaNumber] '[]|]]
--  )

-- $( mkIotaFrag
--      "AnchoritesFlight"
--      [pattern| SOUTH_WEST awawaawq |]
--      [[t|Fragment '[IotaEntity, IotaNumber] '[]|]]
--  )

-- $( mkIotaFrag
--      "WayfarersFlight"
--      [pattern| NORTH_EAST dwdwdewq |]
--      [[t|Fragment '[IotaEntity, IotaNumber] '[]|]]
--  )

-- $( mkIotaFrag
--      "AviatorsPurification"
--      [pattern| NORTH_EAST dwdwdeweaqa |]
--      [[t|Fragment '[IotaEntity] '[IotaBoolean]|]]
--  )

-- $( mkIotaFrag
--      "SummonSentinel"
--      [pattern| EAST waeawae |]
--      [[t|Fragment '[IotaVector] '[]|]]
--  )

-- $( mkIotaFrag
--      "BanishSentinel"
--      [pattern| NORTH_EAST qdwdqdw |]
--      [[t|Fragment '[] '[]|]]
--  )

-- $( mkIotaFrag
--      "LocateSentinel"
--      [pattern| EAST waeawaede |]
--      [[t|Fragment '[] '[IotaVector]|]]
--  )

-- $( mkIotaFrag
--      "WayfindSentinel"
--      [pattern| EAST waeawaedwa |]
--      [[t|Fragment '[IotaVector] '[IotaVector]|]]
--  )

-- $( mkIotaFrag
--      "SummonLightning"
--      [pattern| EAST waadwawdaaweewq |]
--      [[t|Fragment '[IotaVector] '[]|]]
--  )

-- $( mkIotaFrag
--      "Altiora"
--      [pattern| NORTH_WEST eawwaeawawaa |]
--      [[t|Fragment '[IotaEntity] '[]|]]
--  )

-- $( mkIotaFrag
--      "CreateLava"
--      [pattern| EAST eaqawqadaqd |]
--      [[t|Fragment '[IotaVector] '[]|]]
--  )

-- $( mkIotaFrag
--      "GreaterTeleport"
--      [pattern| EAST wwwqqqwwwqqeqqwwwqqwqqdqqqqqdqq |]
--      [[t|Fragment '[IotaEntity, IotaVector] '[]|]]
--  )

-- $( mkIotaFrag
--      "SummonGreaterSentinel"
--      [pattern| EAST waeawaeqqqwqwqqwq |]
--      [[t|Fragment '[IotaVector] '[]|]]
--  )

-- $( mkIotaFrag
--      "DispelRain"
--      [pattern| EAST eeewwweeewwaqqddqdqd |]
--      [[t|Fragment '[] '[]|]]
--  )

-- $( mkIotaFrag
--      "SummonRain"
--      [pattern| WEST wwweeewwweewdawdwad |]
--      [[t|Fragment '[] '[]|]]
--  )

-- $( mkIotaFrag
--      "FlayMind"
--      [pattern| NORTH_EAST qeqwqwqwqwqeqaeqeaqeqaeqaqded |]
--      [[t|Fragment '[IotaEntity, IotaVector] '[]|]]
--  )

-- $( mkIotaFrag
--      "AkashasDistillation"
--      [pattern| WEST qqqwqqqqqaq |]
--      [[t|Fragment '[IotaVector, IotaPattern] '[IotaAny]|]]
--  )

-- $( mkIotaFrag
--      "AkashasGambit"
--      [pattern| EAST eeeweeeeede |]
--      [[t|Fragment '[IotaVector, IotaPattern, IotaAny] '[]|]]
--  )

-- $( mkIotaFrag
--      "HermesGambit"
--      [pattern| SOUTH_EAST deaqq |]
--      [[t|Fragment '[IotaAny] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "IrisGambit"
--      [pattern| NORTH_WEST qwaqde |]
--      [[t|Fragment '[IotaAny] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "CharonsGambit"
--      [pattern| SOUTH_WEST aqdee |]
--      [[t|Fragment '[] '[]|]]
--  )

-- $( mkIotaFrag
--      "ScribesReflection"
--      [pattern| EAST aqqqqq |]
--      [[t|Fragment '[] '[IotaAny]|]]
--  )

-- $( mkIotaFrag
--      "ChroniclersPurification"
--      [pattern| EAST wawqwqwqwqwqw |]
--      [[t|Fragment '[IotaAny] '[IotaAny]|]]
--  )

-- $( mkIotaFrag
--      "ScribesGambit"
--      [pattern| EAST deeeee |]
--      [[t|Fragment '[IotaAny] '[]|]]
--  )

-- $( mkIotaFrag
--      "ChroniclersGambit"
--      [pattern| EAST wdwewewewewew |]
--      [[t|Fragment '[IotaAny, IotaAny] '[]|]]
--  )

-- $( mkIotaFrag
--      "AuditorsReflection"
--      [pattern| EAST aqqqqqe |]
--      [[t|Fragment '[] '[IotaBoolean]|]]
--  )

-- $( mkIotaFrag
--      "AuditorsPurification"
--      [pattern| EAST wawqwqwqwqwqwew |]
--      [[t|Fragment '[IotaAny] '[IotaBoolean]|]]
--  )

-- $( mkIotaFrag
--      "AssessorsReflection"
--      [pattern| EAST deeeeeq |]
--      [[t|Fragment '[] '[IotaBoolean]|]]
--  )

-- $( mkIotaFrag
--      "AssessorsPurification"
--      [pattern| EAST wdwewewewewewqw |]
--      [[t|Fragment '[IotaAny] '[IotaBoolean]|]]
--  )

-- $( mkIotaFrag
--      "MuninnsReflection"
--      [pattern| NORTH_EAST qeewdweddw |]
--      [[t|Fragment '[] '[IotaAny]|]]
--  )

-- $( mkIotaFrag
--      "HuginnsGambit"
--      [pattern| NORTH_WEST eqqwawqaaw |]
--      [[t|Fragment '[IotaAny] '[]|]]
--  )

-- $( mkIotaFrag
--      "ThanatosReflection"
--      [pattern| SOUTH_EAST qqaed |]
--      [[t|Fragment '[] '[IotaNumber]|]]
--  )

-- $( mkIotaFrag
--      "NullaryReflection"
--      [pattern| EAST d |]
--      [[t|Fragment '[] '[IotaNull]|]]
--  )

-- $( mkIotaFrag
--      "TrueReflection"
--      [pattern| SOUTH_EAST aqae |]
--      [[t|Fragment '[] '[IotaBoolean]|]]
--  )

-- $( mkIotaFrag
--      "FalseReflection"
--      [pattern| NORTH_EAST dedq |]
--      [[t|Fragment '[] '[IotaBoolean]|]]
--  )

-- $( mkIotaFrag
--      "VectorReflection+X"
--      [pattern| NORTH_WEST qqqqqea |]
--      [[t|Fragment '[] '[IotaVector]|]]
--  )

-- $( mkIotaFrag
--      "VectorReflection+Y"
--      [pattern| NORTH_WEST qqqqqew |]
--      [[t|Fragment '[] '[IotaVector]|]]
--  )

-- $( mkIotaFrag
--      "VectorReflection+Z"
--      [pattern| NORTH_WEST qqqqqed |]
--      [[t|Fragment '[] '[IotaVector]|]]
--  )

-- $( mkIotaFrag
--      "VectorReflection-X"
--      [pattern| SOUTH_WEST eeeeeqa |]
--      [[t|Fragment '[] '[IotaVector]|]]
--  )

-- $( mkIotaFrag
--      "VectorReflection-Y"
--      [pattern| SOUTH_WEST eeeeeqw |]
--      [[t|Fragment '[] '[IotaVector]|]]
--  )

-- $( mkIotaFrag
--      "VectorReflection-Z"
--      [pattern| SOUTH_WEST eeeeeqd |]
--      [[t|Fragment '[] '[IotaVector]|]]
--  )

-- $( mkIotaFrag
--      "VectorReflectionZero"
--      [pattern| NORTH_WEST qqqqq |]
--      [[t|Fragment '[] '[IotaVector]|]]
--  )

-- $( mkIotaFrag
--      "ArcsReflection"
--      [pattern| NORTH_EAST qdwdq |]
--      [[t|Fragment '[] '[IotaNumber]|]]
--  )

-- $( mkIotaFrag
--      "CirclesReflection"
--      [pattern| NORTH_WEST eawae |]
--      [[t|Fragment '[] '[IotaNumber]|]]
--  )

-- $( mkIotaFrag
--      "EulersReflection"
--      [pattern| EAST aaq |]
--      [[t|Fragment '[] '[IotaNumber]|]]
--  )

-- $( mkIotaFrag
--      "EntityPurification"
--      [pattern| SOUTH_EAST qqqqqdaqa |]
--      [[t|Fragment '[IotaVector] '[IotaEntity]|]]
--  )

-- $( mkIotaFrag
--      "EntityPurificationAnimal"
--      [pattern| SOUTH_EAST qqqqqdaqaawa |]
--      [[t|Fragment '[IotaVector] '[IotaEntity]|]]
--  )

-- $( mkIotaFrag
--      "EntityPurificationMonster"
--      [pattern| SOUTH_EAST qqqqqdaqaawq |]
--      [[t|Fragment '[IotaVector] '[IotaEntity]|]]
--  )

-- $( mkIotaFrag
--      "EntityPurificationItem"
--      [pattern| SOUTH_EAST qqqqqdaqaaww |]
--      [[t|Fragment '[IotaVector] '[IotaEntity]|]]
--  )

-- $( mkIotaFrag
--      "EntityPurificationPlayer"
--      [pattern| SOUTH_EAST qqqqqdaqaawe |]
--      [[t|Fragment '[IotaVector] '[IotaEntity]|]]
--  )

-- $( mkIotaFrag
--      "EntityPurificationLiving"
--      [pattern| SOUTH_EAST qqqqqdaqaawd |]
--      [[t|Fragment '[IotaVector] '[IotaEntity]|]]
--  )

-- $( mkIotaFrag
--      "ZoneDistillationAny"
--      [pattern| SOUTH_EAST qqqqqwded |]
--      [[t|Fragment '[IotaVector, IotaNumber] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "ZoneDistillationAnimal"
--      [pattern| SOUTH_EAST qqqqqwdeddwa |]
--      [[t|Fragment '[IotaVector, IotaNumber] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "ZoneDistillationNonAnimal"
--      [pattern| NORTH_EAST eeeeewaqaawa |]
--      [[t|Fragment '[IotaVector, IotaNumber] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "ZoneDistillationMonster"
--      [pattern| SOUTH_EAST qqqqqwdeddwq |]
--      [[t|Fragment '[IotaVector, IotaNumber] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "ZoneDistillationNonMonster"
--      [pattern| NORTH_EAST eeeeewaqaawq |]
--      [[t|Fragment '[IotaVector, IotaNumber] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "ZoneDistillationItem"
--      [pattern| SOUTH_EAST qqqqqwdeddww |]
--      [[t|Fragment '[IotaVector, IotaNumber] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "ZoneDistillationNonItem"
--      [pattern| NORTH_EAST eeeeewaqaaww |]
--      [[t|Fragment '[IotaVector, IotaNumber] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "ZoneDistillationPlayer"
--      [pattern| SOUTH_EAST qqqqqwdeddwe |]
--      [[t|Fragment '[IotaVector, IotaNumber] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "ZoneDistillationNonPlayer"
--      [pattern| NORTH_EAST eeeeewaqaawe |]
--      [[t|Fragment '[IotaVector, IotaNumber] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "ZoneDistillationLiving"
--      [pattern| SOUTH_EAST qqqqqwdeddwd |]
--      [[t|Fragment '[IotaVector, IotaNumber] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "ZoneDistillationNonLiving"
--      [pattern| NORTH_EAST eeeeewaqaawd |]
--      [[t|Fragment '[IotaVector, IotaNumber] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "IntegrationDistillation"
--      [pattern| SOUTH_WEST edqde |]
--      [[t|Fragment '[IotaAnyList, IotaAny] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "DerivationDecomposition"
--      [pattern| NORTH_WEST qaeaq |]
--      [[t|Fragment '[IotaAny] '[IotaAny, IotaAny]|]]
--  )

-- $( mkIotaFrag
--      "SelectionDistillation"
--      [pattern| NORTH_WEST deeed |]
--      [[t|Fragment '[IotaAny, IotaAny] '[IotaAny]|]]
--  )

-- $( mkIotaFrag
--      "ThothsGambit"
--      [pattern| NORTH_EAST dadad |]
--      [[t|Fragment '[IotaAnyList, IotaAnyList] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "SinglesPurification"
--      [pattern| EAST adeeed |]
--      [[t|Fragment '[IotaAny] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "VacantReflection"
--      [pattern| NORTH_EAST qqaeaae |]
--      [[t|Fragment '[] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "RetrogradePurification"
--      [pattern| EAST qqqaede |]
--      [[t|Fragment '[IotaAny] '[IotaAny]|]]
--  )

-- $( mkIotaFrag
--      "FlocksGambit"
--      [pattern| SOUTH_WEST ewdqdwe |]
--      [[t|Fragment '[IotaAnyList, IotaNumber] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "FlocksDisintegration"
--      [pattern| NORTH_WEST qwaeawq |]
--      [[t|Fragment '[IotaAny] '[IotaAnyList]|]]
--  )

-- $( mkIotaFrag
--      "LocatorsDistillation"
--      [pattern| EAST dedqde |]
--      [[t|Fragment '[IotaAny, IotaAny] '[IotaAny]|]]
--  )

-- $( mkIotaFrag
--      "ExcisorsDistillation"
--      [pattern| SOUTH_WEST edqdewaqa |]
--      [[t|Fragment '[IotaAny, IotaNumber] '[IotaAny]|]]
--  )

-- $( mkIotaFrag
--      "SelectionExaltation"
--      [pattern| NORTH_WEST qaeaqwded |]
--      [[t|Fragment '[IotaAny, IotaNumber, IotaNumber] '[IotaAny]|]]
--  )

-- $( mkIotaFrag
--      "SurgeonsExaltation"
--      [pattern| NORTH_WEST wqaeaqw |]
--      [[t|Fragment '[IotaAny, IotaNumber, IotaAny] '[IotaAny]|]]
--  )

-- $( mkIotaFrag
--      "SpeakersDistillation"
--      [pattern| SOUTH_EAST ddewedd |]
--      [[t|Fragment '[IotaAny, IotaAny] '[IotaAny]|]]
--  )

-- $( mkIotaFrag
--      "SpeakersDecomposition"
--      [pattern| SOUTH_WEST aaqwqaa |]
--      [[t|Fragment '[IotaAny] '[IotaAny, IotaAny]|]]
--  )

-- $( mkIotaFrag
--      "GulliversPurification"
--      [pattern| NORTH_WEST aawawwawwa |]
--      [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
--  )

-- $( mkIotaFrag
--      "AlterScale"
--      [pattern| NORTH_EAST ddwdwwdwwd |]
--      [[t|Fragment '[IotaEntity, IotaNumber] '[]|]]
--  )

-- $( mkIotaFrag
--      "Consideration"
--      [pattern| WEST qqqaw |]
--      [[t|Fragment '[] '[]|]]
--  )

-- $( mkIotaFrag
--      "Introspection"
--      [pattern| WEST qqq |]
--      [[t|Fragment '[] '[]|]]
--  )

-- $( mkIotaFrag
--      "Retrospection"
--      [pattern| EAST eee |]
--      [[t|Fragment '[] '[]|]]
--  )

-- $( mkIotaFrag
--      "Evanition"
--      [pattern| EAST eeedw |]
--      [[t|Fragment '[] '[]|]]
--  )

-- -- $( mkPattern
-- --      "AugursExaltation"
-- --      [pattern| SOUTH_EAST awdd |]
-- --      [[t|forall a b c. (IotaCast a c, IotaCast b c) => Fragment '[a, a, IotaBoolean] '[a]|]]
-- --  )

-- Special Patterns
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

-- fragBookkeepersGambit :: Fragment
