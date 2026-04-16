{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Patterns.Hexal where

import Haskcasting.ExprLang.TH (mkGreatIotaFragExpr, mkIotaFragExpr)
import Haskcasting.Fragment (Fragment)
import Haskcasting.Iota (
  IotaAny,
  IotaAnyList,
  IotaBoolean,
  IotaEntity,
  IotaExec,
  IotaList,
  IotaNull,
  IotaNumber,
  IotaPattern,
  IotaVector,
 )
import Haskcasting.Iota.Hexal (IotaGate, IotaMote)
import Haskcasting.Iota.Moreiotas (IotaItemType)
import Haskcasting.Pattern (pattern)

$( mkIotaFragExpr
     "TimekeepersReflection"
     [pattern| NORTH_WEST ddwaa |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "DiversPurification"
     [pattern| NORTH_WEST aqawdwaqawd |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "NursesPurification"
     [pattern| NORTH_WEST aqwawqa |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "SquiresPurification"
     [pattern| NORTH_WEST wqqqqw |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "BoxersPurification"
     [pattern| EAST aeqqqqea |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "LamplightersPurification"
     [pattern| NORTH_EAST qedqde |]
     [[t|Fragment '[IotaVector] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "FactorialPurification"
     [pattern| SOUTH_EAST wawdedwaw |]
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "RunningSumPurification"
     [pattern| WEST aea |]
     [[t|Fragment '[IotaList IotaNumber] '[IotaList IotaNumber]|]]
 )

$( mkIotaFragExpr
     "RunningProductPurification"
     [pattern| NORTH_EAST qaawaaq |]
     [[t|Fragment '[IotaList IotaNumber] '[IotaList IotaNumber]|]]
 )

$( mkIotaFragExpr
     "MnemosynesGambit"
     [pattern| NORTH_EAST eweeewedqdeddw |]
     [[t|Fragment '[IotaPattern, IotaVector] '[]|]]
 )

$( mkIotaFragExpr
     "ElysiumsGambit"
     [pattern| SOUTH_EAST qwqqqwqaeaqaaw |]
     [[t|Fragment '[IotaPattern, IotaVector] '[]|]]
 )

$( mkIotaFragExpr
     "LethesGambit"
     [pattern| SOUTH_EAST qwqqqwqaww |]
     [[t|Fragment '[IotaPattern] '[]|]]
 )

$( mkIotaFragExpr
     "BrighsGambit"
     [pattern| SOUTH_WEST eweeewedww |]
     [[t|Fragment '[IotaPattern] '[]|]]
 )

$( mkIotaFragExpr
     "Smelt"
     [pattern| EAST wqqqwqqadad |]
     [ [t|Fragment '[IotaVector] '[]|]
     , [t|Fragment '[IotaEntity] '[]|]
     , [t|Fragment '[IotaMote] '[]|]
     ]
 )

$( mkIotaFragExpr
     "Freeze"
     [pattern| WEST weeeweedada |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFragExpr
     "FallingBlock"
     [pattern| EAST wqwawqwqwqwqwqw |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFragExpr
     "PlaceBlockII"
     [pattern| WEST eeeeedeeeee |]
     [ [t|Fragment '[IotaVector, IotaItemType] '[]|]
     , [t|Fragment '[IotaVector, IotaMote] '[]|]
     ]
 )

$( mkIotaFragExpr
     "Particles"
     [pattern| NORTH_EAST eqqqqa |]
     [ [t|Fragment '[IotaVector] '[]|]
     , [t|Fragment '[IotaList IotaVector] '[]|]
     ]
 )

$( mkIotaFragExpr
     "SummonProjectileWisp"
     [pattern| NORTH_WEST aqaeqeeeee |]
     [[t|forall s. Fragment '[IotaNumber, IotaVector, IotaVector, IotaExec '[IotaEntity] s] '[]|]]
 )

$( mkIotaFragExpr
     "SummonCyclicWisp"
     [pattern| NORTH_WEST aqaweewaqawee |]
     [[t|forall s. Fragment '[IotaNumber, IotaVector, IotaExec '[IotaEntity] s] '[]|]]
 )

$( mkIotaFragExpr
     "IdentityReflection"
     [pattern| NORTH_EAST dedwqqwdedwqqaw |]
     [[t|Fragment '[] '[IotaEntity]|]]
 )

$( mkIotaFragExpr
     "ReservoirReflection"
     [pattern| NORTH_WEST aqaweewaqaweedw |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "ManagersPurification"
     [pattern| SOUTH_EAST aweewaqaweewaawww |]
     [[t|Fragment '[IotaEntity] '[IotaAnyList]|]]
 )

$( mkIotaFragExpr
     "AllegianceDistillation"
     [pattern| SOUTH_WEST dwqqwdedwqqwddwww |]
     [[t|Fragment '[IotaEntity, IotaEntity] '[IotaBoolean]|]]
 )

$( mkIotaFragExpr
     "PathfindersGambit"
     [pattern| WEST awqwawqaw |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFragExpr
     "PathfindersReflection"
     [pattern| EAST ewdwewdew |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "Haste"
     [pattern| WEST aeawqqqae |]
     [[t|Fragment '[IotaNumber] '[]|]]
 )

$( mkIotaFragExpr
     "SpeedometersReflection"
     [pattern| EAST eeewdqdee |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "AllowTransfer"
     [pattern| NORTH_WEST qqqqqewwqeeeee |]
     [[t|Fragment '[IotaNumber] '[]|]]
 )

$( mkIotaFragExpr
     "DisallowTransfer"
     [pattern| NORTH_WEST qqqqqeqdeddweqqqqq |]
     [[t|Fragment '[IotaNumber] '[]|]]
 )

$( mkIotaFragExpr
     "AllowTransferOthers"
     [pattern| SOUTH_WEST eeeeeqwweqqqqq |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "DisallowTransferOthers"
     [pattern| SOUTH_WEST eeeeeqeaqaawqeeeee |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "EntityPurificationWisp"
     [pattern| SOUTH_EAST qqwdedwqqdaqaaww |]
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkIotaFragExpr
     "ZoneDistillationWisp"
     [pattern| SOUTH_EAST qqwdedwqqwdeddww |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFragExpr
     "ZoneDistillationNonWisp"
     [pattern| NORTH_EAST eewaqaweewaqaaww |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFragExpr
     "DelayWisp"
     [pattern| NORTH_WEST aqawded |]
     [[t|Fragment '[IotaNumber] '[]|]]
 )

$( mkIotaFragExpr
     "Listen"
     [pattern| EAST aqqqqqwdeddw |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFragExpr
     "Wander"
     [pattern| EAST eqwawqwaqww |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFragExpr
     "FamiliarsReflection"
     [pattern| EAST daqweewqaeaqweewqaqwwww |]
     [[t|Fragment '[] '[IotaEntity]|]]
 )

$( mkIotaFragExpr
     "Link"
     [pattern| EAST eaqaaeqqqqqaweaqaaw |]
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "LinkOthers"
     [pattern| EAST eqqqqqawqeeeeedww |]
     [[t|Fragment '[IotaEntity, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "Unlink"
     [pattern| WEST qdeddqeeeeedwqdeddw |]
     [[t|Fragment '[IotaNumber] '[]|]]
 )

$( mkIotaFragExpr
     "UnlinkOthers"
     [pattern| WEST qeeeeedweqqqqqaww |]
     [[t|Fragment '[IotaEntity, IotaEntity] '[]|]]
 )

$( mkIotaFragExpr
     "PhonebookPurification"
     [pattern| EAST eqqqqqaww |]
     [[t|Fragment '[IotaNumber] '[IotaEntity]|]]
 )

$( mkIotaFragExpr
     "RecognitionPurification"
     [pattern| SOUTH_WEST aeqqqqqawwd |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "PopularityReflection"
     [pattern| WEST qeeeeedww |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "SendIota"
     [pattern| NORTH_WEST qqqqqwdeddw |]
     [[t|Fragment '[IotaAny, IotaNumber] '[]|]]
 )

$( mkIotaFragExpr
     "RecitationReflection"
     [pattern| NORTH_EAST weeeeew |]
     [[t|Fragment '[] '[IotaAny]|]]
 )

$( mkIotaFragExpr
     "PostmastersReflection"
     [pattern| SOUTH_EAST aweeeeewaa |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "UncloggingGambit"
     [pattern| SOUTH_EAST aweeeeewa |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFragExpr
     "OpenTransmit"
     [pattern| WEST qwdedwq |]
     [[t|Fragment '[IotaNumber] '[]|]]
 )

$( mkIotaFragExpr
     "CloseTransmit"
     [pattern| EAST ewaqawe |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFragExpr
     "GatesOpening"
     [pattern| WEST qaqeede |]
     [[t|Fragment '[IotaEntity, IotaGate] '[]|]]
 )

$( mkIotaFragExpr
     "GatesDismissal"
     [pattern| EAST edeqqaq |]
     [[t|Fragment '[IotaEntity, IotaGate] '[]|]]
 )

$( mkIotaFragExpr
     "MarkedDistillation"
     [pattern| EAST edwwdeeede |]
     [[t|Fragment '[IotaEntity, IotaGate] '[IotaBoolean]|]]
 )

$( mkIotaFragExpr
     "GatesClosing"
     [pattern| WEST qqqwwqqqwqqawdedw |]
     [ [t|Fragment '[IotaGate] '[]|]
     , [t|Fragment '[IotaVector, IotaGate] '[]|]
     ]
 )

$( mkIotaFragExpr
     "BindStorage"
     [pattern| NORTH_WEST qaqwqaqwqaq |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFragExpr
     "BindStorageTemporary"
     [pattern| NORTH_EAST edewedewede |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFragExpr
     "StocktakeReflection"
     [pattern| NORTH_EAST dwqqqqqwddww |]
     [[t|Fragment '[] '[IotaList IotaItemType]|]]
 )

$( mkIotaFragExpr
     "StocktakePurification"
     [pattern| SOUTH_EAST aweeeeewaaww |]
     [ [t|Fragment '[IotaMote] '[IotaList IotaMote]|]
     , [t|Fragment '[IotaItemType] '[IotaList IotaMote]|]
     ]
 )

$( mkIotaFragExpr
     "CapacityReflection"
     [pattern| SOUTH_EAST awedqdewa |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "ContainmentDistillation"
     [pattern| NORTH_EAST dwqaeaqwd |]
     [ [t|Fragment '[IotaItemType, IotaVector] '[IotaBoolean]|]
     , [t|Fragment '[IotaMote, IotaVector] '[IotaBoolean]|]
     ]
 )

$( mkIotaFragExpr
     "MediafyItem"
     [pattern| WEST eaqa |]
     [ [t|Fragment '[IotaEntity] '[IotaMote]|]
     , [t|Fragment '[IotaMote, IotaEntity] '[IotaMote]|]
     ]
 )

$( mkIotaFragExpr
     "ReturnItem"
     [pattern| EAST qded |]
     [ [t|Fragment '[IotaVector, IotaMote] '[]|]
     , [t|Fragment '[IotaNumber, IotaVector, IotaMote] '[]|]
     ]
 )

$( mkIotaFragExpr
     "StackingDistillationII"
     [pattern| SOUTH_WEST dedqeaqa |]
     [ [t|Fragment '[IotaMote, IotaMote] '[IotaBoolean]|]
     , [t|Fragment '[IotaItemType, IotaMote] '[IotaBoolean]|]
     ]
 )

$( mkIotaFragExpr
     "SplittingGambit"
     [pattern| EAST eaqaaw |]
     [[t|Fragment '[IotaNumber, IotaMote] '[IotaMote, IotaMote]|]]
 )

$( mkIotaFragExpr
     "DepotPurification"
     [pattern| SOUTH_WEST qqqqqaw |]
     [[t|Fragment '[IotaMote] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "DepotGambit"
     [pattern| SOUTH_EAST eeeeedw |]
     [[t|Fragment '[IotaVector, IotaMote] '[IotaMote]|]]
 )

$( mkIotaFragExpr
     "Craft"
     [pattern| SOUTH_EAST wwawdedwawdewwdwaqawdwwedwawdedwaww |]
     [ [t|Fragment '[IotaMote] '[IotaList IotaMote]|]
     , [t|Fragment '[IotaList IotaMote] '[IotaList IotaMote]|]
     , [t|Fragment '[IotaList (IotaList IotaMote)] '[IotaList IotaMote]|]
     ]
 )

$( mkIotaFragExpr
     "PreviewCraft"
     [pattern| NORTH_EAST wwdwaqawdwaqwwawdedwawwqawdwaqawdww |]
     [ [t|Fragment '[IotaItemType] '[IotaList IotaItemType]|]
     , [t|Fragment '[IotaList IotaItemType] '[IotaList IotaItemType]|]
     , [t|Fragment '[IotaList (IotaList IotaItemType)] '[IotaList IotaItemType]|]
     ]
 )

$( mkIotaFragExpr
     "SeniorityPurification"
     [pattern| NORTH_WEST qqwdedwqqaww |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

-- not typing this :p
$( mkIotaFragExpr
     "OfferingPurification"
     [pattern| SOUTH_EAST awdedwaawwqded |]
     [[t|Fragment '[IotaEntity] '[IotaAny]|]]
 )

$( mkIotaFragExpr
     "Trade"
     [pattern| NORTH_WEST awdedwaeqded |]
     [ [t|Fragment '[IotaNumber, IotaList IotaMote, IotaEntity] '[IotaItemType]|]
     , [t|Fragment '[IotaList IotaMote, IotaEntity] '[IotaItemType]|]
     ]
 )

$( mkIotaFragExpr
     "UseItemOn"
     [pattern| EAST qqqwqqqqaa |]
     [ [t|Fragment '[IotaEntity, IotaMote] '[]|]
     , [t|Fragment '[IotaVector, IotaVector, IotaMote] '[]|]
     ]
 )

$( mkGreatIotaFragExpr
     "ConsumeWisp"
     "Consume Wisp"
     [pattern| NORTH_WEST wawqwawwwewwwewwwawqwawwwewwwewdeaweewaqaweewaawwww |]
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkGreatIotaFragExpr
     "BindWisp"
     "Bind Wisp"
     [pattern| SOUTH_WEST aqweewqaeaqweewqaqwww |]
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkGreatIotaFragExpr
     "Accelerate"
     "Accelerate"
     [pattern| SOUTH_EAST wwwdwdwwwawqqeqwqqwqeqwqq |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkGreatIotaFragExpr
     "GatesReflection"
     "Gate's Reflection"
     [pattern| WEST qwqwqwqwqwqqeaeaeaeaeae |]
     [ [t|Fragment '[IotaNull] '[IotaGate]|]
     , [t|Fragment '[IotaVector] '[IotaGate]|]
     , [t|Fragment '[IotaEntity, IotaVector] '[IotaGate]|]
     ]
 )

$( mkIotaFragExpr
     "PhaseBlock"
     [pattern| WEST daqqqa |]
     [[t|Fragment '[IotaNumber, IotaVector] '[]|]]
 )
