{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Patterns.Hexal where

import Haskcasting.Fragment (Fragment)
import Haskcasting.Iota (
  IotaAny,
  IotaAnyList,
  IotaBoolean,
  IotaEntity,
  IotaExec,
  IotaGreatPattern (IotaGreatPattern),
  IotaList,
  IotaNull,
  IotaNumber,
  IotaPattern,
  IotaVector,
 )
import Haskcasting.Iota.Hexal (IotaGate, IotaMote)
import Haskcasting.Iota.Moreiotas (IotaItemType)
import Haskcasting.TH (mkGreatIotaFrag, mkIotaFrag, pattern)

$( mkIotaFrag
     "TimekeepersReflection"
     [pattern| NORTH_WEST ddwaa |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "DiversPurification"
     [pattern| NORTH_WEST aqawdwaqawd |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "NursesPurification"
     [pattern| NORTH_WEST aqwawqa |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "SquiresPurification"
     [pattern| NORTH_WEST wqqqqw |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "BoxersPurification"
     [pattern| EAST aeqqqqea |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "LamplightersPurification"
     [pattern| NORTH_EAST qedqde |]
     [[t|Fragment '[IotaVector] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "FactorialPurification"
     [pattern| SOUTH_EAST wawdedwaw |]
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "RunningSumPurification"
     [pattern| WEST aea |]
     [[t|Fragment '[IotaList IotaNumber] '[IotaList IotaNumber]|]]
 )

$( mkIotaFrag
     "RunningProductPurification"
     [pattern| NORTH_EAST qaawaaq |]
     [[t|Fragment '[IotaList IotaNumber] '[IotaList IotaNumber]|]]
 )

$( mkIotaFrag
     "MnemosynesGambit"
     [pattern| NORTH_EAST eweeewedqdeddw |]
     [[t|Fragment '[IotaPattern, IotaVector] '[]|]]
 )

$( mkIotaFrag
     "ElysiumsGambit"
     [pattern| SOUTH_EAST qwqqqwqaeaqaaw |]
     [[t|Fragment '[IotaPattern, IotaVector] '[]|]]
 )

$( mkIotaFrag
     "LethesGambit"
     [pattern| SOUTH_EAST qwqqqwqaww |]
     [[t|Fragment '[IotaPattern] '[]|]]
 )

$( mkIotaFrag
     "BrighsGambit"
     [pattern| SOUTH_WEST eweeewedww |]
     [[t|Fragment '[IotaPattern] '[]|]]
 )

$( mkIotaFrag
     "Smelt"
     [pattern| EAST wqqqwqqadad |]
     [ [t|Fragment '[IotaVector] '[]|]
     , [t|Fragment '[IotaEntity] '[]|]
     , [t|Fragment '[IotaMote] '[]|]
     ]
 )

$( mkIotaFrag
     "Freeze"
     [pattern| WEST weeeweedada |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "FallingBlock"
     [pattern| EAST wqwawqwqwqwqwqw |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "PlaceBlockII"
     [pattern| WEST eeeeedeeeee |]
     [ [t|Fragment '[IotaVector, IotaItemType] '[]|]
     , [t|Fragment '[IotaVector, IotaMote] '[]|]
     ]
 )

$( mkIotaFrag
     "Particles"
     [pattern| NORTH_EAST eqqqqa |]
     [ [t|Fragment '[IotaVector] '[]|]
     , [t|Fragment '[IotaList IotaVector] '[]|]
     ]
 )

$( mkIotaFrag
     "SummonProjectileWisp"
     [pattern| NORTH_WEST aqaeqeeeee |]
     [[t|forall s. Fragment '[IotaNumber, IotaVector, IotaVector, IotaExec '[IotaEntity] s] '[]|]]
 )

$( mkIotaFrag
     "SummonCyclicWisp"
     [pattern| NORTH_WEST aqaweewaqawee |]
     [[t|forall s. Fragment '[IotaNumber, IotaVector, IotaExec '[IotaEntity] s] '[]|]]
 )

$( mkIotaFrag
     "IdentityReflection"
     [pattern| NORTH_EAST dedwqqwdedwqqaw |]
     [[t|Fragment '[] '[IotaEntity]|]]
 )

$( mkIotaFrag
     "ReservoirReflection"
     [pattern| NORTH_WEST aqaweewaqaweedw |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "ManagersPurification"
     [pattern| SOUTH_EAST aweewaqaweewaawww |]
     [[t|Fragment '[IotaEntity] '[IotaAnyList]|]]
 )

$( mkIotaFrag
     "AllegianceDistillation"
     [pattern| SOUTH_WEST dwqqwdedwqqwddwww |]
     [[t|Fragment '[IotaEntity, IotaEntity] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "PathfindersGambit"
     [pattern| WEST awqwawqaw |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "PathfindersReflection"
     [pattern| EAST ewdwewdew |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFrag
     "Haste"
     [pattern| WEST aeawqqqae |]
     [[t|Fragment '[IotaNumber] '[]|]]
 )

$( mkIotaFrag
     "SpeedometersReflection"
     [pattern| EAST eeewdqdee |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "AllowTransfer"
     [pattern| NORTH_WEST qqqqqewwqeeeee |]
     [[t|Fragment '[IotaNumber] '[]|]]
 )

$( mkIotaFrag
     "DisallowTransfer"
     [pattern| NORTH_WEST qqqqqeqdeddweqqqqq |]
     [[t|Fragment '[IotaNumber] '[]|]]
 )

$( mkIotaFrag
     "AllowTransferOthers"
     [pattern| SOUTH_WEST eeeeeqwweqqqqq |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "DisallowTransferOthers"
     [pattern| SOUTH_WEST eeeeeqeaqaawqeeeee |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "EntityPurificationWisp"
     [pattern| SOUTH_EAST qqwdedwqqdaqaaww |]
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkIotaFrag
     "ZoneDistillationWisp"
     [pattern| SOUTH_EAST qqwdedwqqwdeddww |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFrag
     "ZoneDistillationNonWisp"
     [pattern| NORTH_EAST eewaqaweewaqaaww |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFrag
     "DelayWisp"
     [pattern| NORTH_WEST aqawded |]
     [[t|Fragment '[IotaNumber] '[]|]]
 )

$( mkIotaFrag
     "Listen"
     [pattern| EAST aqqqqqwdeddw |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "Wander"
     [pattern| EAST eqwawqwaqww |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "FamiliarsReflection"
     [pattern| EAST daqweewqaeaqweewqaqwwww |]
     [[t|Fragment '[] '[IotaEntity]|]]
 )

$( mkIotaFrag
     "Link"
     [pattern| EAST eaqaaeqqqqqaweaqaaw |]
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "LinkOthers"
     [pattern| EAST eqqqqqawqeeeeedww |]
     [[t|Fragment '[IotaEntity, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "Unlink"
     [pattern| WEST qdeddqeeeeedwqdeddw |]
     [[t|Fragment '[IotaNumber] '[]|]]
 )

$( mkIotaFrag
     "UnlinkOthers"
     [pattern| WEST qeeeeedweqqqqqaww |]
     [[t|Fragment '[IotaEntity, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "PhonebookPurification"
     [pattern| EAST eqqqqqaww |]
     [[t|Fragment '[IotaNumber] '[IotaEntity]|]]
 )

$( mkIotaFrag
     "RecognitionPurification"
     [pattern| SOUTH_WEST aeqqqqqawwd |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "PopularityReflection"
     [pattern| WEST qeeeeedww |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "SendIota"
     [pattern| NORTH_WEST qqqqqwdeddw |]
     [[t|Fragment '[IotaAny, IotaNumber] '[]|]]
 )

$( mkIotaFrag
     "RecitationReflection"
     [pattern| NORTH_EAST weeeeew |]
     [[t|Fragment '[] '[IotaAny]|]]
 )

$( mkIotaFrag
     "PostmastersReflection"
     [pattern| SOUTH_EAST aweeeeewaa |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "UncloggingGambit"
     [pattern| SOUTH_EAST aweeeeewa |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "OpenTransmit"
     [pattern| WEST qwdedwq |]
     [[t|Fragment '[IotaNumber] '[]|]]
 )

$( mkIotaFrag
     "CloseTransmit"
     [pattern| EAST ewaqawe |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "GatesOpening"
     [pattern| WEST qaqeede |]
     [[t|Fragment '[IotaEntity, IotaGate] '[]|]]
 )

$( mkIotaFrag
     "GatesDismissal"
     [pattern| EAST edeqqaq |]
     [[t|Fragment '[IotaEntity, IotaGate] '[]|]]
 )

$( mkIotaFrag
     "MarkedDistillation"
     [pattern| EAST edwwdeeede |]
     [[t|Fragment '[IotaEntity, IotaGate] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "GatesClosing"
     [pattern| WEST qqqwwqqqwqqawdedw |]
     [ [t|Fragment '[IotaGate] '[]|]
     , [t|Fragment '[IotaVector, IotaGate] '[]|]
     ]
 )

$( mkIotaFrag
     "BindStorage"
     [pattern| NORTH_WEST qaqwqaqwqaq |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "BindStorageTemporary"
     [pattern| NORTH_EAST edewedewede |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "StocktakeReflection"
     [pattern| NORTH_EAST dwqqqqqwddww |]
     [[t|Fragment '[] '[IotaList IotaItemType]|]]
 )

$( mkIotaFrag
     "StocktakePurification"
     [pattern| SOUTH_EAST aweeeeewaaww |]
     [ [t|Fragment '[IotaMote] '[IotaList IotaMote]|]
     , [t|Fragment '[IotaItemType] '[IotaList IotaMote]|]
     ]
 )

$( mkIotaFrag
     "CapacityReflection"
     [pattern| SOUTH_EAST awedqdewa |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "ContainmentDistillation"
     [pattern| NORTH_EAST dwqaeaqwd |]
     [ [t|Fragment '[IotaItemType, IotaVector] '[IotaBoolean]|]
     , [t|Fragment '[IotaMote, IotaVector] '[IotaBoolean]|]
     ]
 )

$( mkIotaFrag
     "MediafyItem"
     [pattern| WEST eaqa |]
     [ [t|Fragment '[IotaItemType] '[IotaMote]|]
     , [t|Fragment '[IotaMote, IotaItemType] '[IotaMote]|]
     ]
 )

$( mkIotaFrag
     "ReturnItem"
     [pattern| EAST qded |]
     [ [t|Fragment '[IotaVector, IotaMote] '[]|]
     , [t|Fragment '[IotaNumber, IotaVector, IotaMote] '[]|]
     ]
 )

$( mkIotaFrag
     "StackingDistillationII"
     [pattern| SOUTH_WEST dedqeaqa |]
     [ [t|Fragment '[IotaMote, IotaMote] '[IotaBoolean]|]
     , [t|Fragment '[IotaItemType, IotaMote] '[IotaBoolean]|]
     ]
 )

$( mkIotaFrag
     "SplittingGambit"
     [pattern| EAST eaqaaw |]
     [[t|Fragment '[IotaNumber, IotaMote] '[IotaMote, IotaMote]|]]
 )

$( mkIotaFrag
     "DepotPurification"
     [pattern| SOUTH_WEST qqqqqaw |]
     [[t|Fragment '[IotaMote] '[IotaVector]|]]
 )

$( mkIotaFrag
     "DepotGambit"
     [pattern| SOUTH_EAST eeeeedw |]
     [[t|Fragment '[IotaVector, IotaMote] '[IotaMote]|]]
 )

$( mkIotaFrag
     "Craft"
     [pattern| SOUTH_EAST wwawdedwawdewwdwaqawdwwedwawdedwaww |]
     [ [t|Fragment '[IotaMote] '[IotaList IotaMote]|]
     , [t|Fragment '[IotaList IotaMote] '[IotaList IotaMote]|]
     , [t|Fragment '[IotaList (IotaList IotaMote)] '[IotaList IotaMote]|]
     ]
 )

$( mkIotaFrag
     "PreviewCraft"
     [pattern| NORTH_EAST wwdwaqawdwaqwwawdedwawwqawdwaqawdww |]
     [ [t|Fragment '[IotaItemType] '[IotaList IotaItemType]|]
     , [t|Fragment '[IotaList IotaItemType] '[IotaList IotaItemType]|]
     , [t|Fragment '[IotaList (IotaList IotaItemType)] '[IotaList IotaItemType]|]
     ]
 )

$( mkIotaFrag
     "SeniorityPurification"
     [pattern| NORTH_WEST qqwdedwqqaww |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

-- not typing this :p
$( mkIotaFrag
     "OfferingPurification"
     [pattern| SOUTH_EAST awdedwaawwqded |]
     [[t|Fragment '[IotaEntity] '[IotaAny]|]]
 )

$( mkIotaFrag
     "Trade"
     [pattern| NORTH_WEST awdedwaeqded |]
     [ [t|Fragment '[IotaNumber, IotaList IotaMote, IotaEntity] '[IotaItemType]|]
     , [t|Fragment '[IotaList IotaMote, IotaEntity] '[IotaItemType]|]
     ]
 )

$( mkIotaFrag
     "UseItemOn"
     [pattern| EAST qqqwqqqqaa |]
     [ [t|Fragment '[IotaEntity, IotaMote] '[]|]
     , [t|Fragment '[IotaVector, IotaVector, IotaMote] '[]|]
     ]
 )

$( mkGreatIotaFrag
     "ConsumeWisp"
     ( IotaGreatPattern
         "Consume Wisp"
         [pattern| NORTH_WEST wawqwawwwewwwewwwawqwawwwewwwewdeaweewaqaweewaawwww |]
     )
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkGreatIotaFrag
     "BindWisp"
     ( IotaGreatPattern
         "Bind Wisp"
         [pattern| SOUTH_WEST aqweewqaeaqweewqaqwww |]
     )
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkGreatIotaFrag
     "Accelerate"
     ( IotaGreatPattern
         "Accelerate"
         [pattern| SOUTH_EAST wwwdwdwwwawqqeqwqqwqeqwqq |]
     )
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkGreatIotaFrag
     "GatesReflection"
     ( IotaGreatPattern
         "Gate's Reflection"
         [pattern| WEST qwqwqwqwqwqqeaeaeaeaeae |]
     )
     [ [t|Fragment '[IotaNull] '[IotaGate]|]
     , [t|Fragment '[IotaVector] '[IotaGate]|]
     , [t|Fragment '[IotaEntity, IotaVector] '[IotaGate]|]
     ]
 )

$( mkIotaFrag
     "PhaseBlock"
     [pattern| WEST daqqqa |]
     [[t|Fragment '[IotaNumber, IotaVector] '[]|]]
 )
