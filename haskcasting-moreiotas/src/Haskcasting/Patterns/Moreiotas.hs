{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Patterns.Moreiotas where

import Haskcasting.Fragment (Fragment)
import Haskcasting.Iota (IotaAny, IotaBoolean, IotaEntity, IotaList, IotaNull, IotaNumber, IotaPattern, IotaVector)
import Haskcasting.Iota.Moreiotas (IotaEntityType, IotaIotaType, IotaItemStack, IotaItemType, IotaMatrix, IotaString)
import Haskcasting.TH (mkIotaFrag, pattern)

$( mkIotaFrag
     "BlankReflection"
     [pattern| SOUTH_EAST awdwa |]
     [[t|Fragment '[] '[IotaString]|]]
 )

$( mkIotaFrag
     "SpacingReflection"
     [pattern| SOUTH_EAST awdwaaww |]
     [[t|Fragment '[] '[IotaString]|]]
 )

$( mkIotaFrag
     "CommaReflection"
     [pattern| EAST qa |]
     [[t|Fragment '[] '[IotaString]|]]
 )

$( mkIotaFrag
     "BreakingReflection"
     [pattern| EAST waawaw |]
     [[t|Fragment '[] '[IotaString]|]]
 )

$( mkIotaFrag
     "ReadersPurification"
     [pattern| EAST awqwawqe |]
     [[t|Fragment '[IotaVector] '[IotaAny]|]]
 )

$( mkIotaFrag
     "Write"
     [pattern| WEST dwewdweq |]
     [ [t|Fragment '[IotaString, IotaVector] '[]|]
     , [t|Fragment '[IotaList IotaString, IotaVector] '[]|]
     ]
 )

$( mkIotaFrag
     "WhisperReflection"
     [pattern| EAST waqa |]
     [[t|Fragment '[] '[IotaString]|]]
 )

$( mkIotaFrag
     "ListenersReflection"
     [pattern| EAST wded |]
     [[t|Fragment '[] '[IotaString]|]]
 )

$( mkIotaFrag
     "SiftersReflection"
     [pattern| NORTH_EAST ewded |]
     [[t|Fragment '[] '[IotaString]|]]
 )

$( mkIotaFrag
     "SiftersGambit"
     [pattern| SOUTH_EAST qwaqa |]
     [[t|Fragment '[IotaString] '[]|]]
 )

$( mkIotaFrag
     "ScrivenersPurification"
     [pattern| EAST wawqwawaw |]
     [[t|Fragment '[IotaAny] '[IotaString]|]]
 )

$( mkIotaFrag
     "PatternmastersPurification"
     [pattern| NORTH_WEST wdwewdwdw |]
     [[t|Fragment '[IotaPattern] '[IotaString]|]]
 )

$( mkIotaFrag
     "MonikerPurification"
     [pattern| SOUTH_EAST deqqeddqwqqqwq |]
     [[t|Fragment '[IotaEntity] '[IotaString]|]]
 )

$( mkIotaFrag
     "Name"
     [pattern| SOUTH_WEST aqeeqaaeweeewe |]
     [[t|Fragment '[IotaEntity, IotaString] '[]|]]
 )

$( mkIotaFrag
     "SeparationDistillation"
     [pattern| EAST aqwaqa |]
     [[t|Fragment '[IotaString, IotaString] '[IotaList IotaString]|]]
 )

$( mkIotaFrag
     "InputPurification"
     [pattern| EAST aqwaq |]
     [[t|Fragment '[IotaString] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "CaseDistillation"
     [pattern| WEST dwwdwwdwdd |]
     [ [t|Fragment '[IotaBoolean, IotaString] '[IotaString]|]
     , [t|Fragment '[IotaNull, IotaString] '[IotaString]|]
     ]
 )

$( mkIotaFrag
     "MultiplicativeDistillationII"
     [pattern| SOUTH_EAST waqawawwaeaww |]
     [[t|Fragment '[IotaMatrix, IotaMatrix] '[IotaMatrix]|]]
 )

$( mkIotaFrag
     "DivisionDistillationII"
     [pattern| NORTH_EAST wdedwdwwdqdww |]
     [[t|Fragment '[IotaMatrix, IotaMatrix] '[IotaMatrix]|]]
 )

$( mkIotaFrag
     "PowerDistillationII"
     [pattern| NORTH_EAST wedewqawwawqwa |]
     [[t|Fragment '[IotaMatrix, IotaMatrix] '[IotaMatrix]|]]
 )

$( mkIotaFrag
     "TransformationPurification"
     [pattern| SOUTH_WEST awwaeawwaadwa |]
     [ [t|Fragment '[IotaNumber] '[IotaMatrix]|]
     , [t|Fragment '[IotaVector] '[IotaMatrix]|]
     , [t|Fragment '[IotaList IotaNumber] '[IotaMatrix]|]
     , [t|Fragment '[IotaList IotaVector] '[IotaMatrix]|]
     , [t|Fragment '[IotaList (IotaList IotaNumber)] '[IotaMatrix]|]
     ]
 )

$( mkIotaFrag
     "RestorationPurification"
     [pattern| SOUTH_EAST dwwdqdwwddawd |]
     [[t|Fragment '[IotaMatrix] '[IotaAny]|]]
 )

$( mkIotaFrag
     "RestorationPurificationII"
     [pattern| SOUTH_EAST dwwdqdwwdwdwa |]
     [[t|Fragment '[IotaMatrix] '[IotaList (IotaList IotaNumber)]|]]
 )

$( mkIotaFrag
     "IdentityPurification"
     [pattern| SOUTH_WEST awwaeawwaqw |]
     [[t|Fragment '[IotaNumber] '[IotaMatrix]|]]
 )

$( mkIotaFrag
     "ZeroDistillation"
     [pattern| SOUTH_WEST awwaeawwa |]
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaMatrix]|]]
 )

$( mkIotaFrag
     "RotationDistillation"
     [pattern| SOUTH_WEST awwaeawwawawddw |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaMatrix]|]]
 )

$( mkIotaFrag
     "InversePurification"
     [pattern| WEST wwdqdwwdqaq |]
     [[t|Fragment '[IotaMatrix] '[IotaMatrix]|]]
 )

$( mkIotaFrag
     "DeterminantPurification"
     [pattern| WEST aeawwaeawaw |]
     [[t|Fragment '[IotaMatrix] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "TowerDistillation"
     [pattern| SOUTH_WEST awwaeawwawawdedwa |]
     [[t|Fragment '[IotaMatrix, IotaMatrix] '[IotaMatrix]|]]
 )

$( mkIotaFrag
     "SprawlingDistillation"
     [pattern| SOUTH_EAST dwwdqdwwdwdwaqawd |]
     [[t|Fragment '[IotaMatrix, IotaMatrix] '[IotaMatrix]|]]
 )

$( mkIotaFrag
     "TopplingGambit"
     [pattern| SOUTH_EAST awdedwawawwaeawwa |]
     [[t|Fragment '[IotaNumber, IotaMatrix] '[IotaMatrix, IotaMatrix]|]]
 )

$( mkIotaFrag
     "MitosisGambit"
     [pattern| SOUTH_WEST dwaqawdwdwwdqdwwd |]
     [[t|Fragment '[IotaNumber, IotaMatrix] '[IotaMatrix, IotaMatrix]|]]
 )

$( mkIotaFrag
     "SortersPurification"
     [pattern| EAST qaqqaea |]
     [ [t|Fragment '[IotaItemStack] '[IotaItemType]|]
     , [t|Fragment '[IotaEntity] '[IotaItemType]|]
     , [t|Fragment '[IotaVector] '[IotaItemType]|]
     ]
 )

$( mkIotaFrag
     "PhysiciansPurification"
     [pattern| SOUTH_WEST qawde |]
     [[t|Fragment '[IotaEntity] '[IotaEntityType]|]]
 )

$( mkIotaFrag
     "ClassifiersPurification"
     [pattern| SOUTH_WEST awd |]
     [[t|Fragment '[IotaAny] '[IotaIotaType]|]] -- Assuming iotatype renders as str
 )

$( mkIotaFrag
     "SortersReflection"
     [pattern| SOUTH_WEST edeedqd |]
     [[t|Fragment '[] '[IotaItemType]|]]
 )

$( mkIotaFrag
     "EntityDistillationType"
     [pattern| NORTH_EAST dadqqqqqdad |]
     [[t|Fragment '[IotaVector, IotaEntityType] '[IotaEntity]|]]
 )

$( mkIotaFrag
     "ZoneExaltationType"
     [pattern| SOUTH_EAST waweeeeewaw |]
     [[t|Fragment '[IotaNumber, IotaVector, IotaEntityType] '[IotaList IotaEntity]|]]
 )

$( mkIotaFrag
     "ZoneExaltationNotType"
     [pattern| NORTH_EAST wdwqqqqqwdw |]
     [[t|Fragment '[IotaNumber, IotaVector, IotaEntityType] '[IotaList IotaEntity]|]]
 )

$( mkIotaFrag
     "DuelistsPurification"
     [pattern| EAST adeq |]
     [[t|Fragment '[IotaEntity] '[IotaItemStack]|]]
 )

$( mkIotaFrag
     "ShieldbearersPurification"
     [pattern| EAST qeda |]
     [[t|Fragment '[IotaEntity] '[IotaItemStack]|]]
 )

$( mkIotaFrag
     "HoardersDistillation"
     [pattern| NORTH_EAST aqwed |]
     [[t|Fragment '[IotaVector, IotaVector] '[IotaList IotaItemStack]|]]
 )

$( mkIotaFrag
     "CollectorsDistillation"
     [pattern| NORTH_EAST dewqa |]
     [[t|Fragment '[IotaVector, IotaVector] '[IotaList IotaItemStack]|]]
 )

$( mkIotaFrag
     "TreasurersDistillation"
     [pattern| EAST adeeedew |]
     [[t|Fragment '[IotaNumber, IotaItemStack] '[IotaItemStack]|]]
 )
