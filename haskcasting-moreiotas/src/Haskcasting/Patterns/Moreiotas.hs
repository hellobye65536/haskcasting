{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Patterns.Moreiotas where

import Haskcasting.ExprLang.TH (mkIotaFragExpr)
import Haskcasting.Fragment (Fragment)
import Haskcasting.Iota (IotaAny, IotaBoolean, IotaEntity, IotaList, IotaNull, IotaNumber, IotaPattern, IotaVector)
import Haskcasting.Iota.Moreiotas (IotaEntityType, IotaIotaType, IotaItemStack, IotaItemType, IotaMatrix, IotaString)
import Haskcasting.Pattern (pattern)

$( mkIotaFragExpr
     "BlankReflection"
     [pattern| SOUTH_EAST awdwa |]
     [[t|Fragment '[] '[IotaString]|]]
 )

$( mkIotaFragExpr
     "SpacingReflection"
     [pattern| SOUTH_EAST awdwaaww |]
     [[t|Fragment '[] '[IotaString]|]]
 )

$( mkIotaFragExpr
     "CommaReflection"
     [pattern| EAST qa |]
     [[t|Fragment '[] '[IotaString]|]]
 )

$( mkIotaFragExpr
     "BreakingReflection"
     [pattern| EAST waawaw |]
     [[t|Fragment '[] '[IotaString]|]]
 )

$( mkIotaFragExpr
     "ReadersPurification"
     [pattern| EAST awqwawqe |]
     [[t|Fragment '[IotaVector] '[IotaAny]|]]
 )

$( mkIotaFragExpr
     "Write"
     [pattern| WEST dwewdweq |]
     [ [t|Fragment '[IotaString, IotaVector] '[]|]
     , [t|Fragment '[IotaList IotaString, IotaVector] '[]|]
     ]
 )

$( mkIotaFragExpr
     "WhisperReflection"
     [pattern| EAST waqa |]
     [[t|Fragment '[] '[IotaString]|]]
 )

$( mkIotaFragExpr
     "ListenersReflection"
     [pattern| EAST wded |]
     [[t|Fragment '[] '[IotaString]|]]
 )

$( mkIotaFragExpr
     "SiftersReflection"
     [pattern| NORTH_EAST ewded |]
     [[t|Fragment '[] '[IotaString]|]]
 )

$( mkIotaFragExpr
     "SiftersGambit"
     [pattern| SOUTH_EAST qwaqa |]
     [[t|Fragment '[IotaString] '[]|]]
 )

$( mkIotaFragExpr
     "ScrivenersPurification"
     [pattern| EAST wawqwawaw |]
     [[t|Fragment '[IotaAny] '[IotaString]|]]
 )

$( mkIotaFragExpr
     "PatternmastersPurification"
     [pattern| NORTH_WEST wdwewdwdw |]
     [[t|Fragment '[IotaPattern] '[IotaString]|]]
 )

$( mkIotaFragExpr
     "MonikerPurification"
     [pattern| SOUTH_EAST deqqeddqwqqqwq |]
     [[t|Fragment '[IotaEntity] '[IotaString]|]]
 )

$( mkIotaFragExpr
     "Name"
     [pattern| SOUTH_WEST aqeeqaaeweeewe |]
     [[t|Fragment '[IotaEntity, IotaString] '[]|]]
 )

$( mkIotaFragExpr
     "SeparationDistillation"
     [pattern| EAST aqwaqa |]
     [[t|Fragment '[IotaString, IotaString] '[IotaList IotaString]|]]
 )

$( mkIotaFragExpr
     "InputPurification"
     [pattern| EAST aqwaq |]
     [[t|Fragment '[IotaString] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "CaseDistillation"
     [pattern| WEST dwwdwwdwdd |]
     [ [t|Fragment '[IotaBoolean, IotaString] '[IotaString]|]
     , [t|Fragment '[IotaNull, IotaString] '[IotaString]|]
     ]
 )

$( mkIotaFragExpr
     "MultiplicativeDistillationII"
     [pattern| SOUTH_EAST waqawawwaeaww |]
     [[t|Fragment '[IotaMatrix, IotaMatrix] '[IotaMatrix]|]]
 )

$( mkIotaFragExpr
     "DivisionDistillationII"
     [pattern| NORTH_EAST wdedwdwwdqdww |]
     [[t|Fragment '[IotaMatrix, IotaMatrix] '[IotaMatrix]|]]
 )

$( mkIotaFragExpr
     "PowerDistillationII"
     [pattern| NORTH_EAST wedewqawwawqwa |]
     [[t|Fragment '[IotaMatrix, IotaMatrix] '[IotaMatrix]|]]
 )

$( mkIotaFragExpr
     "TransformationPurification"
     [pattern| SOUTH_WEST awwaeawwaadwa |]
     [ [t|Fragment '[IotaNumber] '[IotaMatrix]|]
     , [t|Fragment '[IotaVector] '[IotaMatrix]|]
     , [t|Fragment '[IotaList IotaNumber] '[IotaMatrix]|]
     , [t|Fragment '[IotaList IotaVector] '[IotaMatrix]|]
     , [t|Fragment '[IotaList (IotaList IotaNumber)] '[IotaMatrix]|]
     ]
 )

$( mkIotaFragExpr
     "RestorationPurification"
     [pattern| SOUTH_EAST dwwdqdwwddawd |]
     [[t|Fragment '[IotaMatrix] '[IotaAny]|]]
 )

$( mkIotaFragExpr
     "RestorationPurificationII"
     [pattern| SOUTH_EAST dwwdqdwwdwdwa |]
     [[t|Fragment '[IotaMatrix] '[IotaList (IotaList IotaNumber)]|]]
 )

$( mkIotaFragExpr
     "IdentityPurification"
     [pattern| SOUTH_WEST awwaeawwaqw |]
     [[t|Fragment '[IotaNumber] '[IotaMatrix]|]]
 )

$( mkIotaFragExpr
     "ZeroDistillation"
     [pattern| SOUTH_WEST awwaeawwa |]
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaMatrix]|]]
 )

$( mkIotaFragExpr
     "RotationDistillation"
     [pattern| SOUTH_WEST awwaeawwawawddw |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaMatrix]|]]
 )

$( mkIotaFragExpr
     "InversePurification"
     [pattern| WEST wwdqdwwdqaq |]
     [[t|Fragment '[IotaMatrix] '[IotaMatrix]|]]
 )

$( mkIotaFragExpr
     "DeterminantPurification"
     [pattern| WEST aeawwaeawaw |]
     [[t|Fragment '[IotaMatrix] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "TowerDistillation"
     [pattern| SOUTH_WEST awwaeawwawawdedwa |]
     [[t|Fragment '[IotaMatrix, IotaMatrix] '[IotaMatrix]|]]
 )

$( mkIotaFragExpr
     "SprawlingDistillation"
     [pattern| SOUTH_EAST dwwdqdwwdwdwaqawd |]
     [[t|Fragment '[IotaMatrix, IotaMatrix] '[IotaMatrix]|]]
 )

$( mkIotaFragExpr
     "TopplingGambit"
     [pattern| SOUTH_EAST awdedwawawwaeawwa |]
     [[t|Fragment '[IotaNumber, IotaMatrix] '[IotaMatrix, IotaMatrix]|]]
 )

$( mkIotaFragExpr
     "MitosisGambit"
     [pattern| SOUTH_WEST dwaqawdwdwwdqdwwd |]
     [[t|Fragment '[IotaNumber, IotaMatrix] '[IotaMatrix, IotaMatrix]|]]
 )

$( mkIotaFragExpr
     "SortersPurification"
     [pattern| EAST qaqqaea |]
     [ [t|Fragment '[IotaItemStack] '[IotaItemType]|]
     , [t|Fragment '[IotaEntity] '[IotaItemType]|]
     , [t|Fragment '[IotaVector] '[IotaItemType]|]
     ]
 )

$( mkIotaFragExpr
     "PhysiciansPurification"
     [pattern| SOUTH_WEST qawde |]
     [[t|Fragment '[IotaEntity] '[IotaEntityType]|]]
 )

$( mkIotaFragExpr
     "ClassifiersPurification"
     [pattern| SOUTH_WEST awd |]
     [[t|Fragment '[IotaAny] '[IotaIotaType]|]] -- Assuming iotatype renders as str
 )

$( mkIotaFragExpr
     "SortersReflection"
     [pattern| SOUTH_WEST edeedqd |]
     [[t|Fragment '[] '[IotaItemType]|]]
 )

$( mkIotaFragExpr
     "EntityDistillationType"
     [pattern| NORTH_EAST dadqqqqqdad |]
     [[t|Fragment '[IotaVector, IotaEntityType] '[IotaEntity]|]]
 )

$( mkIotaFragExpr
     "ZoneExaltationType"
     [pattern| SOUTH_EAST waweeeeewaw |]
     [[t|Fragment '[IotaNumber, IotaVector, IotaEntityType] '[IotaList IotaEntity]|]]
 )

$( mkIotaFragExpr
     "ZoneExaltationNotType"
     [pattern| NORTH_EAST wdwqqqqqwdw |]
     [[t|Fragment '[IotaNumber, IotaVector, IotaEntityType] '[IotaList IotaEntity]|]]
 )

$( mkIotaFragExpr
     "DuelistsPurification"
     [pattern| EAST adeq |]
     [[t|Fragment '[IotaEntity] '[IotaItemStack]|]]
 )

$( mkIotaFragExpr
     "ShieldbearersPurification"
     [pattern| EAST qeda |]
     [[t|Fragment '[IotaEntity] '[IotaItemStack]|]]
 )

$( mkIotaFragExpr
     "HoardersDistillation"
     [pattern| NORTH_EAST aqwed |]
     [[t|Fragment '[IotaVector, IotaVector] '[IotaList IotaItemStack]|]]
 )

$( mkIotaFragExpr
     "CollectorsDistillation"
     [pattern| NORTH_EAST dewqa |]
     [[t|Fragment '[IotaVector, IotaVector] '[IotaList IotaItemStack]|]]
 )

$( mkIotaFragExpr
     "TreasurersDistillation"
     [pattern| EAST adeeedew |]
     [[t|Fragment '[IotaNumber, IotaItemStack] '[IotaItemStack]|]]
 )
