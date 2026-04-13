{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskcasting.ExprLang.Hexcasting where

import Data.HList.HList (HAppendListR, HReverse)
import Data.Sequence qualified as Seq
import Haskcasting.ExprLang (Expr, HListLen)
import Haskcasting.ExprLang qualified as E
import Haskcasting.ExprLang.TH (mkExpr)
import Haskcasting.Fragment (Fragment)

import Haskcasting.Iota
import Haskcasting.Patterns.Hexcasting

$( mkExpr
     "MindsReflection"
     [[t|Fragment '[] '[IotaEntity]|]]
 )

$( mkExpr
     "CompassPurification"
     [[t|Fragment '[IotaEntity] '[IotaVector]|]]
 )

$( mkExpr
     "CompassPurificationII"
     [[t|Fragment '[IotaEntity] '[IotaVector]|]]
 )

$( mkExpr
     "AlidadesPurification"
     [[t|Fragment '[IotaEntity] '[IotaVector]|]]
 )

$( mkExpr
     "StadiometersPurification"
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkExpr
     "PacePurification"
     [[t|Fragment '[IotaEntity] '[IotaVector]|]]
 )

$( mkExpr
     "ArchersDistillation"
     [[t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]]
 )

$( mkExpr
     "ArchitectsDistillation"
     [[t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]]
 )

$( mkExpr
     "ScoutsDistillation"
     [[t|Fragment '[IotaVector, IotaVector] '[IotaEntity]|]]
 )

$( mkExpr
     "WaystoneReflection"
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkExpr
     "LodestoneReflection"
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkExpr
     "LesserFoldReflection"
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkExpr
     "GreaterFoldReflection"
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkExpr
     "JestersGambit"
     [[t|forall a b. Fragment '[a, b] '[b, a]|]]
 )

$( mkExpr
     "RotationGambit"
     [[t|forall a b c. Fragment '[a, b, c] '[c, a, b]|]]
 )

$( mkExpr
     "RotationGambitII"
     [[t|forall a b c. Fragment '[a, b, c] '[b, c, a]|]]
 )

$( mkExpr
     "GeminiDecomposition"
     [[t|forall a. Fragment '[a] '[a, a]|]]
 )

$( mkExpr
     "ProspectorsGambit"
     [[t|forall a b. Fragment '[a, b] '[b, a, b]|]]
 )

$( mkExpr
     "UndertakersGambit"
     [[t|forall a b. Fragment '[a, b] '[a, b, a]|]]
 )

$( mkExpr
     "DioscuriGambit"
     [[t|forall a b. Fragment '[a, b] '[a, b, a, b]|]]
 )

$( mkExpr
     "FlocksReflection"
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkExpr
     "AdditiveDistillation"
     [ [t|forall as bs asbs. HAppendListR as bs ~ asbs => Fragment '[IotaHList bs, IotaHList as] '[IotaHList asbs]|]
     , [t|forall a. Fragment '[IotaList a, IotaList a] '[IotaList a]|]
     , [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
 )

$( mkExpr
     "SubtractiveDistillation"
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
 )

$( mkExpr
     "MultiplicativeDistillation"
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaNumber]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
 )

$( mkExpr
     "DivisionDistillation"
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
 )

$( mkExpr
     "LengthPurification"
     [ [t|Fragment '[IotaBoolean] '[IotaNumber]|]
     , [t|forall a. Fragment '[IotaList a] '[IotaNumber]|]
     , [t|forall as. Fragment '[IotaHList as] '[IotaNumber]|]
     , [t|Fragment '[IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector] '[IotaNumber]|]
     ]
 )

$( mkExpr
     "PowerDistillation"
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
 )

$( mkExpr
     "FloorPurification"
     [ [t|Fragment '[IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector] '[IotaVector]|]
     ]
 )

$( mkExpr
     "CeilingPurification"
     [ [t|Fragment '[IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector] '[IotaVector]|]
     ]
 )

$( mkExpr
     "VectorExaltation"
     [[t|Fragment '[IotaNumber, IotaNumber, IotaNumber] '[IotaVector]|]]
 )

$( mkExpr
     "VectorDisintegration"
     [[t|Fragment '[IotaVector] '[IotaNumber, IotaNumber, IotaNumber]|]]
 )

$( mkExpr
     "AxialPurification"
     [ [t|Fragment '[IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector] '[IotaVector]|]
     ]
 )

$( mkExpr
     "ConjunctionDistillation"
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|forall a. Fragment '[IotaList a, IotaList a] '[IotaList a]|]
     , -- IotaHList
       [t|Fragment '[IotaBoolean, IotaBoolean] '[IotaBoolean]|]
     ]
 )

$( mkExpr
     "DisjunctionDistillation"
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|forall a. Fragment '[IotaList a, IotaList a] '[IotaList a]|]
     , -- IotaHList
       [t|Fragment '[IotaBoolean, IotaBoolean] '[IotaBoolean]|]
     ]
 )

$( mkExpr
     "NegationPurification"
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaBoolean, IotaBoolean] '[IotaBoolean]|]
     ]
 )

$( mkExpr
     "ExclusionDistillation"
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|forall a. Fragment '[IotaList a, IotaList a] '[IotaList a]|]
     , -- IotaHList
       [t|Fragment '[IotaBoolean, IotaBoolean] '[IotaBoolean]|]
     ]
 )

$( mkExpr
     "MaximusDistillation"
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaBoolean]|]]
 )

$( mkExpr
     "MinimusDistillation"
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaBoolean]|]]
 )

$( mkExpr
     "MaximusDistillationII"
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaBoolean]|]]
 )

$( mkExpr
     "MinimusDistillationII"
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaBoolean]|]]
 )

$( mkExpr
     "EqualityDistillation"
     [[t|Fragment '[IotaAny, IotaAny] '[IotaBoolean]|]]
 )

$( mkExpr
     "InequalityDistillation"
     [[t|Fragment '[IotaAny, IotaAny] '[IotaBoolean]|]]
 )

$( mkExpr
     "AugursPurification"
     [[t|forall a. Fragment '[a] '[IotaBoolean]|]]
 )

$( mkExpr
     "AugursExaltation"
     [[t|forall a. Fragment '[a, a, IotaBoolean] '[a]|]]
 )

$( mkExpr
     "EntropyReflection"
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkExpr
     "SinePurification"
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkExpr
     "CosinePurification"
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkExpr
     "TangentPurification"
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkExpr
     "InverseSinePurification"
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkExpr
     "InverseCosinePurification"
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkExpr
     "InverseTangentPurification"
     [[t|Fragment '[IotaNumber] '[IotaNumber]|]]
 )

$( mkExpr
     "InverseTangentDistillation"
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]]
 )

$( mkExpr
     "LogarithmicDistillation"
     [[t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]]
 )

$( mkExpr
     "ModulusDistillation"
     [ [t|Fragment '[IotaNumber, IotaNumber] '[IotaNumber]|]
     , [t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaNumber, IotaVector] '[IotaVector]|]
     , [t|Fragment '[IotaVector, IotaNumber] '[IotaVector]|]
     ]
 )

$( mkExpr
     "UniquenessPurification"
     [[t|forall a. Fragment '[IotaList a] '[IotaList a]|]]
 )

$( mkExpr
     "Reveal"
     [[t|forall a. Fragment '[a] '[a]|]]
 )

$( mkExpr
     "Explosion"
     [[t|Fragment '[IotaNumber, IotaVector] '[]|]]
 )

$( mkExpr
     "Fireball"
     [[t|Fragment '[IotaVector, IotaNumber] '[]|]]
 )

$( mkExpr
     "Impulse"
     [[t|Fragment '[IotaVector, IotaEntity] '[]|]]
 )

$( mkExpr
     "Blink"
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkExpr
     "BreakBlock"
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkExpr
     "PlaceBlock"
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkExpr
     "InternalizePigment"
     [[t|Fragment '[] '[]|]]
 )

$( mkExpr
     "CastersGlamour"
     [[t|Fragment '[] '[]|]]
 )

$( mkExpr
     "CreateWater"
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkExpr
     "DestroyLiquid"
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkExpr
     "Ignite"
     [ [t|Fragment '[IotaEntity] '[]|]
     , [t|Fragment '[IotaVector] '[]|]
     ]
 )

$( mkExpr
     "ExtinguishArea"
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkExpr
     "ConjureBlock"
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkExpr
     "ConjureLight"
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkExpr
     "Overgrow"
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkExpr
     "RechargeItem"
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkExpr
     "EraseItem"
     [[t|Fragment '[] '[]|]]
 )

$( mkExpr
     "EdifySapling"
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkExpr
     "MakeNote"
     [[t|Fragment '[IotaNumber, IotaNumber, IotaVector] '[]|]]
 )

$( mkExpr
     "CraftCypher"
     [[t|Fragment '[IotaAnyList, IotaEntity] '[]|]]
 )

$( mkExpr
     "CraftTrinket"
     [[t|Fragment '[IotaAnyList, IotaEntity] '[]|]]
 )

$( mkExpr
     "CraftArtifact"
     [[t|Fragment '[IotaAnyList, IotaEntity] '[]|]]
 )

$( mkExpr
     "CraftPhial"
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkExpr
     "WhiteSunsNadir"
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkExpr
     "BlueSunsNadir"
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkExpr
     "BlackSunsNadir"
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkExpr
     "RedSunsNadir"
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkExpr
     "GreenSunsNadir"
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkExpr
     "WhiteSunsZenith"
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkExpr
     "BlueSunsZenith"
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkExpr
     "BlackSunsZenith"
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkExpr
     "RedSunsZenith"
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkExpr
     "GreenSunsZenith"
     [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]]
 )

$( mkExpr
     "AnchoritesFlight"
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkExpr
     "WayfarersFlight"
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkExpr
     "AviatorsPurification"
     [[t|Fragment '[IotaEntity] '[IotaBoolean]|]]
 )

$( mkExpr
     "SummonSentinel"
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkExpr
     "BanishSentinel"
     [[t|Fragment '[] '[]|]]
 )

$( mkExpr
     "LocateSentinel"
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkExpr
     "WayfindSentinel"
     [[t|Fragment '[IotaVector] '[IotaVector]|]]
 )

$( mkExpr
     "Altiora"
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkExpr
     "CreateLava"
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkExpr
     "GreaterTeleport"
     [[t|Fragment '[IotaVector, IotaEntity] '[]|]]
 )

$( mkExpr
     "SummonGreaterSentinel"
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkExpr
     "DispelRain"
     [[t|Fragment '[] '[]|]]
 )

$( mkExpr
     "SummonRain"
     [[t|Fragment '[] '[]|]]
 )

$( mkExpr
     "FlayMind"
     [[t|Fragment '[IotaVector, IotaEntity] '[]|]]
 )

$( mkExpr
     "AkashasDistillation"
     [[t|Fragment '[IotaPattern, IotaVector] '[IotaAny]|]]
 )

$( mkExpr
     "AkashasGambit"
     [[t|Fragment '[IotaAny, IotaPattern, IotaVector] '[]|]]
 )

exprCharonsGambit :: HListLen bs => Expr blk as -> Expr blk bs
exprCharonsGambit = E.callUnsafe $ Seq.singleton $ iotaCast iotaCharonsGambit

$( mkExpr
     "ScribesReflection"
     [[t|Fragment '[] '[IotaAny]|]]
 )

$( mkExpr
     "ChroniclersPurification"
     [ [t|Fragment '[IotaVector] '[IotaAny]|]
     , [t|Fragment '[IotaEntity] '[IotaAny]|]
     ]
 )

$( mkExpr
     "ScribesGambit"
     [[t|Fragment '[IotaAny] '[]|]]
 )

$( mkExpr
     "ChroniclersGambit"
     [ [t|Fragment '[IotaAny, IotaVector] '[]|]
     , [t|Fragment '[IotaAny, IotaEntity] '[]|]
     ]
 )

$( mkExpr
     "AuditorsReflection"
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkExpr
     "AuditorsPurification"
     [ [t|Fragment '[IotaVector] '[IotaBoolean]|]
     , [t|Fragment '[IotaEntity] '[IotaBoolean]|]
     ]
 )

$( mkExpr
     "AssessorsReflection"
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkExpr
     "AssessorsPurification"
     [ [t|Fragment '[IotaVector] '[IotaBoolean]|]
     , [t|Fragment '[IotaEntity] '[IotaBoolean]|]
     ]
 )

$( mkExpr
     "MuninnsReflection"
     [[t|Fragment '[] '[IotaAny]|]]
 )

$( mkExpr
     "HuginnsGambit"
     [[t|forall a. Fragment '[a] '[]|]]
 )

$( mkExpr
     "ThanatosReflection"
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkExpr
     "NullaryReflection"
     [[t|Fragment '[] '[IotaNull]|]]
 )

$( mkExpr
     "TrueReflection"
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkExpr
     "FalseReflection"
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkExpr
     "VectorReflectionPX"
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkExpr
     "VectorReflectionPY"
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkExpr
     "VectorReflectionPZ"
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkExpr
     "VectorReflectionNX"
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkExpr
     "VectorReflectionNY"
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkExpr
     "VectorReflectionNZ"
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkExpr
     "VectorReflectionZero"
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkExpr
     "ArcsReflection"
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkExpr
     "CirclesReflection"
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkExpr
     "EulersReflection"
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkExpr
     "EntityPurification"
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkExpr
     "EntityPurificationAnimal"
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkExpr
     "EntityPurificationMonster"
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkExpr
     "EntityPurificationItem"
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkExpr
     "EntityPurificationPlayer"
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkExpr
     "EntityPurificationLiving"
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkExpr
     "ZoneDistillationAny"
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkExpr
     "ZoneDistillationAnimal"
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkExpr
     "ZoneDistillationNonAnimal"
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkExpr
     "ZoneDistillationMonster"
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkExpr
     "ZoneDistillationNonMonster"
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkExpr
     "ZoneDistillationItem"
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkExpr
     "ZoneDistillationNonItem"
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkExpr
     "ZoneDistillationPlayer"
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkExpr
     "ZoneDistillationNonPlayer"
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkExpr
     "ZoneDistillationLiving"
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkExpr
     "ZoneDistillationNonLiving"
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkExpr
     "IntegrationDistillation"
     [ [t|forall a. Fragment '[a, IotaList a] '[IotaList a]|]
     , [t|forall a as as' p. (HReverse (a : p) as', HReverse p as) => Fragment '[a, IotaHList as] '[IotaHList as']|]
     ]
 )

$( mkExpr
     "DerivationDecomposition"
     [ [t|forall a. Fragment '[IotaList a] '[a, IotaList a]|]
     , [t|forall a as as' p. (HReverse (a : p) as', HReverse p as) => Fragment '[IotaHList as'] '[a, IotaHList as]|]
     ]
 )

$( mkExpr
     "SelectionDistillation"
     [[t|forall a. Fragment '[IotaNumber, IotaList a] '[a]|]]
 )

$( mkExpr
     "SinglesPurification"
     [[t|forall a. Fragment '[a] '[IotaHList '[a]]|]]
 )

$( mkExpr
     "VacantReflection"
     [[t|Fragment '[] '[IotaHList '[]]|]]
 )

$( mkExpr
     "RetrogradePurification"
     [ [t|forall a. Fragment '[IotaList a] '[IotaList a]|]
     , [t|forall as as'. HReverse as as' => Fragment '[IotaHList as] '[IotaHList as']|]
     ]
 )

$( mkExpr
     "FlocksDisintegration"
     []
 )

instance (HReverse s rs, HListLen rs) => ExprFlocksDisintegration '[IotaHList s] rs

$( mkExpr
     "LocatorsDistillation"
     [[t|forall a. Fragment '[a, IotaList a] '[IotaNumber]|]]
 )

$( mkExpr
     "ExcisorsDistillation"
     [[t|forall a. Fragment '[IotaNumber, IotaList a] '[IotaList a]|]]
 )

$( mkExpr
     "SelectionExaltation"
     [[t|forall a. Fragment '[IotaNumber, IotaNumber, IotaList a] '[IotaList a]|]]
 )

$( mkExpr
     "SurgeonsExaltation"
     [[t|forall a. Fragment '[a, IotaNumber, IotaList a] '[IotaList a]|]]
 )

$( mkExpr
     "SpeakersDistillation"
     [ [t|forall a. Fragment '[a, IotaList a] '[IotaList a]|]
     , [t|forall a as. Fragment '[a, IotaHList as] '[IotaHList (a ': as)]|]
     ]
 )

$( mkExpr
     "SpeakersDecomposition"
     [ [t|forall a. Fragment '[IotaList a] '[a, IotaList a]|]
     , [t|forall a as as'. as' ~ a ': as => Fragment '[IotaHList as'] '[a, IotaHList as]|]
     ]
 )

$( mkExpr
     "GulliversPurification"
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkExpr
     "AlterScale"
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

-- special

exprNumericalReflection :: Int -> Expr blk '[IotaNumber]
exprNumericalReflection n = E.introUnsafe $ Seq.singleton $ iotaCast $ iotaNumericalReflection n
