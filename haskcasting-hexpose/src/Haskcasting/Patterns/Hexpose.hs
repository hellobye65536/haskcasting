{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Patterns.Hexpose where

import Haskcasting.ExprLang.TH (mkIotaFragExpr)
import Haskcasting.Fragment (Fragment)
import Haskcasting.Iota (IotaAny, IotaBoolean, IotaEntity, IotaList, IotaNull, IotaNumber, IotaVector)
import Haskcasting.Iota.Hexpose (IotaDisplay, IotaIdentifier, IotaItem)
import Haskcasting.Iota.Moreiotas (IotaEntityType, IotaIotaType, IotaItemStack, IotaItemType)
import Haskcasting.Pattern (pattern)

-- Cross-Mod Compatibility

-- Competing Standards

$( mkIotaFragExpr
     "DarwinsPurification"
     [pattern| EAST adwaq |]
     [ [t|Fragment '[IotaEntityType] '[IotaIdentifier]|]
     , [t|Fragment '[IotaIdentifier] '[IotaEntityType]|]
     ]
 )

$( mkIotaFragExpr
     "AtomPurification"
     [pattern| EAST adwawd |]
     [ [t|Fragment '[IotaIotaType] '[IotaIdentifier]|]
     , [t|Fragment '[IotaIdentifier] '[IotaIotaType]|]
     ]
 )

$( mkIotaFragExpr
     "ErosionPurification"
     [pattern| EAST adwaqa |]
     [ [t|Fragment '[IotaItemType] '[IotaIdentifier]|]
     , [t|Fragment '[IotaIdentifier] '[IotaItemType]|]
     ]
 )

$( mkIotaFragExpr
     "ExchangePurification"
     [pattern| EAST adwaqw |]
     [ [t|Fragment '[IotaItemStack] '[IotaItem]|]
     , [t|Fragment '[IotaItem] '[IotaItemStack]|]
     ]
 )

--- Patterns

-- Enlightened Patterns

$( mkIotaFragExpr
     "EpiphanyPurification"
     [pattern| SOUTH_EAST awqaqqq |]
     [[t|Fragment '[IotaEntity] '[IotaBoolean]|]]
 )

$( mkIotaFragExpr
     "SentiencePurification"
     [pattern| SOUTH_EAST qqqaqqq |]
     [[t|Fragment '[IotaEntity] '[IotaBoolean]|]]
 )

-- Display Iota

$( mkIotaFragExpr
     "ReadingPurification"
     [pattern| SOUTH_WEST awaqeeeee |]
     [[t|forall a. Fragment '[a] '[IotaDisplay]|]]
 )

$( mkIotaFragExpr
     "ParsingPurification"
     [pattern| SOUTH_EAST dwdeqqqqq |]
     [[t|Fragment '[IotaDisplay] '[IotaList IotaDisplay]|]]
 )

$( mkIotaFragExpr
     "LumièreGambit"
     [pattern| SOUTH_WEST awaqeeeeewded |]
     [[t|Fragment '[IotaVector, IotaDisplay] '[IotaDisplay]|]]
 )

$( mkIotaFragExpr
     "CalligraphersGambit"
     [pattern| SOUTH_WEST awaqeeeeedaqa |]
     [[t|Fragment '[IotaNumber, IotaDisplay] '[IotaDisplay]|]]
 )

$( mkIotaFragExpr
     "GothicGambit"
     [pattern| SOUTH_WEST awaqeeeeedd |]
     [[t|Fragment '[IotaBoolean, IotaDisplay] '[IotaDisplay]|]]
 )

$( mkIotaFragExpr
     "ManutiusGambit"
     [pattern| SOUTH_WEST awaqeeeeede |]
     [[t|Fragment '[IotaBoolean, IotaDisplay] '[IotaDisplay]|]]
 )

$( mkIotaFragExpr
     "NotetakersGambit"
     [pattern| SOUTH_WEST awaqeeeeedw |]
     [[t|Fragment '[IotaBoolean, IotaDisplay] '[IotaDisplay]|]]
 )

$( mkIotaFragExpr
     "EditorsGambit"
     [pattern| SOUTH_WEST awaqeeeeedq |]
     [[t|Fragment '[IotaBoolean, IotaDisplay] '[IotaDisplay]|]]
 )

$( mkIotaFragExpr
     "CensorsGambit"
     [pattern| SOUTH_WEST awaqeeeeeda |]
     [[t|Fragment '[IotaBoolean, IotaDisplay] '[IotaDisplay]|]]
 )

-- Display Iota Manipulation

$( mkIotaFragExpr
     "StylisticDistillation"
     [pattern| SOUTH_EAST dwdeqqqqqdda |]
     [[t|Fragment '[IotaDisplay, IotaDisplay] '[IotaBoolean]|]]
 )

$( mkIotaFragExpr
     "CalculatorPurification"
     [pattern| SOUTH_EAST dwdewqqqwqqaeq |]
     [[t|Fragment '[IotaDisplay] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "CleavingDistillation"
     [pattern| SOUTH_EAST dwdeqqqwqqqqae |]
     [[t|Fragment '[IotaDisplay, IotaDisplay] '[IotaList IotaDisplay]|]]
 )

$( mkIotaFragExpr
     "StreamingPurification"
     [pattern| SOUTH_EAST dwdeqqqqqdeee |]
     [[t|Fragment '[IotaDisplay] '[IotaList IotaDisplay]|]]
 )

--- Spells

-- Item Renaming

$( mkIotaFragExpr
     "NameItem"
     [pattern| SOUTH_EAST qwawqwaadwa |]
     [ [t|Fragment '[IotaDisplay, IotaEntity] '[]|]
     , [t|Fragment '[IotaNull, IotaEntity] '[]|]
     ]
 )

$( mkIotaFragExpr
     "DescribeItem"
     [pattern| NORTH_WEST dwewdweedwa |]
     [ [t|Fragment '[IotaList IotaDisplay, IotaEntity] '[]|]
     , [t|Fragment '[IotaNull, IotaEntity] '[]|]
     ]
 )

--- Scrying

-- Blocks

$( mkIotaFragExpr
     "VoidPurification"
     [pattern| NORTH_EAST edeeeee |]
     [[t|Fragment '[IotaVector] '[IotaBoolean]|]]
 )

$( mkIotaFragExpr
     "OverwritingPurification"
     [pattern| NORTH_EAST eaqqqqqe |]
     [[t|Fragment '[IotaVector] '[IotaBoolean]|]]
 )

$( mkIotaFragExpr
     "MinersPurification"
     [pattern| EAST qaqqqqqeeeeedq |]
     [[t|Fragment '[IotaIdentifier] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "DemomansPurification"
     [pattern| EAST qaqqqqqewaawaawa |]
     [[t|Fragment '[IotaIdentifier] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "OrientationPurification"
     [pattern| EAST qaqqqqqqwadeeed |]
     [[t|Fragment '[IotaVector] '[IotaVector]|]]
 )

$( mkIotaFragExpr
     "FarmersPurification"
     [pattern| EAST qaqqqqqwaea |]
     [[t|Fragment '[IotaVector] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "FacetPurification"
     [pattern| EAST qaqqqqeqqqwqaww |]
     [[t|Fragment '[IotaVector] '[IotaList IotaIdentifier]|]]
 )

$( mkIotaFragExpr
     "FacetDistillation"
     [pattern| EAST qaqqqqqeawa |]
     [[t|Fragment '[IotaIdentifier, IotaVector] '[IotaAny]|]]
 )

$( mkIotaFragExpr
     "SkatingPurification"
     [pattern| EAST qaqqqqqdaqwqwqa |]
     [[t|Fragment '[IotaIdentifier] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "CartographersPurification"
     [pattern| EAST qwedewqqqqq |]
     [[t|Fragment '[IotaIdentifier] '[IotaVector]|]]
 )

-- Chat

$( mkIotaFragExpr
     "NewsReflection"
     [pattern| SOUTH_WEST aeeedw |]
     [[t|Fragment '[] '[IotaNumber, IotaDisplay, IotaDisplay]|]]
 )

$( mkIotaFragExpr
     "NewsDisintegration"
     [pattern| SOUTH_EAST dqqqaw |]
     [[t|Fragment '[IotaNumber] '[IotaNumber, IotaDisplay, IotaDisplay]|]]
 )

-- Enchantments

$( mkIotaFragExpr
     "ThaumaturgistsPurification"
     [pattern| WEST waqwwqawqwawaw |]
     [[t|Fragment '[IotaItem] '[IotaList IotaIdentifier]|]]
 )

$( mkIotaFragExpr
     "CharmDistillation"
     [pattern| EAST wdewwedwewdwdw |]
     [[t|Fragment '[IotaIdentifier, IotaItem] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "ConjuringPurification"
     [pattern| NORTH_EAST waawdedwd |]
     [[t|Fragment '[IotaIdentifier] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "ConjuringDistillation"
     [pattern| WEST aaqqadaqwqa |]
     [[t|Fragment '[IotaIdentifier, IotaItem] '[IotaBoolean]|]]
 )

$( mkIotaFragExpr
     "ValleyPurification"
     [pattern| WEST waqwqaqwaaw |]
     [[t|Fragment '[IotaIdentifier] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "PeakPurification"
     [pattern| EAST wdewedqwaaw |]
     [[t|Fragment '[IotaIdentifier] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "CursePurification"
     [pattern| NORTH_WEST aeaqwqaqwaaw |]
     [[t|Fragment '[IotaIdentifier] '[IotaBoolean]|]]
 )

$( mkIotaFragExpr
     "FablePurification"
     [pattern| WEST aqwqaeaqwddw |]
     [[t|Fragment '[IotaIdentifier] '[IotaBoolean]|]]
 )

-- Entites

$(mkIotaFragExpr "CalipersPurification" [pattern| NORTH_WEST dwe |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])
$(mkIotaFragExpr "TheodolitePurification" [pattern| EAST wqaa |] [[t|Fragment '[IotaEntity] '[IotaVector]|]])
$(mkIotaFragExpr "VitalityPurification" [pattern| SOUTH_EAST wddwaqqwawq |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])
$(mkIotaFragExpr "FitnessPurification" [pattern| SOUTH_EAST wddwwawaeqwawq |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])
$(mkIotaFragExpr "SuffocationPurification" [pattern| EAST wwaade |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])
$(mkIotaFragExpr "LungPurification" [pattern| EAST wwaadee |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])
$(mkIotaFragExpr "InfernoPurification" [pattern| WEST eewdead |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])
$(mkIotaFragExpr "EndermansPurification" [pattern| SOUTH_WEST qqqqwaadq |] [[t|Fragment '[IotaEntity] '[IotaBoolean]|]])
$(mkIotaFragExpr "YouthPurification" [pattern| SOUTH_WEST awaqdwaaw |] [[t|Fragment '[IotaEntity] '[IotaBoolean]|]])
$(mkIotaFragExpr "ReproductionPurification" [pattern| EAST awaaqdqaawa |] [[t|Fragment '[IotaEntity] '[IotaBoolean]|]])
$(mkIotaFragExpr "SlothsPurification" [pattern| NORTH_WEST aqaew |] [[t|Fragment '[IotaEntity] '[IotaBoolean]|]])
$(mkIotaFragExpr "RacersPurification" [pattern| WEST eaq |] [[t|Fragment '[IotaEntity] '[IotaBoolean]|]])
$(mkIotaFragExpr "VehiclePurification" [pattern| EAST eqqedwewew |] [[t|Fragment '[IotaEntity] '[IotaEntity]|]])
$(mkIotaFragExpr "JockeyPurification" [pattern| EAST qeeqawqwqw |] [[t|Fragment '[IotaEntity] '[IotaList IotaEntity]|]])
$(mkIotaFragExpr "FixationPurification" [pattern| SOUTH_WEST aqwedewwded |] [[t|Fragment '[IotaEntity] '[IotaEntity]|]])
$(mkIotaFragExpr "GrudgePurification" [pattern| NORTH_EAST aqawwqaqwed |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])
$(mkIotaFragExpr "VictimPurification" [pattern| NORTH_WEST qqqwaeqa |] [[t|Fragment '[IotaEntity] '[IotaEntity]|]])
$(mkIotaFragExpr "ScarPurification" [pattern| EAST deqdweee |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])
$(mkIotaFragExpr "NamePurification" [pattern| SOUTH_WEST edeweedw |] [[t|Fragment '[IotaEntity] '[IotaDisplay]|]])
$(mkIotaFragExpr "AdorationPurification" [pattern| WEST qdaqwawqeewde |] [[t|Fragment '[IotaEntity] '[IotaEntity]|]])
$(mkIotaFragExpr "MalevolencePurification" [pattern| NORTH_EAST qaedwaa |] [[t|Fragment '[IotaEntity] '[IotaBoolean]|]])
$(mkIotaFragExpr "ShooterPurification" [pattern| EAST aadedade |] [[t|Fragment '[IotaEntity] '[IotaEntity]|]])
$(mkIotaFragExpr "AbsorptionPurification" [pattern| NORTH_EAST waawedwdwd |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])

-- Environment

$( mkIotaFragExpr
     "AmbitPurification"
     [pattern| EAST wawaw |]
     [ [t|Fragment '[IotaEntity] '[IotaBoolean]|]
     , [t|Fragment '[IotaVector] '[IotaBoolean]|]
     ]
 )

$(mkIotaFragExpr "StaffReflection" [pattern| NORTH_EAST waaq |] [[t|Fragment '[] '[IotaBoolean]|]])
$(mkIotaFragExpr "DexterityReflection" [pattern| NORTH_EAST qaqqqwaaq |] [[t|Fragment '[] '[IotaBoolean]|]])
$(mkIotaFragExpr "DeviceReflection" [pattern| NORTH_EAST waaqwwaqqqqq |] [[t|Fragment '[] '[IotaBoolean]|]])
$(mkIotaFragExpr "ConstructedReflection" [pattern| NORTH_EAST waaqdeaqwqae |] [[t|Fragment '[] '[IotaBoolean]|]])

-- Food

$(mkIotaFragExpr "HungerPurification" [pattern| WEST qqqadaddw |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])
$(mkIotaFragExpr "StaminaPurification" [pattern| WEST qqqadaddq |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])
$(mkIotaFragExpr "CaloriePurification" [pattern| WEST adaqqqddqe |] [[t|Fragment '[IotaIdentifier] '[IotaNumber]|]])
$(mkIotaFragExpr "SatiationPurification" [pattern| WEST adaqqqddqw |] [[t|Fragment '[IotaIdentifier] '[IotaNumber]|]])
$(mkIotaFragExpr "FleshPurification" [pattern| WEST adaqqqddaed |] [[t|Fragment '[IotaIdentifier] '[IotaBoolean]|]])
$(mkIotaFragExpr "DessertPurification" [pattern| WEST adaqqqddaq |] [[t|Fragment '[IotaIdentifier] '[IotaBoolean]|]])
$(mkIotaFragExpr "EdibilityPurification" [pattern| WEST adaqqqdd |] [[t|Fragment '[IotaIdentifier] '[IotaBoolean]|]])

-- Identifiers

$( mkIotaFragExpr
     "DetectivesPurification"
     [pattern| NORTH_EAST qqqqqe |]
     [ [t|Fragment '[IotaEntity] '[IotaIdentifier]|]
     , [t|Fragment '[IotaVector] '[IotaIdentifier]|]
     , [t|Fragment '[IotaItem] '[IotaIdentifier]|]
     ]
 )

$( mkIotaFragExpr
     "ModicumPurification"
     [pattern| WEST edqdeq |]
     [[t|forall a. Fragment '[a] '[IotaIdentifier]|]]
 )

-- Items

$(mkIotaFragExpr "ItemPurification" [pattern| WEST edeedq |] [[t|Fragment '[IotaEntity] '[IotaItem]|]])
$(mkIotaFragExpr "OfferDistillation" [pattern| EAST qaqqae |] [[t|Fragment '[IotaNumber, IotaIdentifier] '[IotaItem]|]])
$(mkIotaFragExpr "ToolPurification" [pattern| NORTH_EAST qaqqqq |] [[t|Fragment '[IotaEntity] '[IotaItem]|]])
$(mkIotaFragExpr "AccessoryPurification" [pattern| NORTH_WEST edeeee |] [[t|Fragment '[IotaEntity] '[IotaItem]|]])
$(mkIotaFragExpr "CartPurification" [pattern| WEST edeeeeeqdee |] [[t|Fragment '[IotaEntity] '[IotaList IotaItem]|]])
$(mkIotaFragExpr "ChestPurification" [pattern| EAST qaqqqqqeaqq |] [[t|Fragment '[IotaVector] '[IotaList IotaItem]|]])
$( mkIotaFragExpr
     "AegisPurification"
     [pattern| NORTH_EAST qaqddqeeeeqd |]
     [[t|Fragment '[IotaEntity] '[IotaList IotaItem]|]]
 )
$(mkIotaFragExpr "PocketReflection" [pattern| NORTH_EAST qaqdqaqdeeewedw |] [[t|Fragment '[] '[IotaList IotaItem]|]])
$(mkIotaFragExpr "StoragePurification" [pattern| EAST qaqqwqqqw |] [[t|Fragment '[IotaItem] '[IotaNumber]|]])
$(mkIotaFragExpr "WarehousePurification" [pattern| WEST edeeweeew |] [[t|Fragment '[IotaIdentifier] '[IotaNumber]|]])
$(mkIotaFragExpr "DeteriorationPurification" [pattern| NORTH_EAST eeweeewdeq |] [[t|Fragment '[IotaItem] '[IotaNumber]|]])
$( mkIotaFragExpr
     "FragilityPurification"
     [pattern| NORTH_WEST qqwqqqwaqe |]
     [[t|Fragment '[IotaIdentifier] '[IotaNumber]|]]
 )
$(mkIotaFragExpr "AuditorsPurification" [pattern| EAST wawqwqwqwqwqwew |] [[t|Fragment '[IotaItem] '[IotaBoolean]|]])
$(mkIotaFragExpr "ChroniclersPurification" [pattern| EAST wawqwqwqwqwqw |] [[t|Fragment '[IotaItem] '[IotaAny]|]])
$(mkIotaFragExpr "AppellationPurification" [pattern| SOUTH_EAST qwawqwaqea |] [[t|Fragment '[IotaItem] '[IotaDisplay]|]])
$( mkIotaFragExpr
     "LegacyPurification"
     [pattern| NORTH_WEST dwewdwedea |]
     [[t|Fragment '[IotaItem] '[IotaList IotaDisplay]|]]
 )
$(mkIotaFragExpr "GlamourPurification" [pattern| WEST dwaawaqwa |] [[t|Fragment '[IotaItem] '[IotaNumber]|]])
$(mkIotaFragExpr "GlamourPurificationII" [pattern| WEST dwaawaqwawq |] [[t|Fragment '[IotaItem] '[IotaNumber]|]])
$(mkIotaFragExpr "LiteraturePurification" [pattern| WEST awqqwaqd |] [[t|Fragment '[IotaItem] '[IotaList IotaDisplay]|]])
$( mkIotaFragExpr
     "BibliographyPurification"
     [pattern| EAST eaedweew |]
     [[t|Fragment '[IotaItem] '[IotaNumber, IotaDisplay]|]]
 )
$(mkIotaFragExpr "CollectorPurification" [pattern| NORTH_EAST wqqed |] [[t|Fragment '[IotaItem] '[IotaNumber]|]])

-- Media

$( mkIotaFragExpr
     "MediaReflection"
     [pattern| WEST dde |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "MediaPurification"
     [pattern| WEST ddew |]
     [ [t|Fragment '[IotaEntity] '[IotaNumber]|]
     , [t|Fragment '[IotaVector] '[IotaNumber]|]
     , [t|Fragment '[IotaItem] '[IotaNumber]|]
     ]
 )

$( mkIotaFragExpr
     "PotentialPurification"
     [pattern| EAST ddea |]
     [ [t|Fragment '[IotaEntity] '[IotaNumber]|]
     , [t|Fragment '[IotaVector] '[IotaNumber]|]
     , [t|Fragment '[IotaItem] '[IotaNumber]|]
     ]
 )

-- Miscellaneous

$( mkIotaFragExpr
     "FelinePurification"
     [pattern| SOUTH_WEST wqwqqwqwawaaw |]
     [[t|Fragment '[IotaEntity] '[IotaIdentifier]|]]
 )
$(mkIotaFragExpr "AngerPurification" [pattern| WEST dedwaqwede |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])
$(mkIotaFragExpr "ShowcasePurification" [pattern| NORTH_EAST ewdwewdea |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])
$(mkIotaFragExpr "ShowcaseGambit" [pattern| SOUTH_WEST awqwawqaa |] [[t|Fragment '[IotaNumber, IotaEntity] '[]|]])
$( mkIotaFragExpr
     "ArtisticPurification"
     [pattern| SOUTH_WEST wawwwqwwawwwqadaqeda |]
     [[t|Fragment '[IotaEntity] '[IotaIdentifier]|]]
 )

-- Status Effects

$( mkIotaFragExpr
     "DiagnosisPurification"
     [pattern| SOUTH_WEST wqqq |]
     [[t|Fragment '[IotaEntity] '[IotaList IotaIdentifier]|]]
 )

$( mkIotaFragExpr
     "PrescriptionPurification"
     [pattern| SOUTH_WEST wqqqadee |]
     [[t|Fragment '[IotaItem] '[IotaList IotaIdentifier]|]]
 )

$( mkIotaFragExpr
     "ConditionPurification"
     [pattern| SOUTH_WEST wqqqaawd |]
     [[t|Fragment '[IotaIdentifier] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "ConcentrationDistillation"
     [pattern| SOUTH_WEST wqqqaqwa |]
     [[t|Fragment '[IotaIdentifier, IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "ClearanceDistillation"
     [pattern| SOUTH_WEST wqqqaqwdd |]
     [[t|Fragment '[IotaIdentifier, IotaEntity] '[IotaNumber]|]]
 )

-- Tags

$( mkIotaFragExpr
     "GenusPurification"
     [pattern| NORTH_EAST qaqqqqwqqd |]
     [ [t|Fragment '[IotaEntity] '[IotaList IotaIdentifier]|]
     , [t|Fragment '[IotaIdentifier] '[IotaList IotaIdentifier]|]
     ]
 )

$( mkIotaFragExpr
     "GeologyPurification"
     [pattern| EAST qaqqqqqqwqqd |]
     [ [t|Fragment '[IotaVector] '[IotaList IotaIdentifier]|]
     , [t|Fragment '[IotaEntity] '[IotaList IotaIdentifier]|]
     , [t|Fragment '[IotaItem] '[IotaList IotaIdentifier]|]
     , [t|Fragment '[IotaIdentifier] '[IotaList IotaIdentifier]|]
     ]
 )

$( mkIotaFragExpr
     "BaublePurification"
     [pattern| EAST aqawawqqqd |]
     [ [t|Fragment '[IotaEntity] '[IotaList IotaIdentifier]|]
     , [t|Fragment '[IotaItem] '[IotaList IotaIdentifier]|]
     , [t|Fragment '[IotaIdentifier] '[IotaList IotaIdentifier]|]
     ]
 )

-- Villagers

$( mkIotaFragExpr
     "TierPurification"
     [pattern| EAST qeqwqwqwqwqeqawdaeaeaeaeaea |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "ProfessionalPurification"
     [pattern| EAST qeqwqwqwqwqeqawewawqwawadeeeee |]
     [[t|Fragment '[IotaEntity] '[IotaIdentifier]|]]
 )

$( mkIotaFragExpr
     "CulturePurification"
     [pattern| EAST qeqwqwqwqwqeqaweqqqqqwded |]
     [[t|Fragment '[IotaEntity] '[IotaIdentifier]|]]
 )

$( mkIotaFragExpr
     "NurturePurification"
     [pattern| EAST qeqwqwqwqwqeqawewwqqwwqwwqqww |]
     [[t|Fragment '[IotaIdentifier] '[IotaIdentifier]|]]
 )

-- World

$( mkIotaFragExpr
     "LuminancePurification"
     [pattern| SOUTH_WEST wqwqwqwqwqwaeqqqqaeqaeaeaeaw |]
     [[t|Fragment '[IotaVector] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "MeterologistsReflection"
     [pattern| WEST eweweweweweeeaedqdqde |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "BatteryPurification"
     [pattern| EAST qwqwqwqwqwqqwwaadwdaaww |]
     [[t|Fragment '[IotaVector] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "PeripheralPurification"
     [pattern| WEST eweweweweweewwddawaddww |]
     [[t|Fragment '[IotaVector] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "CircadianReflection"
     [pattern| SOUTH_EAST wwawwawwqqawwdwwdwwaqwqwqwqwq |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "TemporalReflection"
     [pattern| SOUTH_EAST wddwaqqwqaddaqqwddwaqqwqaddaq |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "GeographicalPurification"
     [pattern| WEST qwqwqawdqqaqqdwaqwqwq |]
     [[t|Fragment '[IotaVector] '[IotaIdentifier]|]]
 )

$( mkIotaFragExpr
     "PlaneReflection"
     [pattern| WEST qwqwqwqwqwqqaedwaqd |]
     [[t|Fragment '[] '[IotaIdentifier]|]]
 )

$( mkIotaFragExpr
     "LunarReflection"
     [pattern| WEST eweweweweweeweeedadw |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "ExorcistsPurification"
     [pattern| WEST eweweweweweeweeeeewdeee |]
     [[t|Fragment '[IotaVector] '[IotaBoolean]|]]
 )

$( mkIotaFragExpr
     "RealityPurification"
     [pattern| WEST eweweweweweeedaawaqd |]
     [[t|Fragment '[IotaVector] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "DistortionReflection"
     [pattern| SOUTH_WEST aqwawqwqqwqwqwqwqwq |]
     [[t|Fragment '[] '[IotaBoolean]|]]
 )
