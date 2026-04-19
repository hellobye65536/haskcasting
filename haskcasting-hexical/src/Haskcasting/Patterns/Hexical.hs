{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Haskcasting.Patterns.Hexical where

import Haskcasting.ExprLang.TH (mkGreatIotaFragExpr, mkIotaFragExpr)
import Haskcasting.Iota (
  IotaAny,
  IotaAnyList,
  IotaBoolean,
  IotaEntity,
  IotaList,
  IotaNumber,
  IotaPattern,
  IotaVector,
 )
import Haskcasting.Iota.Hexical (IotaDye, IotaPigment)
import Haskcasting.Iota.Hexpose (IotaIdentifier)
import Haskcasting.Pattern (pattern)

$( mkIotaFragExpr
     "WriteGrimoire"
     [pattern| WEST aqwqaeaqa |]
     [[t|'[IotaAnyList, IotaPattern] -> '[]|]]
     -- ['list', 'pattern'] -> ['']
 )

$( mkIotaFragExpr
     "EraseGrimoire"
     [pattern| WEST aqwqaqded |]
     [[t|'[IotaPattern] -> '[]|]]
     -- ['pattern'] -> ['']
 )

$( mkIotaFragExpr
     "ArchivistReflection"
     [pattern| SOUTH_EAST aqaeaqwqa |]
     [[t|'[] -> '[IotaList IotaPattern]|]]
     -- [''] -> ['list of patterns']
 )

$( mkIotaFragExpr
     "AgeScroll"
     [pattern| EAST waeqqqqeqqqwqeaeaeaeq |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['animated scroll entity'] -> ['']
 )

$( mkIotaFragExpr
     "DyeInk"
     [pattern| EAST waeqqqqewqqwqqeqeqqwqqeq |]
     [[t|'[IotaVector, IotaEntity] -> '[]|]]
     -- ['vector', 'animated scroll entity'] -> ['']
 )

$( mkIotaFragExpr
     "IlluminateInk"
     [pattern| EAST waeqqqqedeqdqdqdqeqdwwd |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['animated scroll entity'] -> ['']
 )

$( mkIotaFragExpr
     "VanishScroll"
     [pattern| EAST waeqqqqedeqeeweeqewee |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['animated scroll entity'] -> ['']
 )

$( mkIotaFragExpr
     "PeriwinkleReflection"
     [pattern| NORTH_EAST qaqwqaqwqaq |]
     [[t|'[] -> '[IotaIdentifier]|]]
     -- [''] -> ['identifier']
 )

$( mkIotaFragExpr
     "Wish"
     [pattern| NORTH_WEST eweweweweweewedeaqqqd |]
     [[t|'[IotaList IotaPattern] -> '[]|]]
     -- ['list of patterns'] -> ['']
 )

$( mkIotaFragExpr
     "GenieReflectionSpatial"
     [pattern| SOUTH_WEST qwddedqdd |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

$( mkIotaFragExpr
     "GenieReflectionRotational"
     [pattern| SOUTH_WEST qwddedadw |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

$( mkIotaFragExpr
     "GenieReflectionKinetic"
     [pattern| SOUTH_WEST qwddedqew |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

$( mkIotaFragExpr
     "GenieReflectionTemporal"
     [pattern| SOUTH_WEST qwddedqwddwa |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

$( mkIotaFragExpr
     "GenieReflectionMedia"
     [pattern| SOUTH_WEST qwddedaeeeee |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

$( mkIotaFragExpr
     "GenieGambit"
     [pattern| SOUTH_WEST qwddedqedeeeee |]
     [[t|forall a. '[a] -> '[]|]]
     -- ['any'] -> ['']
 )

$( mkIotaFragExpr
     "GenieReflectionMemory"
     [pattern| SOUTH_WEST qwddedqwaqqqqq |]
     [[t|'[] -> '[IotaAny]|]]
     -- [''] -> ['any']
 )

$( mkIotaFragExpr
     "FinaleReflection"
     [pattern| EAST aaddaddad |]
     [[t|'[] -> '[IotaBoolean]|]]
     -- [''] -> ['boolean']
 )

$( mkIotaFragExpr
     "RefuelLamp"
     [pattern| EAST qaqwawqwqqwqwqwqwqwqq |]
     [[t|'[IotaNumber] -> '[]|]]
     -- ['number'] -> ['']
 )

$( mkIotaFragExpr
     "PromoteLamp"
     [pattern| WEST qweedeqeedeqdqdwewewwewewwewe |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['entity'] -> ['']
 )

$( mkIotaFragExpr
     "ArchgeniePurification"
     [pattern| NORTH_EAST qaqwddedqeed |]
     [[t|'[IotaEntity] -> '[IotaBoolean]|]]
     -- ['entity'] -> ['boolean']
 )

$( mkIotaFragExpr
     "ArchgenieReflectionSpatial"
     [pattern| NORTH_EAST qaqwddedqdd |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

$( mkIotaFragExpr
     "ArchgenieReflectionRotational"
     [pattern| NORTH_EAST qaqwddedadw |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

$( mkIotaFragExpr
     "ArchgenieReflectionKinetic"
     [pattern| NORTH_EAST qaqwddedqew |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

$( mkIotaFragExpr
     "ArchgenieReflectionTemporal"
     [pattern| NORTH_EAST qaqwddedqwddwa |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

$( mkIotaFragExpr
     "ArchgenieReflectionMedia"
     [pattern| NORTH_EAST qaqwddedaeeeee |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

$( mkIotaFragExpr
     "ArchgenieGambit"
     [pattern| NORTH_EAST qaqwddedqedeeeee |]
     [[t|forall a. '[a] -> '[]|]]
     -- ['any'] -> ['']
 )

$( mkIotaFragExpr
     "ArchgenieReflectionMemory"
     [pattern| NORTH_EAST qaqwddedqwaqqqqq |]
     [[t|'[] -> '[IotaAny]|]]
     -- [''] -> ['any']
 )

$( mkIotaFragExpr
     "CongruenceDistillation"
     [pattern| EAST aaqd |]
     [[t|'[IotaPattern, IotaPattern] -> '[IotaBoolean]|]]
     -- ['pattern', 'pattern'] -> ['boolean']
 )

$( mkIotaFragExpr
     "ChirographersPurification"
     [pattern| EAST wqaedeqd |]
     [[t|'[IotaPattern] -> '[IotaList IotaNumber]|]]
     -- ['pattern'] -> ['list of numbers']
 )

$( mkIotaFragExpr
     "CalligraphersPurification"
     [pattern| EAST wqqqaqwd |]
     [[t|'[IotaList IotaNumber] -> '[IotaPattern]|]]
     -- ['list of numbers'] -> ['pattern']
 )

$( mkIotaFragExpr
     "HandwritingDistillation"
     [pattern| NORTH_EAST eadqqqa |]
     [[t|'[IotaPattern] -> '[IotaList IotaVector]|]]
     -- ['pattern'] -> ['list of vectors']
 )

$( mkIotaFragExpr
     "GlyphmakersDistillation"
     [pattern| NORTH_EAST aqqqdae |]
     [[t|'[IotaNumber, IotaPattern] -> '[IotaPattern]|]]
     -- ['number', 'pattern'] -> ['pattern']
 )

$( mkIotaFragExpr
     "TelepathicReflection"
     [pattern| EAST wqqadaw |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

$( mkIotaFragExpr
     "SendThought"
     [pattern| EAST qqqqwaqa |]
     [[t|forall a. '[a] -> '[]|]]
     -- ['any'] -> ['']
 )

$( mkIotaFragExpr
     "ShoutThought"
     [pattern| EAST daqqqqwa |]
     [[t|forall a. '[a] -> '[]|]]
     -- ['any'] -> ['']
 )

$( mkIotaFragExpr
     "HallucinatePling"
     [pattern| NORTH_EAST eqqqada |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

$( mkIotaFragExpr
     "HallucinateClick"
     [pattern| NORTH_EAST eqqadaq |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

$( mkIotaFragExpr
     "OffensiveReflection"
     [pattern| NORTH_EAST qadee |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

$( mkIotaFragExpr
     "ManipulativeReflection"
     [pattern| NORTH_WEST edaqq |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

$( mkIotaFragExpr
     "ChargeReflection"
     [pattern| SOUTH_EAST aqaddq |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

$( mkIotaFragExpr
     "RetreatReflection"
     [pattern| SOUTH_WEST dedwdq |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

$( mkIotaFragExpr
     "DodgeReflection"
     [pattern| SOUTH_EAST edead |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

$( mkIotaFragExpr
     "EvadeReflection"
     [pattern| SOUTH_WEST qaqda |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

$( mkIotaFragExpr
     "LeapingReflection"
     [pattern| SOUTH_WEST qaqdaqqa |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

$( mkIotaFragExpr
     "StealthyReflection"
     [pattern| NORTH_WEST wede |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

$( mkIotaFragExpr
     "LibrariansPurification"
     [pattern| EAST qaqqadaq |]
     [[t|'[IotaVector] -> '[IotaPattern]|]]
     -- ['vector'] -> ['pattern/null']
 )

$( mkIotaFragExpr
     "LibrariansPurificationII"
     [pattern| EAST qaqqqada |]
     [[t|'[IotaVector] -> '[IotaAny]|]]
     -- ['vector'] -> ['any']
 )

$( mkIotaFragExpr
     "LibrariansGambit"
     [pattern| SOUTH_WEST edeeedad |]
     [[t|forall a. '[a, IotaPattern, IotaVector] -> '[]|]]
     -- ['any', 'pattern', 'vector'] -> ['']
 )

$( mkIotaFragExpr
     "LibrariansGambitII"
     [pattern| SOUTH_WEST edeedade |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

$( mkIotaFragExpr
     "ConjureMageBlock"
     [pattern| NORTH_WEST dee |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

$( mkIotaFragExpr
     "Bouncy"
     [pattern| NORTH_WEST deeqa |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

$( mkIotaFragExpr
     "Energized"
     [pattern| NORTH_WEST deewad |]
     [[t|'[IotaNumber, IotaVector] -> '[]|]]
     -- ['number', 'vector'] -> ['']
 )

$( mkIotaFragExpr
     "Ephemeral"
     [pattern| NORTH_WEST deewwaawd |]
     [[t|'[IotaNumber, IotaVector] -> '[]|]]
     -- ['number', 'vector'] -> ['']
 )

$( mkIotaFragExpr
     "Invisible"
     [pattern| NORTH_WEST deeqedeaqqqwqqq |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

$( mkIotaFragExpr
     "Replaceable"
     [pattern| NORTH_WEST deewqaqqqqq |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

$( mkIotaFragExpr
     "Volatile"
     [pattern| NORTH_WEST deewedeeeee |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

$( mkIotaFragExpr
     "Autograph"
     [pattern| WEST eeeeeww |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

$( mkIotaFragExpr
     "Unautograph"
     [pattern| NORTH_EAST wwqqqqq |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

$( mkIotaFragExpr
     "AuthenticatorsDistillation"
     [pattern| NORTH_EAST wwqqqqqaw |]
     []
     -- [[t|'[IotaEntity, item stack] -> '[IotaBoolean]|]]
     -- ['entity', 'item stack'] -> ['boolean']
 )

$( mkIotaFragExpr
     "ChromaticPurification"
     [pattern| NORTH_EAST weedwa |]
     [ [t|'[IotaIdentifier] -> '[IotaDye]|]
     , [t|'[IotaVector] -> '[IotaDye]|]
     , [t|'[IotaEntity] -> '[IotaDye]|]
     ]
     -- ['id/vector/entity'] -> ['dye/null']
 )

$( mkIotaFragExpr
     "Dye"
     [pattern| NORTH_WEST dwaqqw |]
     [ [t|'[IotaDye, IotaVector] -> '[]|]
     , [t|'[IotaDye, IotaEntity] -> '[]|]
     ]
     -- ['dye', 'vector/entity'] -> ['']
 )

$( mkIotaFragExpr
     "VisionPurification"
     [pattern| EAST wdwwaawwewdwwewwdwwe |]
     [[t|'[IotaDye] -> '[IotaVector]|]]
     -- ['dye'] -> ['vector']
 )

$( mkIotaFragExpr
     "MagicMissile"
     [pattern| WEST qaqww |]
     [[t|'[IotaVector, IotaVector] -> '[]|]]
     -- ['vector', 'vector'] -> ['']
 )

$( mkIotaFragExpr
     "PigmentPurification"
     [pattern| NORTH_WEST aqwedeweeeewweeew |]
     [ [t|'[IotaDye] -> '[IotaPigment]|]
     , [t|'[IotaEntity] -> '[IotaPigment]|]
     ]
     -- ['dye/entity'] -> ['pigment']
 )

$( mkIotaFragExpr
     "PigmentExaltation"
     [pattern| SOUTH_EAST edewqaqqqqqwqqq |]
     [[t|'[IotaNumber, IotaVector, IotaPigment] -> '[IotaVector]|]]
     -- ['num', 'vec', 'pigment'] -> ['vec']
 )

$( mkIotaFragExpr
     "InternalizePigmentII"
     [pattern| EAST weeeweeqeeeewqaqweeee |]
     [[t|'[IotaPigment] -> '[]|]]
     -- ['pigment'] -> ['']
 )

$( mkIotaFragExpr
     "Prestidigitation"
     [pattern| NORTH_EAST wedewedew |]
     [ [t|'[IotaEntity] -> '[]|]
     , [t|'[IotaVector] -> '[]|]
     ]
     -- ['entity/vector'] -> ['']
 )

$( mkIotaFragExpr
     "Wristpocket"
     [pattern| WEST aaqqa |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

$( mkIotaFragExpr
     "WristpocketReflection"
     [pattern| WEST aaqqada |]
     []
     -- [[t|'[] -> '[IotaItem]|]]
     -- [''] -> ['item']
 )

$( mkIotaFragExpr
     "Sleight"
     [pattern| WEST aaqqadeeeq |]
     [ [t|'[IotaEntity] -> '[]|]
     , [t|'[IotaVector] -> '[]|]
     ]
     -- ['item entity/vector'] -> ['']
 )

$( mkIotaFragExpr
     "MageHand"
     [pattern| WEST aaqqaeea |]
     [ [t|'[IotaEntity] -> '[]|]
     , [t|'[IotaVector] -> '[]|]
     ]
     -- ['entity/vector'] -> ['']
 )

$( mkIotaFragExpr
     "MageMouth"
     [pattern| WEST aaqqadaa |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

$( mkIotaFragExpr
     "ConjureSpeck"
     [pattern| SOUTH_WEST ade |]
     [[t|forall a. '[IotaVector, IotaVector, a] -> '[IotaEntity]|]]
     -- ['vector', 'vector', 'any'] -> ['entity']
 )

$( mkIotaFragExpr
     "MoveSpeck"
     [pattern| SOUTH_WEST adeqaa |]
     [[t|'[IotaVector, IotaEntity] -> '[]|]]
     -- ['vector', 'speck entity'] -> ['']
 )

$( mkIotaFragExpr
     "RotateSpeck"
     [pattern| SOUTH_WEST adeaw |]
     [[t|'[IotaVector, IotaEntity] -> '[]|]]
     -- ['vector', 'speck entity'] -> ['']
 )

$( mkIotaFragExpr
     "RollSpeck"
     [pattern| SOUTH_WEST adeqqqqq |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'speck entity'] -> ['']
 )

$( mkIotaFragExpr
     "AlterSpeck"
     [pattern| SOUTH_WEST adeeaqa |]
     [[t|forall a. '[a, IotaEntity] -> '[]|]]
     -- ['any', 'speck entity'] -> ['']
 )

$( mkIotaFragExpr
     "TimeSpeck"
     [pattern| SOUTH_WEST adeqqaawdd |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'speck entity'] -> ['']
 )

$( mkIotaFragExpr
     "ResizeSpeck"
     [pattern| SOUTH_WEST adeeqed |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'speck entity'] -> ['']
 )

$( mkIotaFragExpr
     "ThickenSpeck"
     [pattern| SOUTH_WEST adeeqw |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'speck entity'] -> ['']
 )

$( mkIotaFragExpr
     "PaintSpeck"
     [pattern| SOUTH_WEST adeqqaq |]
     [[t|'[IotaPigment, IotaEntity] -> '[]|]]
     -- ['pigment', 'speck entity'] -> ['']
 )

$( mkIotaFragExpr
     "DismissSpeck"
     [pattern| SOUTH_WEST adeaqde |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['speck entity'] -> ['']
 )

$( mkIotaFragExpr
     "ZoneDistillationSpecklike"
     [pattern| SOUTH_EAST qqqqqwdeddwqde |]
     [[t|'[IotaNumber, IotaVector] -> '[IotaList IotaEntity]|]]
     -- ['number', 'vector'] -> ['list of entities']
 )

$( mkIotaFragExpr
     "Confetti"
     [pattern| EAST awddeqaedd |]
     [ [t|'[IotaNumber, IotaVector] -> '[]|]
     , [t|'[IotaVector, IotaVector] -> '[]|]
     ]
     -- ['number/vector', 'vector'] -> ['']
 )

$( mkIotaFragExpr
     "Sparkle"
     [pattern| NORTH_EAST dqa |]
     [[t|'[IotaNumber, IotaVector, IotaVector] -> '[]|]]
     -- ['number', 'vector', 'vector'] -> ['']
 )

$( mkIotaFragExpr
     "PingBlock"
     [pattern| NORTH_EAST dwwdwwdwewdwwdwwdeq |]
     [[t|'[IotaNumber, IotaVector, IotaVector] -> '[]|]]
     -- ['number', 'vector', 'vector'] -> ['']
 )

$( mkIotaFragExpr
     "CrackDevice"
     [pattern| EAST wwaqqqqqeqdedwqeaeqwdedwqeaeq |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

$( mkIotaFragExpr
     "Construct"
     [pattern| NORTH_WEST wewewewewewdwew |]
     [[t|'[IotaVector, IotaVector, IotaVector, IotaIdentifier] -> '[]|]]
     -- ['vec', 'vec', 'vec', 'identifier'] -> ['']
 )

$( mkIotaFragExpr
     "ConjureFlower"
     [pattern| NORTH_EAST weqqqqqwaeaeaeaeaea |]
     [[t|'[IotaIdentifier, IotaVector] -> '[]|]]
     -- ['identifer', 'vector'] -> ['']
 )

$( mkIotaFragExpr
     "Illuminate"
     [pattern| SOUTH_EAST aeaeaeaeaeawqqqqq |]
     [[t|'[IotaNumber, IotaVector] -> '[]|]]
     -- ['number', 'vector'] -> ['']
 )

$( mkIotaFragExpr
     "Gasp"
     [pattern| NORTH_WEST aweeeeewaweeeee |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['entity'] -> ['']
 )

$( mkIotaFragExpr
     "Squawk"
     [pattern| NORTH_EAST wweedadw |]
     [[t|'[IotaIdentifier, IotaVector] -> '[]|]]
     -- ['identifier', 'vector'] -> ['']
 )

$( mkIotaFragExpr
     "ConjureEgg"
     [pattern| SOUTH_EAST qqqwaqaaqeeewdedde |]
     [[t|'[IotaVector] -> '[IotaEntity]|]]
     -- ['vector'] -> ['entity']
 )

$( mkIotaFragExpr
     "ConjureSpit"
     [pattern| EAST dwqaqw |]
     [[t|'[IotaVector] -> '[IotaEntity]|]]
     -- ['vector'] -> ['entity']
 )

$( mkIotaFragExpr
     "ConjureSnowball"
     [pattern| NORTH_EAST ddeeeeewd |]
     [[t|'[IotaVector] -> '[IotaEntity]|]]
     -- ['vector'] -> ['entity']
 )

$( mkIotaFragExpr
     "ConjureFireball"
     [pattern| SOUTH_EAST wqqqqqwaeaeaeaeae |]
     [[t|'[IotaVector] -> '[IotaEntity]|]]
     -- ['vector'] -> ['entity']
 )

$( mkIotaFragExpr
     "FoggySkysNadir"
     [pattern| SOUTH_EAST wddwaqqeawaeqwa |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

$( mkGreatIotaFragExpr
     "ClearSkysZenith"
     "Clear Sky's Zenith"
     [pattern| SOUTH_EAST wdwdqeeeeedwqwddwq |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

$( mkIotaFragExpr
     "HexGummy"
     [pattern| SOUTH_WEST eeewdw |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

$( mkIotaFragExpr
     "ConjureHexburst"
     [pattern| EAST aadaadqaq |]
     [[t|forall a. '[a, IotaVector] -> '[]|]]
     -- ['any', 'vector'] -> ['']
 )

$( mkIotaFragExpr
     "ConjureHextito"
     [pattern| EAST qaqdqaqdwawaw |]
     [[t|'[IotaList IotaPattern, IotaVector] -> '[]|]]
     -- ['list of patterns', 'vector'] -> ['']
 )

$( mkIotaFragExpr
     "ConjureCompass"
     [pattern| SOUTH_WEST aqwawqwqqwqwq |]
     [[t|'[IotaVector, IotaVector] -> '[]|]]
     -- ['vector', 'vector'] -> ['']
 )

$( mkIotaFragExpr
     "ConjureSpike"
     [pattern| NORTH_EAST qdqdqdqdww |]
     [[t|'[IotaNumber, IotaVector, IotaVector] -> '[]|]]
     -- ['number', 'vector', 'vector'] -> ['']
 )

$( mkIotaFragExpr
     "ConjureTchotchke"
     [pattern| NORTH_EAST wwwwwaqqqqqeaqeaeaeaeaeq |]
     [[t|'[IotaAnyList, IotaNumber, IotaNumber, IotaVector] -> '[]|]]
     -- ['list', 'num', 'num', 'vec'] -> ['']
 )

$( mkIotaFragExpr
     "TchotchkeGambit"
     [pattern| NORTH_EAST waqqqqqedeqdqdqdqdqe |]
     [[t|forall a. '[a] -> '[]|]]
     -- ['any'] -> ['']
 )

$( mkIotaFragExpr
     "TchotchkeReflection"
     [pattern| NORTH_EAST waqqqqqeaqeaeaeaeaeq |]
     [[t|'[] -> '[IotaAny]|]]
     -- [''] -> ['any']
 )

$( mkIotaFragExpr
     "Dispense"
     [pattern| SOUTH_WEST wqwawqwddaeeead |]
     [[t|'[IotaVector, IotaVector, IotaEntity] -> '[]|]]
     -- ['vector', 'vector', 'item entity'] -> ['']
 )

$( mkIotaFragExpr
     "Cook"
     [pattern| SOUTH_EAST qwqqadadadewewewe |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['item entity'] -> ['']
 )

$( mkIotaFragExpr
     "Roast"
     [pattern| NORTH_WEST aqqwwqqawdadedad |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['item entity'] -> ['']
 )

$( mkIotaFragExpr
     "Smoke"
     [pattern| SOUTH_EAST qwqqadadadewdqqdwe |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['item entity'] -> ['']
 )

$( mkIotaFragExpr
     "Blast"
     [pattern| SOUTH_EAST qwqqadadadewweewwe |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['item entity'] -> ['']
 )

$( mkIotaFragExpr
     "CutStone"
     [pattern| EAST qqqqqwaeaeaeaeaeadawa |]
     [[t|'[IotaIdentifier, IotaEntity] -> '[]|]]
     -- ['identifier', 'item entity'] -> ['']
 )

$( mkIotaFragExpr
     "Deposit"
     [pattern| SOUTH_EAST qwawqwaeqqq |]
     [[t|'[IotaVector, IotaVector, IotaEntity] -> '[]|]]
     -- ['vector', 'vector', 'item entity'] -> ['']
 )

$( mkIotaFragExpr
     "Withdraw"
     [pattern| SOUTH_WEST qqqeawqwawq |]
     [[t|'[IotaVector, IotaVector, IotaVector] -> '[IotaEntity]|]]
     -- ['vec', 'vec', 'vec'] -> ['entity | null']
 )

$( mkIotaFragExpr
     "Displace"
     [pattern| NORTH_EAST qaqqqqeedaqqqa |]
     [[t|'[IotaVector, IotaEntity] -> '[]|]]
     -- ['vector', 'entity'] -> ['']
 )

$( mkIotaFragExpr
     "Inculcate"
     [pattern| EAST wwaqqqqqeqdedwwqwqwwdedwwqwqw |]
     [[t|'[IotaList IotaPattern] -> '[]|]]
     -- ['list of patterns'] -> ['']
 )

$( mkIotaFragExpr
     "EvocationReflection"
     [pattern| EAST wwdeeeeeqeaqawwewewwaqawwewew |]
     [[t|'[] -> '[IotaList IotaPattern]|]]
     -- [''] -> ['list of patterns']
 )

$( mkIotaFragExpr
     "EvokerReflection"
     [pattern| EAST wwaqqqqqeeaqawwewewwaqawwewew |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

$( mkIotaFragExpr
     "DeploySentinels"
     [pattern| EAST aeaae |]
     [[t|'[IotaList IotaVector] -> '[]|]]
     -- ['list of vectors'] -> ['']
 )

$( mkIotaFragExpr
     "InfiltrationReflection"
     [pattern| WEST dqddq |]
     [[t|'[] -> '[IotaList IotaVector]|]]
     -- [''] -> ['list of vectors']
 )

$( mkIotaFragExpr
     "SimulateFirework"
     [pattern| SOUTH_WEST dedwaqwqqwqa |]
     [[t|'[IotaNumber, IotaVector, IotaVector] -> '[]|]]
     -- ['number', 'vector', 'vector'] -> ['']
 )

$( mkIotaFragExpr
     "ConjureFirework"
     [pattern| SOUTH_WEST dedwaqwwawwqa |]
     [ [t|
         '[IotaBoolean, IotaBoolean, IotaList IotaDye, IotaList IotaDye, IotaNumber, IotaNumber, IotaVector, IotaVector] -> '[]
         |]
     ]
     -- ['bool', 'bool', 'list of dyes', 'list of dyes', 'num', 'num', 'vec', 'vec'] -> ['']
 )

$( mkIotaFragExpr
     "ClearVision"
     [pattern| WEST eeeeeqaqeeeee |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

$( mkIotaFragExpr
     "PierceDarkness"
     [pattern| WEST edewawede |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

$( mkIotaFragExpr
     "VisualizeForms"
     [pattern| WEST eedwwawwdee |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

$( mkIotaFragExpr
     "BroadcastVision"
     [pattern| WEST wewdwewwawwewdwew |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

$( mkIotaFragExpr
     "IdentifyImportance"
     [pattern| WEST eewdweqaqewdwee |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

$( mkIotaFragExpr
     "SplitVision"
     [pattern| NORTH_EAST qaqdedaedqqdedaqaedeqd |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

$( mkIotaFragExpr
     "Summon"
     [pattern| EAST wedqawqeewdeaqeewdeaqqedqawqqedqawqeedqawqqewdeaqeedqawqeewdeaqqewdeaqeewdeaqeedqawqqedqawqqewdeaqeedqawqeewdeaqqewdeaqeewdeaqeedqawqqedqawqqewdeaqqedqawqeewdeaqeewdeaqqedqawqqedqawqeedqawqqewdeaqqedqawqeewdeaqeewdeaqqedqawqqedqawqeedqawqqewdeaqeedqawqeewdeaqeewdeaqqedqawqqedqawqeedqawqqewdeaqqedqawqeewdeaqqewdeaqeewdeaqeedqawqqedqawqqewdeaqe |]
     [[t|'[IotaVector] -> '[IotaEntity]|]]
     -- ['vector'] -> ['entity']
 )

$( mkGreatIotaFragExpr
     "GreaterBlink"
     "Greater Blink"
     [pattern| SOUTH_WEST wqawawaqwqwqawawaqw |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

$( mkGreatIotaFragExpr
     "ConjureMesh"
     "Conjure Mesh"
     [pattern| EAST qaqqqqqwqqqdeeweweeaeewewee |]
     [[t|'[IotaVector] -> '[IotaEntity]|]]
     -- ['vector'] -> ['entity']
 )

$( mkIotaFragExpr
     "WeaveMesh"
     [pattern| EAST qaqqqqqwqqqdeewewee |]
     [[t|'[IotaList IotaVector, IotaEntity] -> '[]|]]
     -- ['list of vectors', 'entity'] -> ['']
 )

$( mkIotaFragExpr
     "TanglePurification"
     [pattern| SOUTH_WEST edeeeeeweeeaqqwqwqq |]
     [[t|'[IotaEntity] -> '[IotaList IotaVector]|]]
     -- ['entity'] -> ['list of vectors']
 )
