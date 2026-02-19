{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Haskcasting.Patterns.Hexical where

import Data.Sequence qualified as Seq
import Haskcasting.Fragment (Fragment (Fragment))
import Haskcasting.Iota (
  IotaAny,
  IotaBoolean,
  IotaCast (iotaCast),
  IotaEntity,
  IotaExec,
  IotaList,
  IotaNumber,
  IotaPattern (IotaPattern),
  IotaVector,
 )
import Haskcasting.Iota.Hexical (IotaDye, IotaPigment)
import Haskcasting.Pattern (pattern)
import Haskcasting.Patterns.TH (mkGreatIotaFrag, mkIotaFrag)

type IotaIdentifier = IotaAny

$( mkIotaFrag
     "WriteGrimoire"
     [pattern| WEST aqwqaeaqa |]
     [[t|Fragment '[IotaList IotaPattern, IotaPattern] '[]|]]
 )

$( mkIotaFrag
     "EraseGrimoire"
     [pattern| WEST aqwqaqded |]
     [[t|Fragment '[IotaPattern] '[]|]]
 )

$( mkIotaFrag
     "ArchivistReflection"
     [pattern| SOUTH_EAST aqaeaqwqa |]
     [[t|Fragment '[] '[IotaList IotaPattern]|]]
 )

$( mkIotaFrag
     "AgeScroll"
     [pattern| EAST waeqqqqeqqqwqeaeaeaeq |]
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "DyeInk"
     [pattern| EAST waeqqqqewqqwqqeqeqqwqqeq |]
     [[t|Fragment '[IotaVector, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "IlluminateInk"
     [pattern| EAST waeqqqqedeqdqdqdqeqdwwd |]
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "VanishScroll"
     [pattern| EAST waeqqqqedeqeeweeqewee |]
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "Wish"
     [pattern| NORTH_WEST eweweweweweewedeaqqqd |]
     [[t|Fragment '[IotaList IotaPattern] '[]|]]
 )

$( mkIotaFrag
     "GenieReflectionSpatial"
     [pattern| SOUTH_WEST qwddedqdd |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFrag
     "GenieReflectionRotational"
     [pattern| SOUTH_WEST qwddedadw |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFrag
     "GenieReflectionKinetic"
     [pattern| SOUTH_WEST qwddedqew |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFrag
     "GenieReflectionTemporal"
     [pattern| SOUTH_WEST qwddedqwddwa |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "GenieReflectionMedia"
     [pattern| SOUTH_WEST qwddedaeeeee |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "GenieGambit"
     [pattern| SOUTH_WEST qwddedqedeeeee |]
     [[t|forall a. Fragment '[a] '[]|]]
 )

$( mkIotaFrag
     "GenieReflectionMemory"
     [pattern| SOUTH_WEST qwddedqwaqqqqq |]
     [[t|Fragment '[] '[IotaAny]|]]
 )

$( mkIotaFrag
     "FinaleReflection"
     [pattern| EAST aaddaddad |]
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "MakeGenie"
     [pattern| EAST qaqwawqwqqwqwqwqwqwqq |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "PromoteLamp"
     [pattern| WEST qweedeqeedeqdqdwewewwewewwewe |]
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "ArchgeniePurification"
     [pattern| NORTH_EAST qaqwddedqeed |]
     [[t|Fragment '[IotaEntity] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "ArchgenieReflectionSpatial"
     [pattern| NORTH_EAST qaqwddedqdd |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFrag
     "ArchgenieReflectionRotational"
     [pattern| NORTH_EAST qaqwddedadw |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFrag
     "ArchgenieReflectionKinetic"
     [pattern| NORTH_EAST qaqwddedqew |]
     [[t|Fragment '[] '[IotaVector]|]]
 )

$( mkIotaFrag
     "ArchgenieReflectionTemporal"
     [pattern| NORTH_EAST qaqwddedqwddwa |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "ArchgenieReflectionMedia"
     [pattern| NORTH_EAST qaqwddedaeeeee |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "ArchgenieGambit"
     [pattern| NORTH_EAST qaqwddedqedeeeee |]
     [[t|forall a. Fragment '[a] '[]|]]
 )

$( mkIotaFrag
     "ArchgenieReflectionMemory"
     [pattern| NORTH_EAST qaqwddedqwaqqqqq |]
     [[t|Fragment '[] '[IotaAny]|]]
 )

$( mkIotaFrag
     "EpiphanyPurification"
     [pattern| SOUTH_EAST awqaqqq |]
     [[t|Fragment '[IotaEntity] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "SentiencePurification"
     [pattern| SOUTH_EAST qqqaqqq |]
     [[t|Fragment '[IotaEntity] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "GarbageReflection"
     [pattern| EAST aqawde |]
     [[t|Fragment '[] '[IotaAny]|]]
 )

$( mkIotaFrag
     "ReflectingGambit"
     [pattern| NORTH_EAST ddwqaq |]
     [[t|forall a b c. Fragment '[a, b, c] '[c, b, a]|]]
 )

$( mkIotaFrag
     "BubblingGambit"
     [pattern| EAST aawede |]
     [[t|forall a b c. Fragment '[a, b, c] '[a, c, b]|]]
 )

iotaDioscuriGambitII :: IotaPattern
iotaDioscuriGambitII = IotaPattern [pattern| EAST waadadaa |]

$( mkIotaFrag
     "CongruenceDistillation"
     [pattern| EAST aaqd |]
     [[t|Fragment '[IotaPattern, IotaPattern] '[IotaBoolean]|]]
 )

-- yeah these two are swapped the docs are wrong
$( mkIotaFrag
     "ChirographersPurification"
     [pattern| EAST wqaedeqd |]
     -- [[t|Fragment '[IotaPattern] '[IotaList IotaNumber]|]]
     [[t|Fragment '[IotaList IotaNumber] '[IotaPattern]|]]
 )

$( mkIotaFrag
     "CalligraphersPurification"
     [pattern| EAST wqqqaqwd |]
     -- [[t|Fragment '[IotaList IotaNumber] '[IotaPattern]|]]
     [[t|Fragment '[IotaPattern] '[IotaList IotaNumber]|]]
 )

$( mkIotaFrag
     "HandwritingDistillation"
     [pattern| NORTH_EAST eadqqqa |]
     [[t|Fragment '[IotaPattern] '[IotaList IotaVector]|]]
 )

$( mkIotaFrag
     "GlyphmakersDistillation"
     [pattern| NORTH_EAST aqqqdae |]
     [[t|Fragment '[IotaNumber, IotaPattern] '[IotaPattern]|]]
 )

$( mkIotaFrag
     "PerlinDistillation"
     [pattern| WEST qawedqdq |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "NavalDistillation"
     [pattern| EAST wqqaqwede |]
     [[t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]]
 )

$( mkIotaFrag
     "LilypadDistillation"
     [pattern| EAST weedewqaq |]
     [[t|Fragment '[IotaVector, IotaVector] '[IotaVector]|]]
 )

$( mkIotaFrag
     "RailgunExaltation"
     [pattern| EAST wqqddqeqddq |]
     [[t|Fragment '[IotaIdentifier, IotaVector, IotaVector] '[IotaVector]|]]
 )

$( mkIotaFrag
     "LaserExaltation"
     [pattern| EAST weeaaeqeaae |]
     [[t|Fragment '[IotaIdentifier, IotaVector, IotaVector] '[IotaVector]|]]
 )

$( mkIotaFrag
     "TelepathicReflection"
     [pattern| EAST wqqadaw |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "SendThought"
     [pattern| EAST qqqqwaqa |]
     [[t|forall a. Fragment '[a] '[]|]]
 )

$( mkIotaFrag
     "ShoutThought"
     [pattern| EAST daqqqqwa |]
     [[t|forall a. Fragment '[a] '[]|]]
 )

$( mkIotaFrag
     "HallucinatePling"
     [pattern| NORTH_EAST eqqqada |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "HallucinateClick"
     [pattern| NORTH_EAST eqqadaq |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "ChargeReflection"
     [pattern| SOUTH_EAST aqaddq |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "RetreatReflection"
     [pattern| SOUTH_WEST dedwdq |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "DodgeReflection"
     [pattern| SOUTH_EAST edead |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "EvadeReflection"
     [pattern| SOUTH_WEST qaqda |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "LeapingReflection"
     [pattern| SOUTH_WEST qaqdaqqa |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "StealthyReflection"
     [pattern| NORTH_WEST wede |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "SorobanReflection"
     [pattern| NORTH_EAST wdeaqq |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "SorobanReflectionII"
     [pattern| SOUTH_EAST waqdee |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "SorobanGambit"
     [pattern| NORTH_EAST qdeeaae |]
     [[t|Fragment '[] '[]|]]
 )

iotaJanusGambit :: IotaPattern
iotaJanusGambit = IotaPattern [pattern| SOUTH_WEST aadee |]

fragJanusGambit :: Fragment as bs
fragJanusGambit = Fragment $ Seq.singleton $ iotaCast iotaJanusGambit

iotaAtalantasGambit :: IotaPattern
iotaAtalantasGambit = IotaPattern [pattern| SOUTH_WEST aqdea |]

$( mkIotaFrag
     "CastorsGambit"
     [pattern| NORTH_WEST adadee |]
     [[t|forall a. Fragment '[a] '[]|]]
 )

$( mkIotaFrag
     "PolluxsGambit"
     [pattern| NORTH_EAST dadaqq |]
     [[t|forall a. Fragment '[a] '[]|]]
 )

iotaSisyphusGambit :: IotaPattern
iotaSisyphusGambit = IotaPattern [pattern| NORTH_EAST qaqwede |]

$( mkIotaFrag
     "ThemisGambit"
     [pattern| WEST dwaad |]
     []
 )

instance FragThemisGambit (IotaExec (a ': s) (IotaNumber ': s') ': IotaList a ': s) (IotaList a ': s)

$( mkIotaFrag
     "TutusGambit"
     [pattern| WEST eedqa |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "LibrariansPurification"
     [pattern| EAST qaqqadaq |]
     [[t|Fragment '[IotaVector] '[IotaPattern]|]]
 )

$( mkIotaFrag
     "LibrariansPurificationII"
     [pattern| EAST qaqqqada |]
     [[t|Fragment '[IotaVector] '[IotaAny]|]]
 )

$( mkIotaFrag
     "LibrariansGambit"
     [pattern| SOUTH_WEST edeeedad |]
     [[t|forall a. Fragment '[a, IotaPattern, IotaVector] '[]|]]
 )

$( mkIotaFrag
     "LibrariansGambitII"
     [pattern| SOUTH_WEST edeedade |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "ConjureMageBlock"
     [pattern| NORTH_WEST dee |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "Bouncy"
     [pattern| NORTH_WEST deeqa |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "Energized"
     [pattern| NORTH_WEST deewad |]
     [[t|Fragment '[IotaNumber, IotaVector] '[]|]]
 )

$( mkIotaFrag
     "Ephemeral"
     [pattern| NORTH_WEST deewwaawd |]
     [[t|Fragment '[IotaNumber, IotaVector] '[]|]]
 )

$( mkIotaFrag
     "Invisible"
     [pattern| NORTH_WEST deeqedeaqqqwqqq |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "Replaceable"
     [pattern| NORTH_WEST deewqaqqqqq |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "Volatile"
     [pattern| NORTH_WEST deewedeeeee |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "ChromaticPurification"
     [pattern| NORTH_EAST weedwa |]
     [ [t|Fragment '[IotaVector] '[IotaDye]|]
     , [t|Fragment '[IotaEntity] '[IotaDye]|]
     , [t|Fragment '[IotaIdentifier] '[IotaDye]|]
     ]
 )

$( mkIotaFrag
     "Dye"
     [pattern| NORTH_WEST dwaqqw |]
     [ [t|Fragment '[IotaDye, IotaVector] '[]|]
     , [t|Fragment '[IotaDye, IotaEntity] '[]|]
     ]
 )

$( mkIotaFrag
     "VisionPurification"
     [pattern| EAST wdwwaawwewdwwewwdwwe |]
     [[t|Fragment '[IotaDye] '[IotaVector]|]]
 )

$( mkIotaFrag
     "MagicMissile"
     [pattern| WEST qaqww |]
     [[t|Fragment '[IotaVector, IotaVector] '[]|]]
 )

$( mkIotaFrag
     "PigmentPurification"
     [pattern| NORTH_WEST aqwedeweeeewweeew |]
     [ [t|Fragment '[IotaEntity] '[IotaPigment]|]
     , [t|Fragment '[IotaDye] '[IotaPigment]|]
     ]
 )

$( mkIotaFrag
     "PigmentExaltation"
     [pattern| SOUTH_EAST edewqaqqqqqwqqq |]
     [[t|Fragment '[IotaNumber, IotaVector, IotaPigment] '[IotaVector]|]]
 )

$( mkIotaFrag
     "InternalizePigmentII"
     [pattern| EAST weeeweeqeeeewqaqweeee |]
     [[t|Fragment '[IotaPigment] '[]|]]
 )

$( mkIotaFrag
     "Prestidigitation"
     [pattern| NORTH_EAST wedewedew |]
     [ [t|Fragment '[IotaVector] '[]|]
     , [t|Fragment '[IotaEntity] '[]|]
     ]
 )

$( mkIotaFrag
     "AlterationPurification"
     [pattern| NORTH_WEST wqaqwqaqw |]
     [ [t|Fragment '[IotaVector] '[IotaBoolean]|]
     , [t|Fragment '[IotaEntity] '[IotaBoolean]|]
     ]
 )

$( mkIotaFrag
     "Wristpocket"
     [pattern| WEST aaqqa |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "PocketReflection"
     [pattern| WEST aaqqada |]
     [[t|Fragment '[] '[IotaIdentifier]|]]
 )

$( mkIotaFrag
     "PocketReflectionII"
     [pattern| WEST aaqqaaw |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "Sleight"
     [pattern| WEST aaqqadeeeq |]
     [ [t|Fragment '[IotaVector] '[]|]
     , [t|Fragment '[IotaEntity] '[]|]
     ]
 )

$( mkIotaFrag
     "MageHand"
     [pattern| WEST aaqqaeea |]
     [ [t|Fragment '[IotaVector] '[]|]
     , [t|Fragment '[IotaEntity] '[]|]
     ]
 )

$( mkIotaFrag
     "MageMouth"
     [pattern| WEST aaqqqadaa |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "ConjureSpeck"
     [pattern| SOUTH_WEST ade |]
     [[t|forall a. Fragment '[IotaVector, IotaVector, a] '[IotaEntity]|]]
 )

$( mkIotaFrag
     "MoveSpeck"
     [pattern| SOUTH_WEST adeqaa |]
     [[t|Fragment '[IotaVector, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "RotateSpeck"
     [pattern| SOUTH_WEST adeaw |]
     [[t|Fragment '[IotaVector, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "RollSpeck"
     [pattern| SOUTH_WEST adeqqqqq |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "AlterSpeck"
     [pattern| SOUTH_WEST adeeaqa |]
     [[t|forall a. Fragment '[a, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "TimeSpeck"
     [pattern| SOUTH_WEST adeqqaawdd |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "ResizeSpeck"
     [pattern| SOUTH_WEST adeeqed |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "ThickenSpeck"
     [pattern| SOUTH_WEST adeeqw |]
     [[t|Fragment '[IotaNumber, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "PaintSpeck"
     [pattern| SOUTH_WEST adeqqaq |]
     [[t|Fragment '[IotaPigment, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "DismissSpeck"
     [pattern| SOUTH_WEST adeaqde |]
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "ZoneDistillationSpecklike"
     [pattern| SOUTH_EAST qqqqqwdeddwqde |]
     [[t|Fragment '[IotaNumber, IotaVector] '[IotaList IotaEntity]|]]
 )

$( mkIotaFrag
     "Autograph"
     [pattern| NORTH_EAST wwqqqqq |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "ConjureHexburst"
     [pattern| EAST aadaadqaq |]
     [[t|forall a. Fragment '[a, IotaVector] '[]|]]
 )

$( mkIotaFrag
     "ConjureHextito"
     [pattern| EAST qaqdqaqdwawaw |]
     [[t|forall as bs. Fragment '[IotaExec as bs, IotaVector] '[]|]]
 )

$( mkIotaFrag
     "ConjureFireball"
     [pattern| SOUTH_EAST wqqqqqwaeaeaeaeae |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "Spit"
     [pattern| EAST dwqaqw |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkIotaFrag
     "Gasp"
     [pattern| NORTH_WEST aweeeeewaweeeee |]
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "Myodesopsia"
     [pattern| SOUTH_EAST wadawadawawaaw |]
     [[t|Fragment '[IotaList IotaPattern] '[]|]]
 )

$( mkIotaFrag
     "ConjureCompass"
     [pattern| SOUTH_WEST aqwawqwqqwqwq |]
     [[t|Fragment '[IotaVector, IotaVector] '[]|]]
 )

$( mkIotaFrag
     "ConjureSpike"
     [pattern| NORTH_EAST qdqdqdqdww |]
     [[t|Fragment '[IotaNumber, IotaVector, IotaVector] '[]|]]
 )

$( mkIotaFrag
     "ConjureTchotchke"
     [pattern| NORTH_EAST wwwwwaqqqqqeaqeaeaeaeaeq |]
     []
     -- [[t|forall a. Fragment '[IotaList a, IotaNumber, IotaNumber, IotaVector] '[]|]]
 )

$( mkIotaFrag
     "TchotchkeGambit"
     [pattern| NORTH_EAST waqqqqqedeqdqdqdqdqe |]
     [[t|forall a. Fragment '[a] '[]|]]
 )

$( mkIotaFrag
     "TchotchkeReflection"
     [pattern| NORTH_EAST waqqqqqeaqeaeaeaeaeq |]
     [[t|Fragment '[] '[IotaAny]|]]
 )

$( mkIotaFrag
     "Displace"
     [pattern| NORTH_EAST qaqqqqeedaqqqa |]
     [[t|Fragment '[IotaVector, IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "Inculcate"
     [pattern| EAST wwaqqqqqeqdedwwqwqwwdedwwqwqw |]
     [[t|forall bs. Fragment '[IotaExec '[] bs] '[]|]]
 )

$( mkIotaFrag
     "EvokerReflection"
     [pattern| EAST wwaqqqqqeeaqawwewewwaqawwewew |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "SimulateFirework"
     [pattern| SOUTH_WEST dedwaqwqqwqa |]
     [[t|Fragment '[IotaNumber, IotaVector, IotaVector] '[]|]]
 )

$( mkIotaFrag
     "ConjureFirework"
     [pattern| SOUTH_WEST dedwaqwwawwqa |]
     [ [t|
         Fragment
           '[IotaBoolean, IotaBoolean, IotaList IotaDye, IotaList IotaDye, IotaNumber, IotaNumber, IotaVector, IotaVector]
           '[]
         |]
     ]
 )

$( mkIotaFrag
     "ClearVision"
     [pattern| WEST eeeeeqaqeeeee |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "PierceDarkness"
     [pattern| WEST edewawede |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "VisualizeForms"
     [pattern| WEST eedwwawwdee |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "BroadcastVision"
     [pattern| WEST wewdwewwawwewdwew |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "IdentifyImportance"
     [pattern| WEST eewdweqaqewdwee |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "SplitVision"
     [pattern| NORTH_EAST qaqdedaedqqdedaqaedeqd |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "MinersPurification"
     [pattern| EAST qaqqqqqeeeeedq |]
     [[t|Fragment '[IotaIdentifier] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "DemomansPurification"
     [pattern| EAST qaqqqqqewaawaawa |]
     [[t|Fragment '[IotaIdentifier] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "PlumbersPurification"
     [pattern| SOUTH_EAST edeeeeeqwqqqqw |]
     [[t|Fragment '[IotaVector] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "OrientationPurification"
     [pattern| EAST qaqqqqqqwadeeed |]
     [[t|Fragment '[IotaVector] '[IotaVector]|]]
 )

$( mkIotaFrag
     "FarmersPurification"
     [pattern| EAST qaqqqqqwaea |]
     [[t|Fragment '[IotaVector] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "GlowingPurification"
     [pattern| EAST qaqqqqqwaeaeaeaeaea |]
     [[t|Fragment '[IotaVector] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "LockPurification"
     [pattern| EAST qaqqqeaqwdewd |]
     [[t|Fragment '[IotaVector] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "AnglePurification"
     [pattern| EAST qaqqqqqqwqqwqd |]
     [[t|Fragment '[IotaVector] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "BunchingPurification"
     [pattern| EAST qaqqqqqweeeeedeeqaqdeee |]
     [[t|Fragment '[IotaVector] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "BookPurification"
     [pattern| EAST qaqqqqqqeawa |]
     [[t|Fragment '[IotaVector] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "ThaumaturgistsPurification"
     [pattern| WEST waqeaeqawqwawaw |]
     [[t|Fragment '[IotaEntity] '[]|]]
 )

$( mkIotaFrag
     "CharmDistillation"
     [pattern| WEST waqwwqaweede |]
     [[t|Fragment '[IotaIdentifier, IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "CalipersPurification"
     [pattern| NORTH_WEST dwe |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "TheodolitePurification"
     [pattern| EAST wqaa |]
     [[t|Fragment '[IotaEntity] '[IotaVector]|]]
 )

$( mkIotaFrag
     "VitalityPurification"
     [pattern| SOUTH_EAST wddwaqqwawq |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "FitnessPurification"
     [pattern| SOUTH_EAST wddwwawaeqwawq |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "SuffocationPurification"
     [pattern| EAST wwaade |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "LungPurification"
     [pattern| EAST wwaadee |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "HungerPurification"
     [pattern| WEST qqqadaddw |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "StaminaPurification"
     [pattern| WEST qqqadaddq |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "InfernoPurification"
     [pattern| EAST qqwaqda |]
     [[t|Fragment '[IotaEntity] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "InfernoPurificationII"
     [pattern| WEST eewdead |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "EndermansPurification"
     [pattern| SOUTH_WEST qqqqwaadq |]
     [[t|Fragment '[IotaEntity] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "YouthPurification"
     [pattern| SOUTH_WEST awaqdwaaw |]
     [[t|Fragment '[IotaEntity] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "ReproductionPurification"
     [pattern| EAST awaaqdqaawa |]
     [[t|Fragment '[IotaEntity] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "SlothsPurification"
     [pattern| NORTH_WEST aqaew |]
     [[t|Fragment '[IotaEntity] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "RacersPurification"
     [pattern| WEST eaq |]
     [[t|Fragment '[IotaEntity] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "AmbitPurification"
     [pattern| EAST wawaw |]
     [ [t|Fragment '[IotaVector] '[IotaBoolean]|]
     , [t|Fragment '[IotaEntity] '[IotaBoolean]|]
     ]
 )

$( mkIotaFrag
     "StaffReflection"
     [pattern| NORTH_EAST waaq |]
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "DeviceReflection"
     [pattern| NORTH_EAST waaqwwaqqqqq |]
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

-- Note: Repeated name in source, appending "II"
$( mkIotaFrag
     "TchotchkeReflectionII"
     [pattern| NORTH_EAST waaqwwaqqqqqeaqeaeaeaeaeq |]
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "EvocationReflection"
     [pattern| NORTH_EAST waaqeaqa |]
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "WishReflection"
     [pattern| NORTH_EAST waaqdqdded |]
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "GrandWishReflection"
     [pattern| NORTH_EAST waaqqqaqwdd |]
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "ConstructedReflection"
     [pattern| NORTH_EAST waaqdeaqwqae |]
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "DexterityReflection"
     [pattern| NORTH_EAST qaqqqwaaq |]
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "EdibilityPurification"
     [pattern| WEST adaqqqdd |]
     [[t|Fragment '[IotaIdentifier] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "CaloriePurification"
     [pattern| WEST adaqqqddqe |]
     [[t|Fragment '[IotaIdentifier] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "SatiationPurification"
     [pattern| WEST adaqqqddqw |]
     [[t|Fragment '[IotaIdentifier] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "FleshPurification"
     [pattern| WEST adaqqqddaed |]
     [[t|Fragment '[IotaIdentifier] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "DessertPurification"
     [pattern| WEST adaqqqddaq |]
     [[t|Fragment '[IotaIdentifier] '[IotaBoolean]|]]
 )

$( mkIotaFrag
     "DetectivesPurification"
     [pattern| NORTH_EAST qqqqqe |]
     [[t|forall a. Fragment '[a] '[IotaIdentifier]|]]
 )

$( mkIotaFrag
     "RecognizersPurification"
     [pattern| WEST eeeeeq |]
     [[t|Fragment '[IotaEntity] '[IotaIdentifier]|]]
 )

$( mkIotaFrag
     "ClassifiersPurification"
     [pattern| WEST edqdeq |]
     [[t|forall a. Fragment '[a] '[IotaIdentifier]|]]
 )

$( mkIotaFrag
     "ToolPurification"
     [pattern| NORTH_EAST qaqqqq |]
     [[t|Fragment '[IotaEntity] '[IotaIdentifier]|]]
 )

$( mkIotaFrag
     "AccessoryPurification"
     [pattern| NORTH_WEST edeeee |]
     [[t|Fragment '[IotaEntity] '[IotaIdentifier]|]]
 )

$( mkIotaFrag
     "StoragePurification"
     [pattern| EAST qaqqwqqqw |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "WarehousePurification"
     [pattern| WEST edeeweeew |]
     [[t|Fragment '[IotaIdentifier] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "DeteriorationPurification"
     [pattern| NORTH_EAST eeweeewdeq |]
     [[t|Fragment '[IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "FragilityPurification"
     [pattern| NORTH_WEST qqwqqqwaqe |]
     [[t|Fragment '[IotaIdentifier] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "DiagnosisPurification"
     [pattern| SOUTH_WEST wqqq |]
     [[t|Fragment '[IotaEntity] '[IotaList IotaIdentifier]|]]
 )

$( mkIotaFrag
     "PrescriptionPurification"
     [pattern| SOUTH_WEST wqqqadee |]
     [[t|Fragment '[IotaEntity] '[IotaList IotaIdentifier]|]]
 )

$( mkIotaFrag
     "ConditionPurification"
     [pattern| SOUTH_WEST wqqqaawd |]
     [[t|Fragment '[IotaIdentifier] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "ConcentrationDistillation"
     [pattern| SOUTH_WEST wqqqaqwa |]
     [[t|Fragment '[IotaIdentifier, IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "ClearanceDistillation"
     [pattern| SOUTH_WEST wqqqaqwdd |]
     [[t|Fragment '[IotaIdentifier, IotaEntity] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "LuminancePurification"
     [pattern| SOUTH_WEST wqwqwqwqwqwaeqqqqaeqaeaeaeaw |]
     [[t|Fragment '[IotaVector] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "MeterologistsReflection"
     [pattern| WEST eweweweweweeeaedqdqde |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "BatteryPurification"
     [pattern| EAST qwqwqwqwqwqqwwaadwdaaww |]
     [[t|Fragment '[IotaVector] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "PeripheralPurification"
     [pattern| WEST eweweweweweewwddawaddww |]
     [[t|Fragment '[IotaVector] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "CircadianReflection"
     [pattern| SOUTH_EAST wwawwawwqqawwdwwdwwaqwqwqwqwq |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "TemporalReflection"
     [pattern| SOUTH_EAST wddwaqqwqaddaqqwddwaqqwqaddaq |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "GeographicalPurification"
     [pattern| WEST qwqwqawdqqaqqdwaqwqwq |]
     [[t|Fragment '[] '[IotaIdentifier]|]]
 )

$( mkIotaFrag
     "PlaneReflection"
     [pattern| WEST qwqwqwqwqwqqaedwaqd |]
     [[t|Fragment '[] '[IotaIdentifier]|]]
 )

$( mkIotaFrag
     "DistortionReflection"
     [pattern| SOUTH_WEST aqwawqwqqwqwqwqwqwq |]
     [[t|Fragment '[] '[IotaBoolean]|]]
 )

$( mkGreatIotaFrag
     "GreaterBlink"
     "Greater Blink"
     [pattern| SOUTH_WEST wqawawaqwqwqawawaqw |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

$( mkGreatIotaFrag
     "ConjureMesh"
     "Conjure Mesh"
     [pattern| EAST qaqqqqqqwqqqdeeweweeaeewewee |]
     [[t|Fragment '[IotaVector] '[IotaEntity]|]]
 )

$( mkGreatIotaFrag
     "WeaveMesh"
     "Weave Mesh"
     [pattern| EAST qaqqqqqqwqqqdeewewee |]
     [[t|Fragment '[IotaList IotaVector, IotaEntity] '[]|]]
 )

$( mkGreatIotaFrag
     "TanglePurification"
     "Tangle Purification"
     [pattern| SOUTH_WEST edeeeeeweeeaqqwqwqq |]
     [[t|Fragment '[IotaEntity] '[IotaList IotaVector]|]]
 )
