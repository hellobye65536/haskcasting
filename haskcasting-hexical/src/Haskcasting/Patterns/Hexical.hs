{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Haskcasting.Patterns.Hexical where

import Haskcasting.ExprLang.TH (mkFragExprInstance, mkGreatIotaFragExpr, mkIotaFragExpr)
import Haskcasting.Fragment (Fragment)
import Haskcasting.Iota (
  IotaAny,
  IotaAnyList,
  IotaBoolean,
  IotaEntity,
  IotaList,
  IotaNull,
  IotaNumber,
  IotaPattern,
  IotaVector,
 )
import Haskcasting.Pattern (pattern)

import Haskcasting.Iota.Hexical (IotaDye, IotaPigment)
import Haskcasting.Iota.Hexpose (IotaIdentifier, IotaItem)
import Haskcasting.Patterns.Hexcasting

--- Items

-- Driver Dots

$(mkIotaFragExpr "ProgramDriver" [pattern| NORTH_WEST aqqqqaw |] [[t|Fragment '[IotaAnyList, IotaPattern] '[]|]])

-- Grimoires

$(mkIotaFragExpr "WriteGrimoire" [pattern| WEST aqwqaeaqa |] [[t|Fragment '[IotaAnyList, IotaPattern] '[]|]])
$(mkIotaFragExpr "EraseGrimoire" [pattern| WEST aqwqaqded |] [[t|Fragment '[IotaPattern] '[]|]])
$(mkIotaFragExpr "ArchivistReflection" [pattern| SOUTH_EAST aqaeaqwqa |] [[t|Fragment '[] '[IotaList IotaPattern]|]])

-- Periwinkle

$(mkIotaFragExpr "InduceDigging" [pattern| EAST wwwaqdadaadadqqqeaeq |] [[t|Fragment '[IotaEntity] '[]|]])

-- Genie Lamps

$(mkIotaFragExpr "Wish" [pattern| NORTH_WEST eweweweweweewedeaqqqd |] [[t|Fragment '[IotaNumber, IotaAnyList] '[]|]])
$(mkIotaFragExpr "GenieReflectionSpatial" [pattern| NORTH_EAST qaqwddedqdd |] [[t|Fragment '[] '[IotaVector]|]])
$(mkIotaFragExpr "GenieReflectionRotational" [pattern| NORTH_EAST qaqwddedadw |] [[t|Fragment '[] '[IotaVector]|]])
$(mkIotaFragExpr "GenieReflectionKinetic" [pattern| NORTH_EAST qaqwddedqew |] [[t|Fragment '[] '[IotaVector]|]])
$(mkIotaFragExpr "GenieReflectionTemporal" [pattern| NORTH_EAST qaqwddedqwddwa |] [[t|Fragment '[] '[IotaNumber]|]])
$(mkIotaFragExpr "GenieReflectionMedia" [pattern| NORTH_EAST qaqwddedaeeeee |] [[t|Fragment '[] '[IotaNumber]|]])
$(mkIotaFragExpr "GenieGambit" [pattern| NORTH_EAST qaqwddedqedeeeee |] [[t|forall a. Fragment '[a] '[]|]])
$(mkIotaFragExpr "GenieReflectionMemory" [pattern| NORTH_EAST qaqwddedqwaqqqqq |] [[t|Fragment '[] '[IotaAny]|]])
$(mkIotaFragExpr "FinaleReflection" [pattern| EAST aaddaddad |] [[t|Fragment '[] '[IotaBoolean]|]])

-- Pattern Manipulation

$( mkFragExprInstance
     "LengthPurification"
     [[t|Fragment '[IotaPattern] '[IotaNumber]|]]
 )
$( mkIotaFragExpr
     "CongruenceDistillation"
     [pattern| EAST aaqd |]
     [[t|Fragment '[IotaPattern, IotaPattern] '[IotaBoolean]|]]
 )
$( mkIotaFragExpr
     "SimilarityDistillation"
     [pattern| EAST aedd |]
     [[t|Fragment '[IotaPattern, IotaPattern] '[IotaBoolean]|]]
 )
$( mkFragExprInstance
     "AdditiveDistillation"
     [[t|Fragment '[IotaPattern, IotaPattern] '[IotaPattern]|]]
 )
$( mkFragExprInstance
     "SubtractiveDistillation"
     [[t|Fragment '[IotaPattern, IotaPattern] '[IotaNumber]|]]
 )
$( mkFragExprInstance
     "MultiplicativeDistillation"
     [[t|Fragment '[IotaNumber, IotaPattern] '[IotaPattern]|]]
 )
$( mkFragExprInstance
     "DivisionDistillation"
     [[t|Fragment '[IotaNumber, IotaPattern] '[IotaPattern]|]]
 )
$( mkFragExprInstance
     "SelectionDistillation"
     [[t|Fragment '[IotaNumber, IotaPattern] '[IotaPattern]|]]
 )
$( mkFragExprInstance
     "ExcisorsDistillation"
     [[t|Fragment '[IotaNumber, IotaPattern] '[IotaPattern]|]]
 )
$( mkIotaFragExpr
     "ChirographersPurification"
     [pattern| EAST wqaedeqd |]
     [[t|Fragment '[IotaPattern] '[IotaList IotaPattern]|]]
 )
$( mkIotaFragExpr
     "CalligraphersPurification"
     [pattern| EAST wqqqaqwd |]
     [[t|Fragment '[IotaList IotaPattern] '[IotaPattern]|]]
 )
$( mkIotaFragExpr
     "GlyphmakersDistillation"
     [pattern| NORTH_EAST aqqqdae |]
     [[t|Fragment '[IotaNumber, IotaPattern] '[IotaPattern]|]]
 )

-- Grok

$(mkIotaFragExpr "GrokReflection" [pattern| EAST aqawwqaw |] [[t|Fragment '[] '[IotaList IotaAny]|]])
$(mkIotaFragExpr "GrokGambit" [pattern| EAST ewdewwde |] [[t|Fragment '[IotaList IotaAny] '[]|]])
$(mkIotaFragExpr "GrokReflectionII" [pattern| EAST waqwawwqwaw |] [[t|Fragment '[] '[IotaList IotaAny]|]])
$(mkIotaFragExpr "GrokGambitII" [pattern| EAST wewdwewwdwe |] [[t|Fragment '[IotaList IotaAny] '[]|]])

-- Spelunking

$( mkIotaFragExpr
     "SpelunkingGambit"
     [pattern| NORTH_EAST wdqqdwewdqqdwdqdadedaddww |]
     [[t|Fragment '[IotaNumber, IotaVector, IotaVector] '[IotaList IotaAny, IotaList IotaAny]|]]
 )

-- Telepathy

$(mkIotaFragExpr "TelepathicReflection" [pattern| EAST wqqadaw |] [[t|Fragment '[] '[IotaNumber]|]])
$(mkIotaFragExpr "OffensiveReflection" [pattern| NORTH_EAST qadee |] [[t|Fragment '[] '[IotaNumber]|]])
$(mkIotaFragExpr "ManipulativeReflection" [pattern| NORTH_WEST edaqq |] [[t|Fragment '[] '[IotaNumber]|]])
$(mkIotaFragExpr "ChargeReflection" [pattern| SOUTH_EAST aqaddq |] [[t|Fragment '[] '[IotaNumber]|]])
$(mkIotaFragExpr "RetreatReflection" [pattern| SOUTH_WEST dedwdq |] [[t|Fragment '[] '[IotaNumber]|]])
$(mkIotaFragExpr "DodgeReflection" [pattern| SOUTH_EAST edead |] [[t|Fragment '[] '[IotaNumber]|]])
$(mkIotaFragExpr "EvadeReflection" [pattern| SOUTH_WEST qaqda |] [[t|Fragment '[] '[IotaNumber]|]])
$(mkIotaFragExpr "StealthyReflection" [pattern| NORTH_WEST wede |] [[t|Fragment '[] '[IotaNumber]|]])
$(mkIotaFragExpr "FleeingReflection" [pattern| NORTH_WEST wqaq |] [[t|Fragment '[] '[IotaNumber]|]])
$(mkIotaFragExpr "LeapingReflection" [pattern| SOUTH_WEST qaqdaqqa |] [[t|Fragment '[] '[IotaNumber]|]])
$(mkIotaFragExpr "RollingReflection" [pattern| NORTH_EAST qadeeee |] [[t|Fragment '[] '[IotaNumber]|]])
$(mkIotaFragExpr "OffensivePurification" [pattern| NORTH_EAST wqadee |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])
$(mkIotaFragExpr "ManipulativePurification" [pattern| NORTH_WEST wedaqq |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])
$(mkIotaFragExpr "ChargePurification" [pattern| SOUTH_EAST aqaddqw |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])
$(mkIotaFragExpr "RetreatPurification" [pattern| SOUTH_WEST dedwdqw |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])
$(mkIotaFragExpr "DodgePurification" [pattern| SOUTH_EAST edeadw |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])
$(mkIotaFragExpr "EvadePurification" [pattern| SOUTH_WEST qaqdaw |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])
$(mkIotaFragExpr "LeapingPurification" [pattern| SOUTH_WEST qaqdawawa |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])
$(mkIotaFragExpr "StealthyPurification" [pattern| NORTH_WEST wwede |] [[t|Fragment '[IotaEntity] '[IotaNumber]|]])

-- Akashic Utilities

$(mkIotaFragExpr "LibrariansPurification" [pattern| EAST qaqqadaq |] [[t|Fragment '[IotaVector] '[IotaPattern]|]])
$(mkIotaFragExpr "LibrariansPurificationII" [pattern| EAST qaqqqada |] [[t|Fragment '[IotaVector] '[IotaAny]|]])
$( mkIotaFragExpr
     "LibrariansGambit"
     [pattern| SOUTH_WEST edeeedad |]
     [[t|forall a. Fragment '[a, IotaPattern, IotaVector] '[]|]]
 )
$(mkIotaFragExpr "LibrariansGambitII" [pattern| SOUTH_WEST edeedade |] [[t|Fragment '[IotaVector] '[]|]])

--- Spells

-- Mage Blocks

$(mkIotaFragExpr "ConjureMageBlock" [pattern| NORTH_WEST dee |] [[t|Fragment '[IotaVector] '[]|]])
$(mkIotaFragExpr "ResetMageBlock" [pattern| NORTH_WEST deeeaw |] [[t|Fragment '[IotaVector] '[]|]])
$(mkIotaFragExpr "Bouncy" [pattern| NORTH_WEST deeqa |] [[t|Fragment '[IotaVector] '[]|]])
$(mkIotaFragExpr "Energized" [pattern| NORTH_WEST deewad |] [[t|Fragment '[IotaNumber, IotaVector] '[]|]])
$(mkIotaFragExpr "Ephemeral" [pattern| NORTH_WEST deewwaawd |] [[t|Fragment '[IotaNumber, IotaVector] '[]|]])
$(mkIotaFragExpr "Volatile" [pattern| NORTH_WEST deewedeeeee |] [[t|Fragment '[IotaVector] '[]|]])

-- Rotation

$(mkIotaFragExpr "RotateBlock" [pattern| EAST edeeeeeweewadeeed |] [[t|Fragment '[IotaVector, IotaVector] '[]|]])
$(mkIotaFragExpr "RotateEntity" [pattern| EAST qqqdaqqqa |] [[t|Fragment '[IotaNumber, IotaNumber, IotaEntity] '[]|]])

-- Amber Preservation

$(mkIotaFragExpr "AmberSeal" [pattern| EAST qaqeaqwqwqa |] [[t|Fragment '[IotaVector] '[]|]])

-- Autographs

$(mkIotaFragExpr "Autograph" [pattern| WEST eeeeeww |] [[t|Fragment '[] '[]|]])
$(mkIotaFragExpr "Unautograph" [pattern| NORTH_EAST wwqqqqq |] [[t|Fragment '[] '[]|]])
$( mkIotaFragExpr
     "AuthenticatorsDistillation"
     [pattern| NORTH_EAST wwqqqqqaw |]
     [[t|Fragment '[IotaEntity, IotaItem] '[IotaBoolean]|]]
 )

-- Dyes

$( mkIotaFragExpr
     "ChromaticPurification"
     [pattern| NORTH_EAST weedwa |]
     [ [t|Fragment '[IotaIdentifier] '[IotaDye]|]
     , [t|Fragment '[IotaVector] '[IotaDye]|]
     , [t|Fragment '[IotaEntity] '[IotaDye]|]
     ]
 )
$( mkIotaFragExpr
     "Dye"
     [pattern| NORTH_WEST dwaqqw |]
     [ [t|Fragment '[IotaDye, IotaVector] '[]|]
     , [t|Fragment '[IotaDye, IotaEntity] '[]|]
     ]
 )

-- Mage Hand

$( mkIotaFragExpr
     "MageHand"
     [pattern| NORTH_WEST dqq |]
     [ [t|Fragment '[IotaBoolean, IotaNull, IotaEntity] '[]|]
     , [t|Fragment '[IotaBoolean, IotaNull, IotaVector] '[]|]
     , [t|Fragment '[IotaBoolean, IotaEntity, IotaEntity] '[]|]
     , [t|Fragment '[IotaBoolean, IotaEntity, IotaVector] '[]|]
     ]
 )

-- Mage Mouth

$(mkIotaFragExpr "MageMouth" [pattern| EAST eeede |] [[t|Fragment '[IotaEntity] '[]|]])

-- Magic Missile

$(mkIotaFragExpr "MagicMissile" [pattern| WEST qaqww |] [[t|Fragment '[IotaVector, IotaVector] '[]|]])

-- Pigment Manipulation

$( mkIotaFragExpr
     "PigmentPurification"
     [pattern| NORTH_WEST aqwedeweeeewweeew |]
     [ [t|Fragment '[IotaDye] '[IotaPigment]|]
     , [t|Fragment '[IotaEntity] '[IotaPigment]|]
     ]
 )
$( mkIotaFragExpr
     "PigmentExaltation"
     [pattern| SOUTH_EAST edewqaqqqqqwqqq |]
     [[t|Fragment '[IotaNumber, IotaVector, IotaPigment] '[IotaVector]|]]
 )
$(mkIotaFragExpr "InternalizePigmentII" [pattern| EAST weeeweeqeeeewqaqweeee |] [[t|Fragment '[IotaPigment] '[]|]])

-- Prestidigitation

$( mkIotaFragExpr
     "Prestidigitation"
     [pattern| NORTH_EAST wedewedew |]
     [ [t|Fragment '[IotaEntity] '[]|]
     , [t|Fragment '[IotaVector] '[]|]
     ]
 )

-- Toasting

$(mkIotaFragExpr "SendThought" [pattern| EAST qqqqwaqa |] [[t|forall a. Fragment '[a] '[]|]])
$(mkIotaFragExpr "ShoutThought" [pattern| EAST daqqqqwa |] [[t|forall a. Fragment '[a] '[]|]])
$(mkIotaFragExpr "Toast" [pattern| NORTH_EAST ewqqqwqqaee |] [[t|forall a b. Fragment '[IotaItem, b, a, IotaEntity] '[]|]])

-- Wristpocket

$(mkIotaFragExpr "Wristpocket" [pattern| WEST aaqqa |] [[t|Fragment '[] '[]|]])
$(mkIotaFragExpr "WristpocketReflection" [pattern| WEST aaqqada |] [[t|Fragment '[] '[IotaItem]|]])
$( mkIotaFragExpr
     "Sleight"
     [pattern| WEST aaqqaded |]
     [ [t|Fragment '[IotaEntity] '[]|]
     , [t|Fragment '[IotaVector] '[]|]
     ]
 )

-- Flora Conjuration

$( mkIotaFragExpr
     "ConjureFlora"
     [pattern| NORTH_EAST weqqqqqwaeaeaeaeaea |]
     [[t|Fragment '[IotaIdentifier, IotaVector] '[]|]]
 )

-- Hexical Spells

$( mkIotaFragExpr
     "Confetti"
     [pattern| EAST awddeqaedd |]
     [ [t|Fragment '[IotaNumber, IotaVector] '[]|]
     , [t|Fragment '[IotaVector, IotaVector] '[]|]
     ]
 )
$( mkIotaFragExpr
     "Silence"
     [pattern| EAST wddaq |]
     [[t|Fragment '[IotaEntity] '[]|]]
 )
$( mkIotaFragExpr
     "Vibrate"
     [pattern| EAST wwawawwd |]
     [ [t|Fragment '[IotaNumber, IotaEntity, IotaVector] '[]|]
     , [t|Fragment '[IotaNumber, IotaVector, IotaVector] '[]|]
     ]
 )
$(mkIotaFragExpr "Sparkle" [pattern| NORTH_EAST dqa |] [[t|Fragment '[IotaNumber, IotaVector, IotaVector] '[]|]])
$(mkIotaFragExpr "CrackDevice" [pattern| EAST wwaqqqqqeqdedwqeaeqwdedwqeaeq |] [[t|Fragment '[] '[]|]])
$(mkIotaFragExpr "Illuminate" [pattern| SOUTH_EAST aeaeaeaeaeawqqqqq |] [[t|Fragment '[IotaNumber, IotaVector] '[]|]])
$(mkIotaFragExpr "Gasp" [pattern| NORTH_WEST aweeeeewaweeeee |] [[t|Fragment '[IotaEntity] '[]|]])
$(mkIotaFragExpr "Squawk" [pattern| NORTH_EAST wweedadw |] [[t|Fragment '[IotaIdentifier, IotaVector] '[]|]])

-- Projectiles

$(mkIotaFragExpr "ConjureEgg" [pattern| SOUTH_EAST qqqwaqaaqeeewdedde |] [[t|Fragment '[IotaVector] '[IotaEntity]|]])
$(mkIotaFragExpr "ConjureSpit" [pattern| EAST dwqaqw |] [[t|Fragment '[IotaVector] '[IotaEntity]|]])
$(mkIotaFragExpr "ConjureSnowball" [pattern| NORTH_EAST ddeeeeewd |] [[t|Fragment '[IotaVector] '[IotaEntity]|]])
$(mkIotaFragExpr "ConjureFireball" [pattern| SOUTH_EAST wqqqqqwaeaeaeaeae |] [[t|Fragment '[IotaVector] '[IotaEntity]|]])

-- Muting

$(mkIotaFragExpr "CensorshipPurification" [pattern| NORTH_WEST edaaw |] [[t|Fragment '[IotaEntity] '[IotaBoolean]|]])

-- Redstone Zapping

$(mkIotaFragExpr "Zap" [pattern| WEST qad |] [[t|Fragment '[IotaNumber, IotaNumber, IotaVector] '[]|]])

-- Specialized Breaking

$(mkIotaFragExpr "ExtractBlock" [pattern| EAST qaqqqqqdeeeqeee |] [[t|Fragment '[IotaNumber, IotaVector] '[]|]])
$(mkIotaFragExpr "CollectBlock" [pattern| EAST aqaeaqdeeweweedq |] [[t|Fragment '[IotaVector] '[]|]])

-- Conjurable Delights

$(mkIotaFragExpr "ConjureHexGummy" [pattern| SOUTH_WEST eeewdw |] [[t|Fragment '[IotaVector] '[]|]])
$(mkIotaFragExpr "ConjureHexburst" [pattern| EAST aadaadqaq |] [[t|forall a. Fragment '[a, IotaVector] '[]|]])
$(mkIotaFragExpr "ConjureHextito" [pattern| EAST qaqdqaqdwawaw |] [[t|Fragment '[IotaAnyList, IotaVector] '[]|]])

-- Conjure Spike

$( mkIotaFragExpr
     "ConjureSpike"
     [pattern| NORTH_EAST qdqdqdqdww |]
     [[t|Fragment '[IotaNumber, IotaVector, IotaVector] '[]|]]
 )

-- Block Mimicry

$(mkIotaFragExpr "Push" [pattern| SOUTH_WEST wqwawqwqqqeqq |] [[t|Fragment '[IotaVector, IotaVector] '[]|]])
$( mkIotaFragExpr
     "Dispense"
     [pattern| SOUTH_WEST wqwawqwddaeeead |]
     [[t|Fragment '[IotaVector, IotaVector, IotaItem] '[]|]]
 )
$(mkIotaFragExpr "Cook" [pattern| SOUTH_EAST qwqqadadadewewewe |] [[t|Fragment '[IotaItem] '[]|]])
$(mkIotaFragExpr "Roast" [pattern| NORTH_WEST aqqwwqqawdadedad |] [[t|Fragment '[IotaItem] '[]|]])
$(mkIotaFragExpr "Smoke" [pattern| SOUTH_EAST qwqqadadadewdqqdwe |] [[t|Fragment '[IotaItem] '[]|]])
$(mkIotaFragExpr "Blast" [pattern| SOUTH_EAST qwqqadadadewweewwe |] [[t|Fragment '[IotaItem] '[]|]])
$(mkIotaFragExpr "CutStone" [pattern| EAST qqqqqwaeaeaeaeaeadawa |] [[t|Fragment '[IotaIdentifier, IotaItem] '[]|]])

-- Circle Spells

$(mkIotaFragExpr "Displace" [pattern| NORTH_EAST qaqqqqeedaqqqa |] [[t|Fragment '[IotaVector, IotaEntity] '[]|]])
$(mkIotaFragExpr "Appendage" [pattern| WEST aaqqadaqwqa |] [[t|Fragment '[IotaVector] '[]|]])

-- Evocation

$(mkIotaFragExpr "Inculcate" [pattern| EAST wwaqqqqqeqdedwwqwqwwdedwwqwqw |] [[t|Fragment '[IotaAnyList] '[]|]])
$(mkIotaFragExpr "EvocationReflection" [pattern| EAST wwdeeeeeqeaqawwewewwaqawwewew |] [[t|Fragment '[] '[IotaAnyList]|]])
$(mkIotaFragExpr "EvokerReflection" [pattern| EAST wwaqqqqqeeaqawwewewwaqawwewew |] [[t|Fragment '[] '[IotaNumber]|]])

-- Hopper

{- ORMOLU_DISABLE -}
$( mkIotaFragExpr
     "Hopper"
     [pattern| SOUTH_EAST qwawqwaeqqq |]
     [[t|Fragment '[IotaEntity, IotaEntity] '[]|]
     ,[t|Fragment '[IotaVector, IotaEntity] '[]|]
     ,[t|Fragment '[IotaNull  , IotaEntity] '[]|]
     ,[t|Fragment '[IotaEntity, IotaVector] '[]|]
     ,[t|Fragment '[IotaVector, IotaVector] '[]|]
     ,[t|Fragment '[IotaNull  , IotaVector] '[]|]
     ,[t|Fragment '[IotaEntity, IotaNull  ] '[]|]
     ,[t|Fragment '[IotaVector, IotaNull  ] '[]|]
     ,[t|Fragment '[IotaNull  , IotaNull  ] '[]|]
     ,[t|Fragment '[IotaNumber, IotaEntity, IotaEntity] '[]|]
     ,[t|Fragment '[IotaNumber, IotaVector, IotaEntity] '[]|]
     ,[t|Fragment '[IotaNumber, IotaNull  , IotaEntity] '[]|]
     ,[t|Fragment '[IotaNumber, IotaEntity, IotaVector] '[]|]
     ,[t|Fragment '[IotaNumber, IotaVector, IotaVector] '[]|]
     ,[t|Fragment '[IotaNumber, IotaNull  , IotaVector] '[]|]
     ,[t|Fragment '[IotaNumber, IotaEntity, IotaNull  ] '[]|]
     ,[t|Fragment '[IotaNumber, IotaVector, IotaNull  ] '[]|]
     ,[t|Fragment '[IotaNumber, IotaNull  , IotaNull  ] '[]|]
     ,[t|Fragment '[IotaEntity, IotaNumber, IotaEntity] '[]|]
     ,[t|Fragment '[IotaVector, IotaNumber, IotaEntity] '[]|]
     ,[t|Fragment '[IotaNull  , IotaNumber, IotaEntity] '[]|]
     ,[t|Fragment '[IotaEntity, IotaNumber, IotaVector] '[]|]
     ,[t|Fragment '[IotaVector, IotaNumber, IotaVector] '[]|]
     ,[t|Fragment '[IotaNull  , IotaNumber, IotaVector] '[]|]
     ,[t|Fragment '[IotaEntity, IotaNumber, IotaNull  ] '[]|]
     ,[t|Fragment '[IotaVector, IotaNumber, IotaNull  ] '[]|]
     ,[t|Fragment '[IotaNull  , IotaNumber, IotaNull  ] '[]|]
     ,[t|Fragment '[IotaNumber, IotaEntity, IotaNumber, IotaEntity] '[]|]
     ,[t|Fragment '[IotaNumber, IotaVector, IotaNumber, IotaEntity] '[]|]
     ,[t|Fragment '[IotaNumber, IotaNull  , IotaNumber, IotaEntity] '[]|]
     ,[t|Fragment '[IotaNumber, IotaEntity, IotaNumber, IotaVector] '[]|]
     ,[t|Fragment '[IotaNumber, IotaVector, IotaNumber, IotaVector] '[]|]
     ,[t|Fragment '[IotaNumber, IotaNull  , IotaNumber, IotaVector] '[]|]
     ,[t|Fragment '[IotaNumber, IotaEntity, IotaNumber, IotaNull  ] '[]|]
     ,[t|Fragment '[IotaNumber, IotaVector, IotaNumber, IotaNull  ] '[]|]
     ,[t|Fragment '[IotaNumber, IotaNull  , IotaNumber, IotaNull  ] '[]|]]
 )
{- ORMOLU_ENABLE -}

$(mkIotaFragExpr "HopperPurification" [pattern| SOUTH_WEST qqqeawqwawq |] [[t|forall a. Fragment '[a] '[IotaAnyList]|]])

-- Hotbar

$(mkIotaFragExpr "HandyReflection" [pattern| EAST qwawqwa |] [[t|Fragment '[] '[IotaNumber]|]])
$(mkIotaFragExpr "Switch" [pattern| WEST dwewdwe |] [[t|Fragment '[IotaNumber] '[]|]])

-- Lesser Sentinels

$(mkIotaFragExpr "DeploySentinels" [pattern| EAST aeaae |] [[t|Fragment '[IotaList IotaVector] '[]|]])
$(mkIotaFragExpr "InfiltrationReflection" [pattern| WEST dqddq |] [[t|Fragment '[] '[IotaList IotaVector]|]])

-- Pyrotechnics

$( mkIotaFragExpr
     "ConjureFirework"
     [pattern| SOUTH_WEST dedwaqwwawwqa |]
     [ [t|
         Fragment
           '[IotaBoolean, IotaBoolean, IotaList IotaVector, IotaList IotaVector, IotaNumber, IotaNumber, IotaVector, IotaVector]
           '[]
         |]
     ]
 )

-- Sentinel Defense

$( mkIotaFragExpr
     "VigilancePurification"
     [pattern| NORTH_EAST dwqwaeawaeqqqwqwqqw |]
     [[t|Fragment '[IotaVector] '[IotaVector]|]]
 )
$(mkIotaFragExpr "ExorciseSentinel" [pattern| EAST wdqdwdqedwewaawewd |] [[t|Fragment '[IotaVector] '[]|]])

-- Shaders

$(mkIotaFragExpr "ClearVision" [pattern| WEST eeeeeqaqeeeee |] [[t|Fragment '[] '[]|]])
$(mkIotaFragExpr "PierceDarkness" [pattern| WEST edewawede |] [[t|Fragment '[] '[]|]])
$(mkIotaFragExpr "VisualizeForms" [pattern| WEST eedwwawwdee |] [[t|Fragment '[] '[]|]])
$(mkIotaFragExpr "BroadcastVision" [pattern| WEST wewdwewwawwewdwew |] [[t|Fragment '[] '[]|]])
$(mkIotaFragExpr "IdentifyImportance" [pattern| WEST eewdweqaqewdwee |] [[t|Fragment '[] '[]|]])
$(mkIotaFragExpr "SplitVision" [pattern| NORTH_EAST qaqdedaedqqdedaqaedeqd |] [[t|Fragment '[] '[]|]])

-- Horrible
$( mkIotaFragExpr
     "Horrible"
     [pattern| EAST wedqawqeewdeaqeewdeaqqedqawqqedqawqeedqawqqewdeaqeedqawqeewdeaqqewdeaqeewdeaqeedqawqqedqawqqewdeaqeedqawqeewdeaqqewdeaqeewdeaqeedqawqqedqawqqewdeaqqedqawqeewdeaqeewdeaqqedqawqqedqawqeedqawqqewdeaqqedqawqeewdeaqeewdeaqqedqawqqedqawqeedqawqqewdeaqeedqawqeewdeaqeewdeaqqedqawqqedqawqeedqawqqewdeaqqedqawqeewdeaqqewdeaqeewdeaqeedqawqqedqawqqewdeaqe |]
     [[t|Fragment '[IotaVector] '[]|]]
 )

--- Specks

-- Specks

$( mkIotaFragExpr
     "ConjureSpeck"
     [pattern| SOUTH_WEST ade |]
     [[t|forall a. Fragment '[IotaVector, IotaVector, a] '[IotaEntity]|]]
 )
$(mkIotaFragExpr "DismissSpecklike" [pattern| SOUTH_WEST adeaqde |] [[t|Fragment '[IotaEntity] '[]|]])
$(mkIotaFragExpr "ExecuteSpecklike" [pattern| SOUTH_WEST adeqqaawdd |] [[t|Fragment '[IotaNumber, IotaEntity] '[]|]])
$(mkIotaFragExpr "MoveSpecklike" [pattern| SOUTH_WEST adeqaa |] [[t|Fragment '[IotaVector, IotaEntity] '[]|]])
$(mkIotaFragExpr "RotateSpecklike" [pattern| SOUTH_WEST adeaw |] [[t|Fragment '[IotaVector, IotaEntity] '[]|]])
$(mkIotaFragExpr "RollSpeck" [pattern| SOUTH_WEST adeqqqqq |] [[t|Fragment '[IotaNumber, IotaEntity] '[]|]])
$(mkIotaFragExpr "ResizeSpecklike" [pattern| SOUTH_WEST adeeqed |] [[t|Fragment '[IotaNumber, IotaEntity] '[]|]])
$(mkIotaFragExpr "ThickenSpeck" [pattern| SOUTH_WEST adeeqw |] [[t|Fragment '[IotaNumber, IotaEntity] '[]|]])
$(mkIotaFragExpr "ColorSpeck" [pattern| SOUTH_WEST adeqqaq |] [[t|Fragment '[IotaPigment, IotaEntity] '[]|]])

-- Strands

$(mkIotaFragExpr "ConjureStrand" [pattern| SOUTH_EAST daq |] [[t|Fragment '[IotaVector] '[IotaEntity]|]])
$( mkIotaFragExpr
     "HandwritingPurification"
     [pattern| NORTH_EAST eadqqqa |]
     [[t|Fragment '[IotaPattern] '[IotaList IotaVector]|]]
 )

-- Meshes

$(mkIotaFragExpr "ConjureMesh" [pattern| EAST qaqqqqqwqqqdeeweweeaeewewee |] [[t|Fragment '[IotaVector] '[IotaEntity]|]])

--- Great Spells

-- Charm

$( mkGreatIotaFragExpr
     "Charm"
     "Charm"
     [pattern| SOUTH_EAST edeeeeeqaaqeeeadweeqeeqdqeeqeeqde |]
     [[t|Fragment '[IotaAny, IotaList IotaAny, IotaList IotaAny, IotaNumber, IotaList IotaAny] '[]|]]
 )
$(mkIotaFragExpr "LEDGambit" [pattern| NORTH_EAST qaqqdwdwd |] [[t|Fragment '[IotaVector] '[]|]])
$(mkIotaFragExpr "Discharm" [pattern| NORTH_EAST qaqwddaaeawaea |] [[t|Fragment '[IotaEntity] '[]|]])
$(mkIotaFragExpr "CharmedGambit" [pattern| NORTH_EAST waqqqqqedeqdqdqdqdqe |] [[t|forall a. Fragment '[a] '[]|]])
$(mkIotaFragExpr "CharmedReflection" [pattern| NORTH_EAST waqqqqqeaqeaeaeaeaeq |] [[t|Fragment '[] '[IotaAny]|]])
$(mkIotaFragExpr "CharmedGambitII" [pattern| SOUTH_EAST edewqaqqdeeeee |] [[t|forall a. Fragment '[a] '[]|]])
$(mkIotaFragExpr "CharmedReflectionII" [pattern| NORTH_EAST qaqwedeeaqqqqq |] [[t|Fragment '[] '[IotaAny]|]])

-- Greater Blink

$( mkGreatIotaFragExpr
     "GreaterBlink"
     "Greater Blink"
     [pattern| SOUTH_WEST wqawawaqwqwqawawaqw |]
     [[t|Fragment '[IotaVector] '[]|]]
 )
