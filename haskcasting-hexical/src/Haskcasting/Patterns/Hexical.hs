{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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

-- Associates a pattern to a list of patterns in the Grimoire in my offhand.
$( mkIotaFragExpr
     "WriteGrimoire"
     [pattern| WEST aqwqaeaqa |]
     [[t|'[IotaAnyList, IotaPattern] -> '[]|]]
     -- ['list', 'pattern'] -> ['']
 )

-- Erases any associations for a pattern that may exist in the Grimoire in my offhand.
$( mkIotaFragExpr
     "EraseGrimoire"
     [pattern| WEST aqwqaqded |]
     [[t|'[IotaPattern] -> '[]|]]
     -- ['pattern'] -> ['']
 )

-- Gets a list of all patterns modified by the Grimoire in my offhand.
$( mkIotaFragExpr
     "ArchivistReflection"
     [pattern| SOUTH_EAST aqaeaqwqa |]
     [[t|'[] -> '[IotaList IotaPattern]|]]
     -- [''] -> ['list of patterns']
 )

-- Yellows the parchment of an Animated Scroll, making it bear striking semblance to some ancient scrolls I\'ve been finding.
$( mkIotaFragExpr
     "AgeScroll"
     [pattern| EAST waeqqqqeqqqwqeaeaeaeq |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['animated scroll entity'] -> ['']
 )

-- Changes the color of an Animated Scroll\'s ink to the color specified by the vector, with the components being 0 to 1 values of red, green, and blue respectively.
$( mkIotaFragExpr
     "DyeInk"
     [pattern| EAST waeqqqqewqqwqqeqeqqwqqeq |]
     [[t|'[IotaVector, IotaEntity] -> '[]|]]
     -- ['vector', 'animated scroll entity'] -> ['']
 )

-- Causes the ink of an Animated Scroll to brightly glow, regardless of lighting conditions.
$( mkIotaFragExpr
     "IlluminateInk"
     [pattern| EAST waeqqqqedeqdqdqdqeqdwwd |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['animated scroll entity'] -> ['']
 )

-- Causes the Animated Scroll to become invisible leaving only the pattern, making it appear as though the pattern were magically etched into the surface the scroll is on.
$( mkIotaFragExpr
     "VanishScroll"
     [pattern| EAST waeqqqqedeqeeweeqewee |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['animated scroll entity'] -> ['']
 )

-- Fortunately all is not lost. I have found this identifier by a group of botanist Hexcasters. I believe there may be a spell to allow me to bring back these flowers.
$( mkIotaFragExpr
     "PeriwinkleReflection"
     [pattern| NORTH_EAST qaqwqaqwqaq |]
     [[t|'[] -> '[IotaIdentifier]|]]
     -- [''] -> ['identifier']
 )

-- Wish the genie to cast a Hex for me. Because I am teaching a mind rather than etching a casting device, I can freely reteach the genie a new Hex any time without losing media.
$( mkIotaFragExpr
     "Wish"
     [pattern| NORTH_WEST eweweweweweewedeaqqqd |]
     [[t|'[IotaList IotaPattern] -> '[]|]]
     -- ['list of patterns'] -> ['']
 )

-- Pushes my original position when I began using the Hand Lamp.
$( mkIotaFragExpr
     "GenieReflectionSpatial"
     [pattern| SOUTH_WEST qwddedqdd |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

-- Pushes my original rotation when I began using the Hand Lamp.
$( mkIotaFragExpr
     "GenieReflectionRotational"
     [pattern| SOUTH_WEST qwddedadw |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

-- Pushes my original velocity when I began using the Hand Lamp.
$( mkIotaFragExpr
     "GenieReflectionKinetic"
     [pattern| SOUTH_WEST qwddedqew |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

-- Pushes how many times the Hand Lamp has cast since I began using the Hand Lamp. I can divide by by 20 to convert to seconds.
$( mkIotaFragExpr
     "GenieReflectionTemporal"
     [pattern| SOUTH_WEST qwddedqwddwa |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

-- Pushes the remaining media of the Hand Lamp, in units of dust.
$( mkIotaFragExpr
     "GenieReflectionMedia"
     [pattern| SOUTH_WEST qwddedaeeeee |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

-- Asks the genie to remember an iota for me. Strangely, the genie seems able to bypass the Transgress Others mishap, perhaps because it requires my active concentration to use this Hand Lamp.
$( mkIotaFragExpr
     "GenieGambit"
     [pattern| SOUTH_WEST qwddedqedeeeee |]
     [[t|forall a. '[a] -> '[]|]]
     -- ['any'] -> ['']
 )

-- Requests the iota I had saved to the genie to be pushed to the top of the stack. If I had not previously saved anything, the genie pushes Null.
$( mkIotaFragExpr
     "GenieReflectionMemory"
     [pattern| SOUTH_WEST qwddedqwaqqqqq |]
     [[t|'[] -> '[IotaAny]|]]
     -- [''] -> ['any']
 )

-- The instant I let go of my Hand Lamp, it casts an additional time. I can use this pattern to recognize whether a cast is that finale, and react accordingly.
$( mkIotaFragExpr
     "FinaleReflection"
     [pattern| EAST aaddaddad |]
     [[t|'[] -> '[IotaBoolean]|]]
     -- [''] -> ['boolean']
 )

-- Refuels the Lamp in my offhand. Costs about one Charged Amethyst, plus the media I\'m giving the Lamp.
$( mkIotaFragExpr
     "RefuelLamp"
     [pattern| EAST qaqwawqwqqwqwqwqwqwqq |]
     [[t|'[IotaNumber] -> '[]|]]
     -- ['number'] -> ['']
 )

-- Hold the lamp in my other hand and cast. Costs about ten Charged Amethyst.
$( mkIotaFragExpr
     "PromoteLamp"
     [pattern| WEST qweedeqeedeqdqdwewewwewewwewe |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['entity'] -> ['']
 )

-- Pushes whether a player currently has an active Archgenie Lamp.
$( mkIotaFragExpr
     "ArchgeniePurification"
     [pattern| NORTH_EAST qaqwddedqeed |]
     [[t|'[IotaEntity] -> '[IotaBoolean]|]]
     -- ['entity'] -> ['boolean']
 )

-- Pushes my original position when the Archgenie Lamp started casting.
$( mkIotaFragExpr
     "ArchgenieReflectionSpatial"
     [pattern| NORTH_EAST qaqwddedqdd |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

-- Pushes my original rotation when the Archgenie Lamp started casting.
$( mkIotaFragExpr
     "ArchgenieReflectionRotational"
     [pattern| NORTH_EAST qaqwddedadw |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

-- Pushes my original velocity when the Archgenie Lamp started casting.
$( mkIotaFragExpr
     "ArchgenieReflectionKinetic"
     [pattern| NORTH_EAST qaqwddedqew |]
     [[t|'[] -> '[IotaVector]|]]
     -- [''] -> ['vector']
 )

-- Pushes how many times the Archgenie Lamp has cast since its activation. I can divide by by 20 to convert to seconds.
$( mkIotaFragExpr
     "ArchgenieReflectionTemporal"
     [pattern| NORTH_EAST qaqwddedqwddwa |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

-- Pushes the remaining amount of media left in the Archgenie Lamp, in units of dust.
$( mkIotaFragExpr
     "ArchgenieReflectionMedia"
     [pattern| NORTH_EAST qaqwddedaeeeee |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

-- Can be cast by a casting device, Staff, or within the Archgenie Lamp to remember an iota. Unlike Hand Lamp, this is subject to Transgress Others mishap.
$( mkIotaFragExpr
     "ArchgenieGambit"
     [pattern| NORTH_EAST qaqwddedqedeeeee |]
     [[t|forall a. '[a] -> '[]|]]
     -- ['any'] -> ['']
 )

-- Can be cast by a casting device, Staff, or within the Archgenie Lamp to push the stored iota.
$( mkIotaFragExpr
     "ArchgenieReflectionMemory"
     [pattern| NORTH_EAST qaqwddedqwaqqqqq |]
     [[t|'[] -> '[IotaAny]|]]
     -- [''] -> ['any']
 )

-- Pushes whether two patterns have the same shape and orientation.
$( mkIotaFragExpr
     "CongruenceDistillation"
     [pattern| EAST aaqd |]
     [[t|'[IotaPattern, IotaPattern] -> '[IotaBoolean]|]]
     -- ['pattern', 'pattern'] -> ['boolean']
 )

-- Turns a pattern into a list of numbers for my splitting, analysis, and dissection. The inverse of Calligrapher\'s Purification.
$( mkIotaFragExpr
     "ChirographersPurification"
     [pattern| EAST wqaedeqd |]
     [[t|'[IotaPattern] -> '[IotaList IotaNumber]|]]
     -- ['pattern'] -> ['list of numbers']
 )

-- Turns a list of numbers into a pattern for my viewing and execution. The inverse of Chirographer\'s Purification.
$( mkIotaFragExpr
     "CalligraphersPurification"
     [pattern| EAST wqqqaqwd |]
     [[t|'[IotaList IotaNumber] -> '[IotaPattern]|]]
     -- ['list of numbers'] -> ['pattern']
 )

-- Takes a pattern and produces a normalized list of vectors. The z-component of these vectors is zero. I feel this may be useful for artistic Hexes.
$( mkIotaFragExpr
     "HandwritingDistillation"
     [pattern| NORTH_EAST eadqqqa |]
     [[t|'[IotaPattern] -> '[IotaList IotaVector]|]]
     -- ['pattern'] -> ['list of vectors']
 )

-- Uses the number to shuffle the pattern into one of the same shape, but different stroke order. My notes reveal that ancient Hexcasters used this for some grand library.
$( mkIotaFragExpr
     "GlyphmakersDistillation"
     [pattern| NORTH_EAST aqqqdae |]
     [[t|'[IotaNumber, IotaPattern] -> '[IotaPattern]|]]
     -- ['number', 'pattern'] -> ['pattern']
 )

-- By focusing on a particular image, I can detect that concentration via this pattern. Pushes how many twentieths of a second I\'ve been intending to Call Telepathy, or -1 if I am not.
$( mkIotaFragExpr
     "TelepathicReflection"
     [pattern| EAST wqqadaw |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

-- Momentarily displays an iota above my hotbar. If cast repeatedly, each cast overwrites the previous instantly.
$( mkIotaFragExpr
     "SendThought"
     [pattern| EAST qqqqwaqa |]
     [[t|forall a. '[a] -> '[]|]]
     -- ['any'] -> ['']
 )

-- Grandly flahes an iota across my vision. It obstructs my vision and takes some time to fade in, so I shall only use it sparingly.
$( mkIotaFragExpr
     "ShoutThought"
     [pattern| EAST daqqqqwa |]
     [[t|forall a. '[a] -> '[]|]]
     -- ['any'] -> ['']
 )

-- Causes me to hear a pling that is inaudible to other players.
$( mkIotaFragExpr
     "HallucinatePling"
     [pattern| NORTH_EAST eqqqada |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Causes me to hear a click that is inaudible to other players.
$( mkIotaFragExpr
     "HallucinateClick"
     [pattern| NORTH_EAST eqqadaq |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Pushes how many twentieths of a second I\'ve been intending to Attack/Destroy, or -1 if I am not.
$( mkIotaFragExpr
     "OffensiveReflection"
     [pattern| NORTH_EAST qadee |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

-- Pushes how many twentieths of a second I\'ve been intending to Use Item/Place Block, or -1 if I am not.
$( mkIotaFragExpr
     "ManipulativeReflection"
     [pattern| NORTH_WEST edaqq |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

-- Pushes how many twentieths of a second I\'ve been intending to Walk Forwards, or -1 if I am not.
$( mkIotaFragExpr
     "ChargeReflection"
     [pattern| SOUTH_EAST aqaddq |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

-- Pushes how many twentieths of a second I\'ve been intending to Walk Backwards, or -1 if I am not.
$( mkIotaFragExpr
     "RetreatReflection"
     [pattern| SOUTH_WEST dedwdq |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

-- Pushes how many twentieths of a second I\'ve been intending to Strafe Left, or -1 if I am not. Can be differentiated from Evade Reflection because d comes to the left of e.
$( mkIotaFragExpr
     "DodgeReflection"
     [pattern| SOUTH_EAST edead |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

-- Pushes how many twentieths of a second I\'ve been intending to Strafe Right, or -1 if I am not. Can be differentiated from Dodge Reflection because e comes to the right of d.
$( mkIotaFragExpr
     "EvadeReflection"
     [pattern| SOUTH_WEST qaqda |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

-- Pushes how many twentieths of a second I\'ve been intending to Jump, or -1 if I am not.
$( mkIotaFragExpr
     "LeapingReflection"
     [pattern| SOUTH_WEST qaqdaqqa |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

-- Pushes how many twentieths of a second I\'ve been intending to Sneak, or -1 if I am not.
$( mkIotaFragExpr
     "StealthyReflection"
     [pattern| NORTH_WEST wede |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

-- Reads the pattern key of an Akashic Bookshelf.
$( mkIotaFragExpr
     "LibrariansPurification"
     [pattern| EAST qaqqadaq |]
     [[t|'[IotaVector] -> '[IotaPattern]|]]
     -- ['vector'] -> ['pattern/null']
 )

-- Reads the iota from an Akashic Bookshelf.
$( mkIotaFragExpr
     "LibrariansPurificationII"
     [pattern| EAST qaqqqada |]
     [[t|'[IotaVector] -> '[IotaAny]|]]
     -- ['vector'] -> ['any']
 )

-- Writes an iota under a pattern key to an Akashic Bookshelf.
$( mkIotaFragExpr
     "LibrariansGambit"
     [pattern| SOUTH_WEST edeeedad |]
     [[t|forall a. '[a, IotaPattern, IotaVector] -> '[]|]]
     -- ['any', 'pattern', 'vector'] -> ['']
 )

-- Clears an Akashic Bookshelf.
$( mkIotaFragExpr
     "LibrariansGambitII"
     [pattern| SOUTH_WEST edeedade |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Conjures a mage block at the location. Costs about three Amethyst Dust.
$( mkIotaFragExpr
     "ConjureMageBlock"
     [pattern| NORTH_WEST dee |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- This modifier makes the block delightfully fun to bounce on! It returns more force to me than regular slime blocks and I notice even if I sneak, the block bounces me regardless.
$( mkIotaFragExpr
     "Bouncy"
     [pattern| NORTH_WEST deeqa |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- This modifier makes the block emit a Redstone signal, with the number corresponding to the power level of the output.
$( mkIotaFragExpr
     "Energized"
     [pattern| NORTH_WEST deewad |]
     [[t|'[IotaNumber, IotaVector] -> '[]|]]
     -- ['number', 'vector'] -> ['']
 )

-- This modifier takes in a number in addition to a position and shatters the block after many twentieths of a second. Subsequent casts can lengthen or shorten the duration.
$( mkIotaFragExpr
     "Ephemeral"
     [pattern| NORTH_WEST deewwaawd |]
     [[t|'[IotaNumber, IotaVector] -> '[]|]]
     -- ['number', 'vector'] -> ['']
 )

-- This modifier prevents the block from emitting the telltale particles that accompany my casting, even when being stood on.
$( mkIotaFragExpr
     "Invisible"
     [pattern| NORTH_WEST deeqedeaqqqwqqq |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- This modifier is useful for construction! With this modifier applied, I find I can easily place another block in the place of the mage block.
$( mkIotaFragExpr
     "Replaceable"
     [pattern| NORTH_WEST deewqaqqqqq |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- This modifier makes the block break other mage blocks it\'s touching. Notably, if it breaks another volatile block, that block breaks and so on until every volatile block shatters.
$( mkIotaFragExpr
     "Volatile"
     [pattern| NORTH_WEST deewedeeeee |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Autographs the item in my offhand with my name and Pigment. Autographing an item with my name already on it moves my name to the top of the list.
$( mkIotaFragExpr
     "Autograph"
     [pattern| WEST eeeeeww |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- A more destructive form of Erase Item, capable of burning off autographs. Costs about one Amethyst Dust similar to Erase Item.
$( mkIotaFragExpr
     "Unautograph"
     [pattern| NORTH_EAST wwqqqqq |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Pushes whether a given player has signed a given item stack.
$( mkIotaFragExpr
     "AuthenticatorsDistillation"
     [pattern| NORTH_EAST wwqqqqqaw |]
     []
     -- ['entity', 'item stack'] -> ['boolean']
 )

-- Gets the dye of a block, entity, or block/item identifier.
$( mkIotaFragExpr
     "ChromaticPurification"
     [pattern| NORTH_EAST weedwa |]
     [ [t|'[IotaIdentifier] -> '[IotaDye]|]
     , [t|'[IotaVector] -> '[IotaDye]|]
     , [t|'[IotaEntity] -> '[IotaDye]|]
     ]
     -- ['id/vector/entity'] -> ['dye/null']
 )

-- Dyes a dyeable block or entity. Costs about an eighth of one Amethyst Dust.
$( mkIotaFragExpr
     "Dye"
     [pattern| NORTH_WEST dwaqqw |]
     [ [t|'[IotaDye, IotaVector] -> '[]|]
     , [t|'[IotaDye, IotaEntity] -> '[]|]
     ]
     -- ['dye', 'vector/entity'] -> ['']
 )

-- Translates a dye color into a vector, where each component is from 0 to 1 and represents the red, green, and blueness of the dye respectively.
$( mkIotaFragExpr
     "VisionPurification"
     [pattern| EAST wdwwaawwewdwwewwdwwe |]
     [[t|'[IotaDye] -> '[IotaVector]|]]
     -- ['dye'] -> ['vector']
 )

-- Conjures a small silver of amethyst that always deals a full heart of damage and forces the target backwards. Costs about one Amethyst Dust.
$( mkIotaFragExpr
     "MagicMissile"
     [pattern| WEST qaqww |]
     [[t|'[IotaVector, IotaVector] -> '[]|]]
     -- ['vector', 'vector'] -> ['']
 )

-- Takes a colored dye iota and transforms it into a pigment, takes a player and returns their pigment, or takes an item containing a pigment and returns its pigment.
$( mkIotaFragExpr
     "PigmentPurification"
     [pattern| NORTH_WEST aqwedeweeeewweeew |]
     [ [t|'[IotaDye] -> '[IotaPigment]|]
     , [t|'[IotaEntity] -> '[IotaPigment]|]
     ]
     -- ['dye/entity'] -> ['pigment']
 )

-- Samples the pigment at a certain place and time as a vector, where each component is from 0 to 1 and represents the red, green, and blueness respectively.
$( mkIotaFragExpr
     "PigmentExaltation"
     [pattern| SOUTH_EAST edewqaqqqqqwqqq |]
     [[t|'[IotaNumber, IotaVector, IotaPigment] -> '[IotaVector]|]]
     -- ['num', 'vec', 'pigment'] -> ['vec']
 )

-- Internalizes a pigment iota.
$( mkIotaFragExpr
     "InternalizePigmentII"
     [pattern| EAST weeeweeqeeeewqaqweeee |]
     [[t|'[IotaPigment] -> '[]|]]
     -- ['pigment'] -> ['']
 )

-- Causes a small magical effect, not too distinct from the original nature or function of the target. Costs about a tenth of one Amethyst Dust.
$( mkIotaFragExpr
     "Prestidigitation"
     [pattern| NORTH_EAST wedewedew |]
     [ [t|'[IotaEntity] -> '[]|]
     , [t|'[IotaVector] -> '[]|]
     ]
     -- ['entity/vector'] -> ['']
 )

-- This spell vanishes the stack of items in my other hand, or conjures it back, swapping if my other hand is holding something. Costs about a eighth of one Amethyst Dust.
$( mkIotaFragExpr
     "Wristpocket"
     [pattern| WEST aaqqa |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Returns the item in my wristpocket, or Null if it is empty.
$( mkIotaFragExpr
     "WristpocketReflection"
     [pattern| WEST aaqqada |]
     []
     -- [''] -> ['item']
 )

-- Performs an act of magical sleight of hand to steal items into my wristpocket or expel my wristpocket back into the world. Costs about a fourth of one Amethyst Dust.
$( mkIotaFragExpr
     "Sleight"
     [pattern| WEST aaqqadeeeq |]
     [ [t|'[IotaEntity] -> '[]|]
     , [t|'[IotaVector] -> '[]|]
     ]
     -- ['item entity/vector'] -> ['']
 )

-- Projects my mind\'s hand forward to use my wristpocketed item and interact with the world. If my wristpocket is empty, acts as though a plain hand had reached out. Costs about one Amethyst Dust.
$( mkIotaFragExpr
     "MageHand"
     [pattern| WEST aaqqaeea |]
     [ [t|'[IotaEntity] -> '[]|]
     , [t|'[IotaVector] -> '[]|]
     ]
     -- ['entity/vector'] -> ['']
 )

-- Makes me eat my wristpocketed item, nourishing me or applying potions to me. Costs about one Amethyst Dust and mishaps if the item is not edible.
$( mkIotaFragExpr
     "MageMouth"
     [pattern| WEST aaqqadaa |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Conjures a speck with the iota, position, and rotation. Costs a negligible amount of media. Pushes the speck iota to the stack.
$( mkIotaFragExpr
     "ConjureSpeck"
     [pattern| SOUTH_WEST ade |]
     [[t|forall a. '[IotaVector, IotaVector, a] -> '[IotaEntity]|]]
     -- ['vector', 'vector', 'any'] -> ['entity']
 )

-- Moves a speck entity to the position.
$( mkIotaFragExpr
     "MoveSpeck"
     [pattern| SOUTH_WEST adeqaa |]
     [[t|'[IotaVector, IotaEntity] -> '[]|]]
     -- ['vector', 'speck entity'] -> ['']
 )

-- Rotates a speck entity to face the vector.
$( mkIotaFragExpr
     "RotateSpeck"
     [pattern| SOUTH_WEST adeaw |]
     [[t|'[IotaVector, IotaEntity] -> '[]|]]
     -- ['vector', 'speck entity'] -> ['']
 )

-- Accepts a number between 0 and 1, representing a fraction of a full rotation, and rotates the speck\'s image by that amount.
$( mkIotaFragExpr
     "RollSpeck"
     [pattern| SOUTH_WEST adeqqqqq |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'speck entity'] -> ['']
 )

-- Changes the speck\'s iota.
$( mkIotaFragExpr
     "AlterSpeck"
     [pattern| SOUTH_WEST adeeaqa |]
     [[t|forall a. '[a, IotaEntity] -> '[]|]]
     -- ['any', 'speck entity'] -> ['']
 )

-- Commands the speck to disappear after that many twentieths of a second.
$( mkIotaFragExpr
     "TimeSpeck"
     [pattern| SOUTH_WEST adeqqaawdd |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'speck entity'] -> ['']
 )

-- Scales the speck, can range from 0 to 10. Represents size in blocks and is zero by default.
$( mkIotaFragExpr
     "ResizeSpeck"
     [pattern| SOUTH_WEST adeeqed |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'speck entity'] -> ['']
 )

-- Changes the stroke thickness of a pattern speck, can range from 0 to 10. Represents twentieths of a block, and is zero by default.
$( mkIotaFragExpr
     "ThickenSpeck"
     [pattern| SOUTH_WEST adeeqw |]
     [[t|'[IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'speck entity'] -> ['']
 )

-- Splashes a pigment iota onto the speck, changing its color after creation.
$( mkIotaFragExpr
     "PaintSpeck"
     [pattern| SOUTH_WEST adeqqaq |]
     [[t|'[IotaPigment, IotaEntity] -> '[]|]]
     -- ['pigment', 'speck entity'] -> ['']
 )

-- Forces a speck to disappear.
$( mkIotaFragExpr
     "DismissSpeck"
     [pattern| SOUTH_WEST adeaqde |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['speck entity'] -> ['']
 )

-- Targets structural faults in a block, resulting in a higher yield. Costs about one, three, and five Amethyst Dust when power input is 0, 1, and 2 respectively.
$( mkIotaFragExpr
     "ExtractBlock"
     [pattern| WEST qaqqqqqdeeeqeee |]
     [[t|'[IotaVector, IotaNumber] -> '[]|]]
     -- ['vector', 'number'] -> ['']
 )

-- Breaks a block gently, preserving the block better than standard mining. Costs about half of an Amethyst Dust.
$( mkIotaFragExpr
     "CollectBlock"
     [pattern| SOUTH_WEST aqaeaqdeeweweedq |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Returns specks similarly to other zone distillation patterns.
$( mkIotaFragExpr
     "ZoneDistillationSpecklike"
     [pattern| SOUTH_EAST qqqqqwdeddwqde |]
     [[t|'[IotaNumber, IotaVector] -> '[IotaList IotaEntity]|]]
     -- ['number', 'vector'] -> ['list of entities']
 )

-- Creates a loud bang similar to that of Fireworks and a burst of colorful particles, either in a direction or in a radial blast. Costs about half of one Amethyst Dust.
$( mkIotaFragExpr
     "Confetti"
     [pattern| EAST awddeqaedd |]
     [ [t|'[IotaNumber, IotaVector] -> '[]|]
     , [t|'[IotaVector, IotaVector] -> '[]|]
     ]
     -- ['number/vector', 'vector'] -> ['']
 )

-- Produces a small sparkling particle of my desired position, color, and lifespan. Costs a negligible amount of media.
$( mkIotaFragExpr
     "Sparkle"
     [pattern| NORTH_EAST dqa |]
     [[t|'[IotaNumber, IotaVector, IotaVector] -> '[]|]]
     -- ['number', 'vector', 'vector'] -> ['']
 )

-- Concentrates a little cube of media to be visible to the naked eye, useful for highlighting things to me and those around me. Costs a negligible amount of media.
$( mkIotaFragExpr
     "PingBlock"
     [pattern| NORTH_EAST dwwdwwdwewdwwdwwdeq |]
     [[t|'[IotaNumber, IotaVector, IotaVector] -> '[]|]]
     -- ['number', 'vector', 'vector'] -> ['']
 )

-- When casted on a casting device without a Hex, cracks it. Cracked casting devices proudly display their Hexes. Costs about one Charged Amethyst.
$( mkIotaFragExpr
     "CrackDevice"
     [pattern| EAST wwaqqqqqeqdedwqeaeqwdedwqeaeq |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Places a specific type of block from my inventory at a position, orientation, and horizontal orientation. Costs about one-fourth of one Amethyst Dust.
$( mkIotaFragExpr
     "Construct"
     [pattern| NORTH_WEST wewewewewewdwew |]
     [[t|'[IotaVector, IotaVector, IotaVector, IotaIdentifier] -> '[]|]]
     -- ['vec', 'vec', 'vec', 'identifier'] -> ['']
 )

-- Conjures a flower of my choosing at a location. I must have identified this flower at some point before. Costs about a quarter of an Amethyst Dust.
$( mkIotaFragExpr
     "ConjureFlower"
     [pattern| NORTH_EAST weqqqqqwaeaeaeaeaea |]
     [[t|'[IotaIdentifier, IotaVector] -> '[]|]]
     -- ['identifer', 'vector'] -> ['']
 )

-- Conjures a completely invisible light with illumination strength of my choosing at a location. Costs about a quarter of an Amethyst Dust.
$( mkIotaFragExpr
     "Illuminate"
     [pattern| SOUTH_EAST aeaeaeaeaeawqqqqq |]
     [[t|'[IotaNumber, IotaVector] -> '[]|]]
     -- ['number', 'vector'] -> ['']
 )

-- Instantly replenishes a creature\'s air bubbles. Costs about one Amethyst Dust.
$( mkIotaFragExpr
     "Gasp"
     [pattern| NORTH_WEST aweeeeewaweeeee |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['entity'] -> ['']
 )

-- Emits a sound associated with a mob near a location, as though a parrot had imitated that mob. Costs about a half of an Amethyst Dust.
$( mkIotaFragExpr
     "Squawk"
     [pattern| NORTH_EAST wweedadw |]
     [[t|'[IotaIdentifier, IotaVector] -> '[]|]]
     -- ['identifier', 'vector'] -> ['']
 )

-- Conjures an egg. May be fertile. Costs about two Amethyst Dust.
$( mkIotaFragExpr
     "ConjureEgg"
     [pattern| SOUTH_EAST qqqwaqaaqeeewdedde |]
     [[t|'[IotaVector] -> '[IotaEntity]|]]
     -- ['vector'] -> ['entity']
 )

-- Conjures a sticky low-damaging projectile. Costs about a fourth of one Amethyst Dust.
$( mkIotaFragExpr
     "ConjureSpit"
     [pattern| EAST dwqaqw |]
     [[t|'[IotaVector] -> '[IotaEntity]|]]
     -- ['vector'] -> ['entity']
 )

-- Conjures a harmless snowball. Costs about half of an Amethyst Dust.
$( mkIotaFragExpr
     "ConjureSnowball"
     [pattern| NORTH_EAST ddeeeeewd |]
     [[t|'[IotaVector] -> '[IotaEntity]|]]
     -- ['vector'] -> ['entity']
 )

-- Conjures an explosive fireball that can be percussively propelled. Costs about three Amethyst Dust.
$( mkIotaFragExpr
     "ConjureFireball"
     [pattern| SOUTH_EAST wqqqqqwaeaeaeaeae |]
     [[t|'[IotaVector] -> '[IotaEntity]|]]
     -- ['vector'] -> ['entity']
 )

-- Bestows clouding, an effect that impedes Hexcasting vision. Base cost is about one Amethyst Dust per three seconds. Follows nadir cost scaling.
$( mkIotaFragExpr
     "FoggySkysNadir"
     [pattern| SOUTH_EAST wddwaqqeawaeqwa |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

-- Bestows clarity, an effect that extends Hexcasting vision. Base cost is about one Amethyst Dust per second. Follows zenith cost scaling.
$( mkGreatIotaFragExpr
     "ClearSkysZenith"
     "Clear Sky's Zenith"
     [pattern| SOUTH_EAST wdwdqeeeeedwqwddwq |]
     [[t|'[IotaNumber, IotaNumber, IotaEntity] -> '[]|]]
     -- ['number', 'number', 'entity'] -> ['']
 )

-- Conjures a Hex Gummy: a delightful light snack that also provides about a tenth of an Amethyst Dust\'s worth of media. Costs about one Amethyst Dust.
$( mkIotaFragExpr
     "HexGummy"
     [pattern| SOUTH_WEST eeewdw |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Conjures a Hexburst of the given iota at the location. Costs about one Amethyst Dust and is subject to the Transgress Others mishap.
$( mkIotaFragExpr
     "ConjureHexburst"
     [pattern| EAST aadaadqaq |]
     [[t|forall a. '[a, IotaVector] -> '[]|]]
     -- ['any', 'vector'] -> ['']
 )

-- Conjures a Hextito of the given Hex at the location. Costs about two Amethyst Dust and is subject to the Transgress Others mishap.
$( mkIotaFragExpr
     "ConjureHextito"
     [pattern| EAST qaqdqaqdwawaw |]
     [[t|'[IotaList IotaPattern, IotaVector] -> '[]|]]
     -- ['list of patterns', 'vector'] -> ['']
 )

-- Conjures a Conjured Compass at the location pointing towards the second vector, linked to the current dimension. Costs about three Amethyst Dust.
$( mkIotaFragExpr
     "ConjureCompass"
     [pattern| SOUTH_WEST aqwawqwqqwqwq |]
     [[t|'[IotaVector, IotaVector] -> '[]|]]
     -- ['vector', 'vector'] -> ['']
 )

-- Takes a non-air position, an axis vector, and a delay in seconds up to ten seconds long and conjures a spike at that location. Costs about one Amethyst Shard.
$( mkIotaFragExpr
     "ConjureSpike"
     [pattern| NORTH_EAST qdqdqdqdww |]
     [[t|'[IotaNumber, IotaVector, IotaVector] -> '[]|]]
     -- ['number', 'vector', 'vector'] -> ['']
 )

-- Conjures a Tchotchke at the location with the amount of media and Hex. Costs about one Charged Amethyst and the media used for the battery.
$( mkIotaFragExpr
     "ConjureTchotchke"
     [pattern| NORTH_EAST wwwwwaqqqqqeaqeaeaeaeaeq |]
     [[t|'[IotaAnyList, IotaNumber, IotaNumber, IotaVector] -> '[]|]]
     -- ['list', 'num', 'num', 'vec'] -> ['']
 )

-- Writes an iota to the Tchotchke\'s inner iota storage, which can be read back on future casts. Subject to the Transgress Others mishap and unreadable from any external source.
$( mkIotaFragExpr
     "TchotchkeGambit"
     [pattern| NORTH_EAST waqqqqqedeqdqdqdqdqe |]
     [[t|forall a. '[a] -> '[]|]]
     -- ['any'] -> ['']
 )

-- Pushes the iota from the Tchotchke\'s inner iota storage.
$( mkIotaFragExpr
     "TchotchkeReflection"
     [pattern| NORTH_EAST waqqqqqeaqeaeaeaeaeq |]
     [[t|'[] -> '[IotaAny]|]]
     -- [''] -> ['any']
 )

-- Takes an item entity, a position, and an axis vector and dispenses the item. For most applications, costs about half of one Amethyst Dust.
$( mkIotaFragExpr
     "Dispense"
     [pattern| SOUTH_WEST wqwawqwddaeeead |]
     [[t|'[IotaVector, IotaVector, IotaEntity] -> '[]|]]
     -- ['vector', 'vector', 'item entity'] -> ['']
 )

-- Takes an item entity and cooks it as if in a Furnace. Costs about one Amethyst Dust for every ten seconds that the Furnace would need to have been active.
$( mkIotaFragExpr
     "Cook"
     [pattern| SOUTH_EAST qwqqadadadewewewe |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['item entity'] -> ['']
 )

-- A specialized variant of Cook that simulates a Campfire instead. This has a much more limited set of possibilities; I am unsure why I would ever use this.
$( mkIotaFragExpr
     "Roast"
     [pattern| NORTH_WEST aqqwwqqawdadedad |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['item entity'] -> ['']
 )

-- A specialized variant of Cook that simulates a Smoker instead, making it unable to process anything but foods but at a faster rate, making the spell cheaper.
$( mkIotaFragExpr
     "Smoke"
     [pattern| SOUTH_EAST qwqqadadadewdqqdwe |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['item entity'] -> ['']
 )

-- A specialized variant of Cook that simulates a Blast Furnace instead, making it unable to process anything but ores but at a faster rate, making the spell cheaper.
$( mkIotaFragExpr
     "Blast"
     [pattern| SOUTH_EAST qwqqadadadewweewwe |]
     [[t|'[IotaEntity] -> '[]|]]
     -- ['item entity'] -> ['']
 )

-- Takes an item entity and cuts the item into the shape of an item identifier. Costs about an eighth of one Amethyst Dust.
$( mkIotaFragExpr
     "CutStone"
     [pattern| EAST qqqqqwaeaeaeaeaeadawa |]
     [[t|'[IotaIdentifier, IotaEntity] -> '[]|]]
     -- ['identifier', 'item entity'] -> ['']
 )

-- Takes an item entity, an inventory location, and an axis vector. Inserts the item through that side if possible. Costs about one Amethyst Dust.
$( mkIotaFragExpr
     "Deposit"
     [pattern| SOUTH_EAST qwawqwaeqqq |]
     [[t|'[IotaVector, IotaVector, IotaEntity] -> '[]|]]
     -- ['vector', 'vector', 'item entity'] -> ['']
 )

-- Takes a spawning vector, an inventory location, and an axis vector. Extracts a single item from that side into the location and pushes it. Costs about one Amethyst Dust.
$( mkIotaFragExpr
     "Withdraw"
     [pattern| SOUTH_WEST qqqeawqwawq |]
     [[t|'[IotaVector, IotaVector, IotaVector] -> '[IotaEntity]|]]
     -- ['vec', 'vec', 'vec'] -> ['entity | null']
 )

-- Teleports an entity within the circle to any other point inside within the same circle. Unlike Greater Teleport, this spell takes in world coordinates rather than an offset. Free.
$( mkIotaFragExpr
     "Displace"
     [pattern| NORTH_EAST qaqqqqeedaqqqa |]
     [[t|'[IotaVector, IotaEntity] -> '[]|]]
     -- ['vector', 'entity'] -> ['']
 )

-- Etches a Hex, allowing me to cast it by holding Evoke for one second. Costs about one Charged Amethyst.
$( mkIotaFragExpr
     "Inculcate"
     [pattern| EAST wwaqqqqqeqdedwwqwqwwdedwwqwqw |]
     [[t|'[IotaList IotaPattern] -> '[]|]]
     -- ['list of patterns'] -> ['']
 )

-- Pushes the Hex etched into my mind.
$( mkIotaFragExpr
     "EvocationReflection"
     [pattern| EAST wwdeeeeeqeaqawwewewwaqawwewew |]
     [[t|'[] -> '[IotaList IotaPattern]|]]
     -- [''] -> ['list of patterns']
 )

-- Pushes how long since a player has been evoking. Is -1 if they are not currently evoking.
$( mkIotaFragExpr
     "EvokerReflection"
     [pattern| EAST wwaqqqqqeeaqawwewewwaqawwewew |]
     [[t|'[] -> '[IotaNumber]|]]
     -- [''] -> ['number']
 )

-- Dismisses any existing Lesser Sentinels and deploys a Lesser Sentinel at every position in the list.
$( mkIotaFragExpr
     "DeploySentinels"
     [pattern| EAST aeaae |]
     [[t|'[IotaList IotaVector] -> '[]|]]
     -- ['list of vectors'] -> ['']
 )

-- Returns a list of vectors corresponding to the positions of all my Lesser Sentinels.
$( mkIotaFragExpr
     "InfiltrationReflection"
     [pattern| WEST dqddq |]
     [[t|'[] -> '[IotaList IotaVector]|]]
     -- [''] -> ['list of vectors']
 )

-- Analyzes the Firework Star in my other hand and conjures a firework of that star, with the position, velocity, and gunpowder amount specified. Costs about one Amethyst Shard.
$( mkIotaFragExpr
     "SimulateFirework"
     [pattern| SOUTH_WEST dedwaqwqqwqa |]
     [[t|'[IotaNumber, IotaVector, IotaVector] -> '[]|]]
     -- ['number', 'vector', 'vector'] -> ['']
 )

-- Conjures a firework of my exact specifications. It may be one of the most complex spells in existence. Costs equivalently to the other firework spell.
$( mkIotaFragExpr
     "ConjureFirework"
     [pattern| SOUTH_WEST dedwaqwwawwqa |]
     [ [t|
         '[IotaBoolean, IotaBoolean, IotaList IotaDye, IotaList IotaDye, IotaNumber, IotaNumber, IotaVector, IotaVector] -> '[]
         |]
     ]
     -- ['bool', 'bool', 'list of dyes', 'list of dyes', 'num', 'num', 'vec', 'vec'] -> ['']
 )

-- Breaks any shader spell currently applied to me. Useful for "bleaching" my eyes after too much experimentation.
$( mkIotaFragExpr
     "ClearVision"
     [pattern| WEST eeeeeqaqeeeee |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Greatly augments my ability to see in the dark, although the light resultingly becomes extremely blinding.
$( mkIotaFragExpr
     "PierceDarkness"
     [pattern| WEST edewawede |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Transforms the world into a thin outline of black and white. Possibly useful for identifying subtle contrasts.
$( mkIotaFragExpr
     "VisualizeForms"
     [pattern| WEST eedwwawwdee |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Alters my vision to contain strange lines and make objects towards the center of my vision bulge "towards" me. Seems to be in reference to something...
$( mkIotaFragExpr
     "BroadcastVision"
     [pattern| WEST wewdwewwawwewdwew |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Transforms my vision to perceive only the most important things...
$( mkIotaFragExpr
     "IdentifyImportance"
     [pattern| WEST eewdweqaqewdwee |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Splits my vision into multiple sections, similar to what a spider might see. I am unsure whether it actually grants me more vision...
$( mkIotaFragExpr
     "SplitVision"
     [pattern| NORTH_EAST qaqdedaedqqdedaqaedeqd |]
     [[t|'[] -> '[]|]]
     -- [] -> []
 )

-- Judging by the pattern signature, I can only imagine that it summons something.
$( mkIotaFragExpr
     "Summon"
     [pattern| EAST wedqawqeewdeaqeewdeaqqedqawqqedqawqeedqawqqewdeaqeedqawqeewdeaqqewdeaqeewdeaqeedqawqqedqawqqewdeaqeedqawqeewdeaqqewdeaqeewdeaqeedqawqqedqawqqewdeaqqedqawqeewdeaqeewdeaqqedqawqqedqawqeedqawqqewdeaqqedqawqeewdeaqeewdeaqqedqawqqedqawqeedqawqqewdeaqeedqawqeewdeaqeewdeaqqedqawqqedqawqeedqawqqewdeaqqedqawqeewdeaqqewdeaqeewdeaqeedqawqqedqawqqewdeaqe |]
     [[t|'[IotaVector] -> '[IotaEntity]|]]
     -- ['vector'] -> ['entity']
 )

-- Teleports me up to 128 blocks away, relative to my position and rotation. Costs about two Amethyst Dust.
$( mkGreatIotaFragExpr
     "GreaterBlink"
     "Greater Blink"
     [pattern| SOUTH_WEST wqawawaqwqwqawawaqw |]
     [[t|'[IotaVector] -> '[]|]]
     -- ['vector'] -> ['']
 )

-- Conjures a mesh with the location and leaves a mesh entity on the stack. Costs about one Amethyst Dust.
$( mkGreatIotaFragExpr
     "ConjureMesh"
     "Conjure Mesh"
     [pattern| EAST qaqqqqqwqqqdeeweweeaeewewee |]
     [[t|'[IotaVector] -> '[IotaEntity]|]]
     -- ['vector'] -> ['entity']
 )

-- Takes a list of vectors, with constraints defined on the prior page, and weaves the mesh into that shape. Free.
$( mkIotaFragExpr
     "WeaveMesh"
     [pattern| EAST qaqqqqqwqqqdeewewee |]
     [[t|'[IotaList IotaVector, IotaEntity] -> '[]|]]
     -- ['list of vectors', 'entity'] -> ['']
 )

-- Pushes the shape of a mesh as a list of vectors.
$( mkIotaFragExpr
     "TanglePurification"
     [pattern| SOUTH_WEST edeeeeeweeeaqqwqwqq |]
     [[t|'[IotaEntity] -> '[IotaList IotaVector]|]]
     -- ['entity'] -> ['list of vectors']
 )
