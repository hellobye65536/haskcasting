{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Haskcasting.Serialize.A (
  SerializeOptions (..),
  defaultSerializeOptions,
  Inst (..),
  patternToStrokes,
  strokeChars,
  serialize,
) where

import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (sort, elemIndex)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Unboxed qualified as VU
import Haskcasting.Pattern (Pattern (Pattern), pattern)

data SerializeOptions = SerializeOptions
  { soGreatSpells :: HashMap Text Pattern
  , soBootstrap :: Bool
  , soMaxLineLength :: Int
  , soSuspendHoisting :: Bool
  }

defaultSerializeOptions :: SerializeOptions
defaultSerializeOptions =
  SerializeOptions
    { soGreatSpells = HM.empty
    , soBootstrap = False
    , soMaxLineLength = 250
    , soSuspendHoisting = False
    }

data Inst
  = IPattern Pattern
  | IMergeN Int
  | IString Text
  | ISuspend Text
  | INumber Double
  | IVector Double Double Double
  | IExec Int Int
  | INull
  | IBool Bool
  | -- abstract instructions
    ITagAsSuspend Text
  | -- bootstrap instructions
    IBootstrapSemi
  | IBootstrapHalt
  deriving (Eq, Show)

data VmInst
  = VmPattern Pattern
  | VmMergeN Int
  | VmString Text
  | VmSuspend Text
  | VmNumber Double
  | VmVector Double Double Double
  | VmExec Int Int
  | VmReuse Int
  | VmIntrinsicConsideration
  | VmIntrinsicIntrospection
  | VmIntrinsicRetrospection
  | -- bootstrap instructions
    VmBootstrapSemi
  | VmBootstrapHalt

patternToStrokes :: Pattern -> [Int]
patternToStrokes (Pattern dir ang) = NE.toList $ NE.reverse $ foldl' go (NE.singleton $ fromEnum dir) ang
 where
  go ls@(d NE.:| _) a = (fromEnum d + fromEnum a) `rem` 6 NE.<| ls

patConsideration, patIntrospection, patRetrospection :: Pattern
patConsideration = [pattern| WEST qqqaw |]
patIntrospection = [pattern| WEST qqq |]
patRetrospection = [pattern| EAST eee |]

intrinsicPatterns :: [(Pattern, VmInst)]
intrinsicPatterns =
  [ (patConsideration, VmIntrinsicConsideration)
  , (patIntrospection, VmIntrinsicIntrospection)
  , (patRetrospection, VmIntrinsicRetrospection)
  ]

data VmStackElem
  = VmsPattern Pattern
  | VmsSuspend Text
  | VmsString Text
  | VmsUnknown
  deriving (Eq)

convertInsts :: SerializeOptions -> [Inst] -> [VmInst]
convertInsts opt = go [] . (if suspendHoist then hoistSuspends else id)
 where
  bootstrap = soBootstrap opt
  suspendHoist = soSuspendHoisting opt
  hoistSuspends is = suspends <> is
   where
    suspends =
      map (ISuspend . NE.head)
        . NE.group
        . sort
        . mapMaybe (\case ISuspend tag -> Just tag; _ -> Nothing)
        $ is
  lower = if bootstrap then lowerInstBootstrap else lowerInstReuse
  go _stack [] = []
  go stack (i : is) =
    let vmis = lower stack i
     in vmis <> go (simInst stack i) is

simInst :: [VmStackElem] -> Inst -> [VmStackElem]
simInst st = \case
  IPattern pat -> VmsPattern pat : st
  IMergeN n -> VmsUnknown : drop n st
  IString s -> VmsString s : st
  ISuspend tag -> VmsSuspend tag : st
  INumber _n -> VmsUnknown : st
  IVector _x _y _z -> VmsUnknown : st
  IExec i o -> replicate o VmsUnknown <> drop (i + 1) st
  INull -> VmsUnknown : st
  IBool _b -> VmsUnknown : st
  -- abstract instructions
  ITagAsSuspend tag -> VmsSuspend tag : drop 1 st
  -- bootstrap instructions
  IBootstrapSemi -> VmsUnknown : st
  IBootstrapHalt -> VmsUnknown : st

lowerInstReuse, lowerInst, lowerInstBootstrap :: [VmStackElem] -> Inst -> [VmInst]
lowerInstReuse stack = \case
  ISuspend tag
    | Just sti <- elemIndex (VmsSuspend tag) stack ->
        [VmReuse sti]
  IPattern pat@(Pattern _ang dir)
    | Just sti <- elemIndex (VmsPattern pat) stack
    , length (show sti) < ((length dir + 1) `quot` 2) ->
        [VmReuse sti]
  IString str
    | Just sti <- elemIndex (VmsString str) stack
    , length (show sti) < T.length str ->
        [VmReuse sti]
  i -> lowerInst stack i
lowerInst stack = \case
  -- intrinsic iotas
  IPattern pat | Just vmInst <- lookup pat intrinsicPatterns -> [vmInst]
  --
  ISuspend tag -> [VmSuspend tag]
  INumber n -> [VmNumber n]
  IVector x y z -> [VmVector x y z]
  IExec i o -> [VmExec i o]
  INull -> [VmPattern [pattern| EAST d |], VmExec 0 1]
  IBool b ->
    let p = if b then [pattern| SOUTH_EAST aqae |] else [pattern| NORTH_EAST dedq |]
     in [VmPattern p, VmExec 0 1]
  i -> lowerInstBootstrap stack i
lowerInstBootstrap _stack = \case
  IPattern pat -> [VmPattern pat]
  IMergeN n -> [VmMergeN n]
  IString s -> [VmString s]
  IBootstrapSemi -> [VmBootstrapSemi]
  IBootstrapHalt -> [VmBootstrapHalt]
  --
  ITagAsSuspend _tag -> []
  --
  i -> error $ "instruction '" <> show i <> "' not supported during bootstrap"

strokeChars :: VU.Vector Char
strokeChars = VU.fromList "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef"

serializeVmInst :: SerializeOptions -> VmInst -> Text
serializeVmInst opt = \case
  VmPattern pat -> "0;" <> patToStr pat
  VmMergeN n -> "1;" <> T.show n
  VmString s -> "2;" <> noSemi "string" s
  VmSuspend tag -> "3;" <> noSemi "tag" tag
  VmNumber n -> "4;" <> showNum n
  VmVector x y z -> "5;" <> showNum x <> "," <> showNum y <> "," <> showNum z
  VmExec _i _o -> "6"
  VmReuse n -> "7;" <> T.show n
  VmIntrinsicConsideration -> "8"
  VmIntrinsicIntrospection -> "9"
  VmIntrinsicRetrospection -> "10"
  --
  VmBootstrapSemi -> "3"
  VmBootstrapHalt -> "4"
 where
  bootstrap = soBootstrap opt
  patToStr = T.pack . map (strokeChars VU.!) . (if bootstrap then id else groupStrokes) . patternToStrokes
  groupStrokes [] = []
  groupStrokes [a] = [a]
  groupStrokes (a : b : as) = (6 + a + 6 * b) : groupStrokes as
  noSemi msg tx =
    if T.elem ';' tx
      then error (msg <> ": '" <> T.unpack tx <> "' contains a semicolon")
      else tx
  showNum n =
    if fromIntegral @Int (round n) == n
      then T.show $ (round n :: Int)
      else T.show n

serialize :: SerializeOptions -> Seq Inst -> [Text]
serialize opt = reverse . foldl' go [] . map (serializeVmInst opt) . convertInsts opt . toList
 where
  go [] s = [s]
  go allouts@(out : outs) s =
    let out' = out <> ";" <> s
     in if T.length out' > maxLength
          then s : allouts
          else out' : outs
  maxLength = soMaxLineLength opt
