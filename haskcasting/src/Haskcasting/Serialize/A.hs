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
import Data.List (elemIndex, sort, (!?))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, mapMaybe)
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
  | --
    IBootstrapSemi
  | IBootstrapHalt
  deriving (Eq)

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
  | --
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

simVmInst :: [VmStackElem] -> VmInst -> [VmStackElem]
simVmInst st = \case
  VmPattern pat -> VmsPattern pat : st
  VmMergeN n -> VmsUnknown : drop n st
  VmString s -> VmsString s : st
  VmSuspend tag -> VmsSuspend tag : st
  VmNumber _n -> VmsUnknown : st
  VmVector _x _y _z -> VmsUnknown : st
  VmExec i o -> replicate o VmsUnknown <> drop (i + 1) st
  VmReuse n -> fromMaybe VmsUnknown (st !? n) : st
  VmIntrinsicConsideration -> VmsPattern patConsideration : st
  VmIntrinsicIntrospection -> VmsPattern patIntrospection : st
  VmIntrinsicRetrospection -> VmsPattern patRetrospection : st
  --
  VmBootstrapSemi -> VmsUnknown : st
  VmBootstrapHalt -> VmsUnknown : st

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
  go _stack [] = []
  go stack is = let (vis, is') = inner stack is in vis <> go (foldl' simVmInst stack vis) is'
  inner = if bootstrap then innerBootstrap else innerNormal
  innerNormal stack is
    -- intrinsic patterns
    | (IPattern pat : is') <- is
    , Just vmInst <- lookup pat intrinsicPatterns =
        ([vmInst], is')
    -- stack reuse
    | (IPattern pat : is') <- is
    , Just sti <- elemIndex (VmsPattern pat) stack =
        ([VmReuse sti], is')
    | (ISuspend tag : is') <- is
    , Just sti <- elemIndex (VmsSuspend tag) stack =
        ([VmReuse sti], is')
    --
    | (ISuspend tag : is') <- is =
        ([VmSuspend tag], is')
    | (INumber n : is') <- is =
        ([VmNumber n], is')
    | (IVector x y z : is') <- is =
        ([VmVector x y z], is')
    | (IExec i o : is') <- is =
        ([VmExec i o], is')
    | (INull : is') <- is =
        ([VmPattern [pattern| EAST d |], VmExec 0 1], is')
    | (IBool b : is') <- is =
        let p = if b then [pattern| SOUTH_EAST aqae |] else [pattern| NORTH_EAST dedq |]
         in ([VmPattern p, VmExec 0 1], is')
    | otherwise = innerBootstrap stack is
  innerBootstrap _stack is
    | (IPattern pat : is') <- is =
        ([VmPattern pat], is')
    | (IMergeN n : is') <- is =
        ([VmMergeN n], is')
    | (IString s : is') <- is =
        ([VmString s], is')
    | (IBootstrapSemi : is') <- is =
        ([VmBootstrapSemi], is')
    | (IBootstrapHalt : is') <- is =
        ([VmBootstrapHalt], is')
    | [] <- is = error "empty instructions"
    | otherwise = error "instruction not supported in bootstrap mode"

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
