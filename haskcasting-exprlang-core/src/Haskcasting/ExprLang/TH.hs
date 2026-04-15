{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.ExprLang.TH (
  mkIotaFragExpr,
  mkGreatIotaFragExpr,
  mkExpr,
  mkFragExpr,
  mkExprInstance,
  mkFragExprInstance,
  -- exported helper functions
  exprClassName,
  exprName,
) where

import Language.Haskell.TH (
  BndrVis (BndrReq),
  Dec,
  FunDep (FunDep),
  Quote (..),
  TyVarBndr (PlainTV),
  Type (PromotedNilT),
  classD,
  conT,
  forallT,
  instanceD,
  mkName,
  normalB,
  sigD,
  valD,
  varE,
  varP,
  varT,
 )

import Data.List (singleton)
import Data.Sequence qualified as Seq

import Haskcasting.ExprLang.Core (Expr, callUnsafe, introUnsafe)
import Haskcasting.Iota (IotaCast (iotaCast))
import Haskcasting.Pattern (Pattern)
import Haskcasting.Patterns.TH (
  ParsedFragIntroType,
  ParsedFragType,
  iotaName,
  mkFrag,
  mkFragInstance,
  mkGreatIotaFrag,
  mkIotaFrag,
  parseFragType,
  toTypeList,
 )
import Haskcasting.Util (HListLen)

infixr 6 <<>>
(<<>>) :: (Applicative m, Monoid a) => m a -> m a -> m a
(<<>>) = liftA2 (<>)

exprClassName, exprName :: String -> String
exprClassName = ("Expr" <>)
exprName = ("expr" <>)

mkExprClassImpl :: Quote m => String -> m [Dec]
mkExprClassImpl name = fmap singleton $ do
  nas <- newName "as"
  nbs <- newName "bs"
  nblk <- newName "blk"
  let tblk = varT nblk
      tas = varT nas
      tbs = varT nbs
  let vars = map (\ts -> PlainTV ts BndrReq) [nas, nbs]
  classD (sequenceA [[t|HListLen $tbs|]]) exprClassIdent vars [FunDep [nas] [nbs]] $
    [ sigD exprIdent [t|Expr $tblk $tas -> Expr $tblk $tbs|]
    , valD (varP exprIdent) (normalB [e|callUnsafe $ Seq.singleton $ iotaCast $(varE iotaIdent)|]) []
    ]
 where
  iotaIdent = mkName $ iotaName name
  exprClassIdent = mkName $ exprClassName name
  exprIdent = mkName $ exprName name

mkExprImpl :: Quote m => String -> ParsedFragType -> m [Dec]
mkExprImpl name (_vars, cxt, (tas, tbs)) = fmap singleton $ do
  let toTypeList' = pure . toTypeList PromotedNilT
      ta = toTypeList' tas
      tb = toTypeList' tbs
  instanceD (pure cxt) [t|$(conT exprClassIdent) $ta $tb|] []
 where
  exprClassIdent = mkName $ exprClassName name

mkExprIntroImpl :: Quote m => String -> ParsedFragIntroType -> m [Dec]
mkExprIntroImpl name (vars, cxt, ts) = do
  nblk <- newName "blk"
  let tblk = varT nblk
      toTypeList' = pure . toTypeList PromotedNilT
      t = toTypeList' ts
  sequenceA
    [ sigD exprIdent $ forallT vars (pure cxt) [t|Expr $tblk $t|]
    , valD (varP exprIdent) (normalB [e|introUnsafe $ Seq.singleton $ iotaCast $(varE iotaIdent)|]) []
    ]
 where
  iotaIdent = mkName $ iotaName name
  exprIdent = mkName $ exprName name

mkIotaFragExpr :: Quote m => String -> Pattern -> [m Type] -> m [Dec]
mkIotaFragExpr name pat types =
  mkIotaFrag name pat types
    <<>> mkExpr name types

mkGreatIotaFragExpr :: Quote m => String -> String -> Pattern -> [m Type] -> m [Dec]
mkGreatIotaFragExpr name tag pat types =
  mkGreatIotaFrag name tag pat types
    <<>> mkExpr name types

mkExpr :: Quote m => String -> [m Type] -> m [Dec]
mkExpr name types_ = do
  types <- sequenceA types_
  let parsed = map parseFragType types
  case parsed of
    [(vars, cxt, ([], ts))] -> mkExprIntroImpl name (vars, cxt, ts)
    _ -> mkExprClassImpl name <<>> mkExprInstance name types_

mkFragExpr :: Quote m => String -> [m Type] -> m [Dec]
mkFragExpr name types = mkFrag name types <<>> mkExpr name types

mkExprInstance :: Quote m => String -> [m Type] -> m [Dec]
mkExprInstance name types = fmap concat $ traverse (mkExprImpl name . parseFragType) =<< sequenceA types

mkFragExprInstance :: Quote m => String -> [m Type] -> m [Dec]
mkFragExprInstance name types = mkFragInstance name types <<>> mkExprInstance name types
