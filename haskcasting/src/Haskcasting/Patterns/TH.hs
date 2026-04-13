{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Haskcasting.Patterns.TH (
  mkIotaFrag,
  mkGreatIotaFrag,
  mkFrag,
  mkFragInstance,
  -- exported helper functions
  iotaName,
  fragClassName,
  fragName,
  --
  ParsedFragType,
  ParsedFragIntroType,
  parseFragType,
  toTypeList,
) where

import Language.Haskell.TH (
  BndrVis (BndrReq),
  Cxt,
  Dec,
  FunDep (FunDep),
  Quote (..),
  Specificity (SpecifiedSpec),
  TyVarBndr (PlainTV),
  Type (AppT, ForallT, PromotedConsT, PromotedNilT, VarT),
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
import Language.Haskell.TH.Syntax (Lift (lift))

import Data.List (singleton, unfoldr)
import Data.Text qualified as T

import Control.Monad ((<=<))
import Haskcasting.Fragment (Fragment, fragSingleton)
import Haskcasting.Iota (
  IotaGreatPattern (IotaGreatPattern),
  IotaPattern (IotaPattern),
 )
import Haskcasting.Pattern (Pattern)

infixr 6 <<>>
(<<>>) :: (Applicative m, Monoid a) => m a -> m a -> m a
(<<>>) = liftA2 (<>)

type ParsedFragType = ([TyVarBndr Specificity], Cxt, ([Type], [Type]))
type ParsedFragIntroType = ([TyVarBndr Specificity], Cxt, [Type])

parseFragType :: Type -> ParsedFragType
parseFragType = \case
  ForallT vars cxt inner -> (vars, cxt, parseFragApp inner)
  ty -> ([], [], parseFragApp ty)

parseFragApp :: Type -> ([Type], [Type])
parseFragApp = \case
  _frag `AppT` tas `AppT` tbs -> (parseTypeList tas, parseTypeList tbs)
  ty -> error $ "invalid type: '" <> show ty <> "'"

parseTypeList :: Type -> [Type]
parseTypeList = unfoldr $ \case
  PromotedConsT `AppT` x `AppT` xs -> Just (x, xs)
  PromotedNilT -> Nothing
  ty -> error $ "invalid type: '" <> show ty <> "'"

toTypeList :: Type -> [Type] -> Type
toTypeList = foldr (\t ts -> PromotedConsT `AppT` t `AppT` ts)

iotaName, fragName, fragClassName :: String -> String
iotaName = ("iota" <>)
fragName = ("frag" <>)
fragClassName = ("Frag" <>)

mkIotaImpl :: (Quote m, Lift a) => m Type -> String -> a -> m [Dec]
mkIotaImpl ty name v =
  sequenceA
    [ sigD iotaIdent ty
    , valD (varP iotaIdent) (normalB (lift v)) []
    ]
 where
  iotaIdent = mkName $ iotaName name

fragDecl :: Quote m => String -> m Dec
fragDecl name = valD (varP fragIdent) (normalB [e|fragSingleton $(varE iotaIdent)|]) []
 where
  iotaIdent = mkName $ iotaName name
  fragIdent = mkName $ fragName name

mkFragClassImpl :: Quote m => String -> m [Dec]
mkFragClassImpl name = fmap singleton $ do
  tas <- newName "as"
  tbs <- newName "bs"
  let vars = map (\ts -> PlainTV ts BndrReq) [tas, tbs]
  classD (pure []) fragClassIdent vars [FunDep [tas] [tbs]] $
    [ sigD fragIdent [t|Fragment $(varT tas) $(varT tbs)|]
    , fragDecl name
    ]
 where
  fragIdent = mkName $ fragName name
  fragClassIdent = mkName $ fragClassName name

mkFragImpl :: Quote m => String -> ParsedFragType -> m [Dec]
mkFragImpl name (_vars, cxt, (tas, tbs)) =
  fmap singleton $ do
    rest <- newName "s"
    let toTypeList' = pure . toTypeList (VarT rest)
    instanceD
      (pure cxt)
      [t|$(conT fragClassIdent) $(toTypeList' tas) $(toTypeList' tbs)|]
      []
 where
  fragClassIdent = mkName $ fragClassName name

mkFragIntroImpl :: Quote m => String -> ParsedFragIntroType -> m [Dec]
mkFragIntroImpl name (vars, cxt, ts) = do
  rest <- newName "s"
  let toTypeList' = pure . toTypeList (VarT rest)
  sequenceA
    [ sigD fragIdent $ forallT (vars ++ [PlainTV rest SpecifiedSpec]) (pure cxt) [t|Fragment $(varT rest) $(toTypeList' ts)|]
    , fragDecl name
    ]
 where
  fragIdent = mkName $ fragName name

mkIotaFrag :: Quote m => String -> Pattern -> [m Type] -> m [Dec]
mkIotaFrag name pat types =
  mkIotaImpl (conT ''IotaPattern) name (IotaPattern pat)
    <<>> mkFrag name types

mkGreatIotaFrag :: Quote m => String -> String -> Pattern -> [m Type] -> m [Dec]
mkGreatIotaFrag name tag pat types =
  mkIotaImpl (conT ''IotaGreatPattern) name (IotaGreatPattern (T.pack tag) pat)
    <<>> mkFrag name types

mkFrag :: Quote m => String -> [m Type] -> m [Dec]
mkFrag name types_ = do
  types <- sequenceA types_
  let parsed = map parseFragType types
  case parsed of
    [(vars, cxt, ([], ts))] -> mkFragIntroImpl name (vars, cxt, ts)
    _ -> mkFragClassImpl name <<>> mkFragInstance name types_

mkFragInstance :: Quote m => String -> [m Type] -> m [Dec]
mkFragInstance name = fmap concat . traverse (mkFragImpl name . parseFragType) <=< sequenceA
