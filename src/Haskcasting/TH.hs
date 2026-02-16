{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.TH (
  angles,
  pattern,
  mkIotaFrag,
  mkGreatIotaFrag,
  mkFrag,
) where

import Data.Char (toLower, toUpper)
import Data.List (dropWhileEnd, unfoldr)
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Haskcasting.Fragment (Fragment (Fragment))
import Haskcasting.Iota (Angle (..), Direction (..), IotaCast (iotaCast), IotaPattern (IotaPattern), IotaGreatPattern)
import Language.Haskell.TH (
  BndrVis (BndrReq),
  Cxt,
  Dec (..),
  Exp (..),
  FunDep (FunDep),
  Name,
  Quote,
  TyVarBndr (PlainTV),
  Type (..),
  conT,
  mkName,
  newName,
  normalB,
  sigD,
  valD,
  varE,
  varP,
  varT,
 )
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift (lift), Specificity)

isAsciiWhitespace :: Char -> Bool
isAsciiWhitespace c = '\9' <= c && c <= '\13' || c == ' '

angleMap :: [(Char, Name)]
angleMap =
  [ ('w', 'AngleW)
  , ('e', 'AngleE)
  , ('d', 'AngleD)
  , ('s', 'AngleS)
  , ('a', 'AngleA)
  , ('q', 'AngleQ)
  ]

parseAngles :: Quote m => String -> m Exp
parseAngles xs = pure $ ListE $ concatMap go xs
 where
  go c
    | isAsciiWhitespace c = []
    | Just ident <- lookup (toLower c) angleMap = [ConE ident]
    | otherwise = error $ "invalid char '" <> [c] <> "'"

directionMap :: [(String, Name)]
directionMap =
  [ ("NORTH_EAST", 'DirectionNE)
  , ("EAST", 'DirectionE)
  , ("SOUTH_EAST", 'DirectionSE)
  , ("SOUTH_WEST", 'DirectionSW)
  , ("WEST", 'DirectionW)
  , ("NORTH_WEST", 'DirectionNW)
  ]

parseDirection :: Quote m => String -> m Exp
parseDirection x =
  pure $
    ConE $
      fromMaybe (error $ "invalid direction: '" <> x' <> "'") $
        flip lookup directionMap $
          map toUpper x'
 where
  x' = dropWhileEnd isAsciiWhitespace $ dropWhile isAsciiWhitespace x

parsePattern :: Quote m => String -> m Exp
parsePattern x = apply (ConE 'IotaPattern) <$> sequenceA [parseDirection dir, parseAngles ang]
 where
  apply fn = foldl AppE fn
  x' = dropWhile isAsciiWhitespace x
  (dir, ang) = break isAsciiWhitespace x'

angles :: QuasiQuoter
angles =
  QuasiQuoter {quoteDec = err "declaration", quoteType = err "type", quoteExp = parseAngles, quotePat = err "pattern"}
 where
  err p = error $ "bad position: " <> p

pattern :: QuasiQuoter
pattern =
  QuasiQuoter {quoteDec = err "declaration", quoteType = err "type", quoteExp = parsePattern, quotePat = err "pattern"}
 where
  err p = error $ "bad position: " <> p

infixr 6 <<>>
(<<>>) :: (Applicative m, Monoid a) => m a -> m a -> m a
(<<>>) = liftA2 (<>)

fragExpr :: Quote m => Name -> m Exp
fragExpr iotaName = [e|Fragment $ Seq.singleton $ iotaCast $(varE iotaName)|]

parseFragType :: Type -> ([TyVarBndr Specificity], Cxt, ([Type], [Type]))
parseFragType = \case
  ForallT vars cxt inner -> (vars, cxt, parseFragApp inner)
  ty -> ([], [], parseFragApp ty)

parseFragApp :: Type -> ([Type], [Type])
parseFragApp = \case
  AppT (AppT _frag tas) tbs -> (parseTypeList tas, parseTypeList tbs)
  ty -> error $ "invalid type: '" <> show ty <> "'"

parseTypeList :: Type -> [Type]
parseTypeList = unfoldr $ \case
  AppT (AppT PromotedConsT x) xs -> Just (x, xs)
  PromotedNilT -> Nothing
  ty -> error $ "invalid type: '" <> show ty <> "'"

toTypeList :: Type -> [Type] -> Type
toTypeList = foldr (\t ts -> (AppT (AppT PromotedConsT t) ts))

mkIotaFragImpl :: (Quote m, Lift p) => Type -> String -> p -> [m Type] -> m [Dec]
mkIotaFragImpl iotaType name pat types = iotaDecls <<>> classDecls <<>> implDecls
 where
  iotaName = mkName $ "iota" <> name
  fragName = mkName $ "frag" <> name
  className = mkName $ "Frag" <> name
  iotaDecls =
    sequenceA
      [ pure $ SigD iotaName iotaType
      , valD (varP iotaName) (normalB (lift pat)) []
      ]
  classDecls = do
    tas <- newName "as"
    tbs <- newName "bs"
    inner <-
      sequenceA
        [ sigD fragName [t|Fragment $(varT tas) $(varT tbs)|]
        , valD (varP fragName) (normalB $ fragExpr iotaName) []
        ]
    let vars = map (\ts -> PlainTV ts BndrReq) [tas, tbs]
    pure $ [ClassD [] className vars [FunDep [tas] [tbs]] inner]
  implDecls = mkFrag name types

mkIotaFrag :: Quote m => String -> IotaPattern -> [m Type] -> m [Dec]
mkIotaFrag = mkIotaFragImpl (ConT ''IotaPattern)

mkGreatIotaFrag :: Quote m => String -> IotaGreatPattern -> [m Type] -> m [Dec]
mkGreatIotaFrag = mkIotaFragImpl (ConT ''IotaGreatPattern)

mkFrag :: Quote m => String -> [m Type] -> m [Dec]
mkFrag name types = traverse go types
 where
  className = mkName $ "Frag" <> name
  go ty_ = do
    ty <- ty_
    rest <- newName "as"
    let (_vars, cxt, (tas, tbs)) = parseFragType ty
    impl <- [t|$(conT className) $(pure $ toTypeList (VarT rest) tas) $(pure $ toTypeList (VarT rest) tbs)|]
    pure $ InstanceD Nothing cxt impl []
