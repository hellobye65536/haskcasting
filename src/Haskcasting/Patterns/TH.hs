{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Patterns.TH (angles, pattern, mkIotaFrag) where

import Data.Char (toLower, toUpper)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Haskcasting.Fragment (Fragment (Fragment))
import Haskcasting.Iota (Angle (..), Direction (..), IotaCast (iotaCast), IotaPattern (IotaPattern))
import Language.Haskell.TH (
  BndrVis (BndrReq),
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
import Language.Haskell.TH.Syntax (Lift (lift))

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

mkIotaFrag :: forall m. Quote m => String -> IotaPattern -> [m Type] -> m [Dec]
mkIotaFrag name pat types = iotaDecls <<>> fragDecls
 where
  iotaName = mkName $ "iota" <> name
  fragName = mkName $ "frag" <> name
  iotaDecls =
    sequenceA
      [ sigD iotaName (conT ''IotaPattern)
      , valD (varP iotaName) (normalB (lift pat)) []
      ]
  fragDecls = case types of
    [] -> pure []
    [ty] ->
      sequenceA
        [ sigD fragName ty
        , valD (varP fragName) (normalB $ fragExpr iotaName) []
        ]
    tys ->
      let
        className = mkName $ "Frag" <> name
        classDecl = do
          tas <- newName "as"
          tbs <- newName "bs"
          inner <- sigD fragName [t|Fragment $(varT tas) $(varT tbs)|]
          let vars = map (\ts -> PlainTV ts BndrReq) [tas, tbs]
          pure $ [ClassD [] className vars [FunDep [tas] []] [inner]]
       in
        classDecl <<>> mkFrag name tys

mkFrag :: Quote m => String -> [m Type] -> m [Dec]
mkFrag name types = traverse go types
 where
  iotaName = mkName $ "iota" <> name
  fragName = mkName $ "frag" <> name
  className = mkName $ "Frag" <> name
  go ty_ = do
    ty <- ty_
    let (_vars, cxt, (tas, tbs)) = matchConstraint ty
    impl <- [t|$(conT className) $(pure tas) $(pure tbs)|]
    decl <- valD (varP fragName) (normalB $ fragExpr iotaName) []
    pure $ InstanceD Nothing cxt impl [decl]
  matchConstraint = \case
    ForallT vars cxt inner -> (vars, cxt, matchFragment inner)
    ty -> ([], [], matchFragment ty)
  matchFragment = \case
    AppT (AppT _frag tas) tbs -> (tas, tbs)
    ty -> error $ "invalid type: '" <> show ty <> "'"
