{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Haskcasting.Iota (
  Iota (..),
  IotaCast (..),
  IotaTryCast (..),
  IotaAny,
  IotaNull (..),
  IotaBoolean (..),
  IotaNumber (..),
  IotaVector (..),
  IotaEntity (..),
  Direction (..),
  directionShow,
  directionParse,
  Angle (..),
  angleShow,
  angleParse,
  IotaPattern (..),
  IotaGreatPattern (..),
  IotaList (..),
  pattern IotaHNil,
  pattern IotaHCons,
  IotaAnyList,
  IotaHList (..),
) where

import Data.HList (HList (HCons, HNil))
import Data.Sequence (Seq (..))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Language.Haskell.TH.Syntax qualified as TH
import Type.Reflection (TypeRep, Typeable, eqTypeRep, typeOf, typeRep, type (:~~:) (HRefl))

class Iota a where
  iotaShow :: a -> Text

class (Iota a, Iota b) => IotaCast a b where
  iotaCast :: a -> b
class (Iota a, Iota b) => IotaTryCast a b where
  iotaTryCast :: a -> Maybe b

instance Iota a => IotaCast a a where
  iotaCast = id

instance Iota a => IotaTryCast a a where
  iotaTryCast = Just
instance {-# OVERLAPPABLE #-} (Iota a, Iota b) => IotaTryCast a b where
  iotaTryCast = const Nothing

data IotaAny = forall a. (Typeable a, Iota a) => IotaAny (TypeRep a) a
instance Iota IotaAny where
  iotaShow (IotaAny _ a) = iotaShow a

instance (Typeable a, Iota a) => IotaCast a IotaAny where
  iotaCast a = IotaAny (typeOf a) a
instance (Typeable a, Iota a) => IotaTryCast a IotaAny where
  iotaTryCast a = Just $ IotaAny (typeOf a) a
instance (Typeable a, Iota a) => IotaTryCast IotaAny a where
  iotaTryCast (IotaAny ty a) = case eqTypeRep ty (typeRep @a) of
    Just HRefl -> Just a
    _ -> Nothing

data IotaNull = IotaNull deriving (Eq, Ord, Bounded, Enum)
instance Iota IotaNull where
  iotaShow IotaNull = "Null"

newtype IotaBoolean = IotaBoolean Bool deriving (Eq, Ord, Bounded, Enum)
instance Iota IotaBoolean where
  iotaShow (IotaBoolean b) = T.show b

newtype IotaNumber = IotaNumber Double deriving (Eq, Ord)
instance Iota IotaNumber where
  iotaShow (IotaNumber n) = T.show n

data IotaVector = IotaVector Double Double Double deriving (Eq)
instance Iota IotaVector where
  iotaShow (IotaVector x y z) = "(" <> T.show x <> ", " <> T.show y <> ", " <> T.show z <> ")"

data IotaEntity = IotaEntity Text
instance Iota IotaEntity where
  iotaShow (IotaEntity tag) = "<entity " <> tag <> ">"

data Direction
  = DirectionNE
  | DirectionE
  | DirectionSE
  | DirectionSW
  | DirectionW
  | DirectionNW
  deriving (Eq, Bounded, Enum, TH.Lift)

directionShow :: Direction -> Text
directionShow = \case
  DirectionNE -> "NORTH_EAST"
  DirectionE -> "EAST"
  DirectionSE -> "SOUTH_EAST"
  DirectionSW -> "SOUTH_WEST"
  DirectionW -> "WEST"
  DirectionNW -> "NORTH_WEST"

directionParse :: (IsString s, Eq s) => s -> Maybe Direction
directionParse = \case
  "NORTH_EAST" -> Just DirectionNE
  "EAST" -> Just DirectionE
  "SOUTH_EAST" -> Just DirectionSE
  "SOUTH_WEST" -> Just DirectionSW
  "WEST" -> Just DirectionW
  "NORTH_WEST" -> Just DirectionNW
  _ -> Nothing

data Angle
  = AngleW
  | AngleE
  | AngleD
  | AngleS
  | AngleA
  | AngleQ
  deriving (Eq, Bounded, Enum, TH.Lift)

angleShow :: Angle -> Char
angleShow = \case
  AngleW -> 'w'
  AngleE -> 'e'
  AngleD -> 'd'
  AngleS -> 's'
  AngleA -> 'a'
  AngleQ -> 'q'

angleParse :: Char -> Maybe Angle
angleParse = \case
  'w' -> Just AngleW
  'e' -> Just AngleE
  'd' -> Just AngleD
  's' -> Just AngleS
  'a' -> Just AngleA
  'q' -> Just AngleQ
  _ -> Nothing

data IotaPattern = IotaPattern Direction [Angle] deriving (Eq, TH.Lift)
instance Iota IotaPattern where
  iotaShow (IotaPattern dir angles) =
    "HexPattern["
      <> directionShow dir
      <> ", "
      <> (T.pack $ map angleShow angles)
      <> "]"

data IotaGreatPattern = IotaGreatPattern Text IotaPattern deriving (Eq, TH.Lift)
instance Iota IotaGreatPattern where
  iotaShow (IotaGreatPattern tag pat) =
    "Great["
      <> tag
      <> ", "
      <> iotaShow pat
      <> "]"

data IotaHList as = IotaHList (HList as)

{-# COMPLETE IotaHNil #-}
{-# COMPLETE IotaHCons #-}
pattern IotaHNil :: IotaHList '[]
pattern IotaHNil = IotaHList HNil
pattern IotaHCons :: a -> IotaHList as -> IotaHList (a ': as)
pattern IotaHCons a as <- IotaHList (a `HCons` (IotaHList -> as))
  where
    IotaHCons a (IotaHList as) = IotaHList (a `HCons` as)
infixr 5 `IotaHCons`

class IotaHListImpl as where
  iotaShowHList :: IotaHList as -> Text
instance IotaHListImpl '[] where
  iotaShowHList IotaHNil = "]"
instance {-# OVERLAPPING #-} Iota a => IotaHListImpl '[a] where
  iotaShowHList (x `IotaHCons` IotaHNil) = iotaShow x <> "]"
instance (Iota a, IotaHListImpl as) => IotaHListImpl (a ': as) where
  iotaShowHList (x `IotaHCons` xs) = iotaShow x <> ", " <> iotaShowHList xs

instance IotaHListImpl as => Iota (IotaHList as) where
  iotaShow xs = "[" <> iotaShowHList xs

newtype IotaList a = IotaList (Seq a)
instance Iota a => Iota (IotaList a) where
  iotaShow (IotaList xs) = "[" <> go xs
   where
    go Empty = "]"
    go (y :<| Empty) = iotaShow y <> "]"
    go (y :<| ys) = iotaShow y <> ", " <> go ys
type IotaAnyList = IotaList IotaAny

instance Iota b => IotaCast (IotaHList '[]) (IotaList b) where
  iotaCast (IotaHList HNil) = IotaList Empty
instance
  ( IotaCast a b
  , IotaCast (IotaHList as) (IotaList b)
  , IotaHListImpl as
  ) =>
  IotaCast (IotaHList (a ': as)) (IotaList b)
  where
  iotaCast (IotaHList (x `HCons` xs)) = IotaList (x' :<| xs')
   where
    x' = iotaCast x
    IotaList xs' = iotaCast (IotaHList xs)

instance
  ( IotaCast (IotaHList as) (IotaList b)
  , Iota b
  , IotaHListImpl as
  ) =>
  IotaTryCast (IotaHList as) (IotaList b)
  where
  iotaTryCast = Just . iotaCast

instance Iota a => IotaTryCast (IotaList a) (IotaHList '[]) where
  iotaTryCast (IotaList Empty) = Just $ IotaHList HNil
  iotaTryCast _ = Nothing
instance
  (IotaTryCast a b, IotaTryCast (IotaList a) (IotaHList bs), IotaHListImpl bs) =>
  IotaTryCast (IotaList a) (IotaHList (b ': bs))
  where
  iotaTryCast (IotaList (x :<| xs)) = do
    x' <- iotaTryCast x
    IotaHList xs' <- iotaTryCast (IotaList xs)
    Just $ IotaHList (x' `HCons` xs')
  iotaTryCast _ = Nothing
