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
  IotaPattern (..),
  IotaGreatPattern (..),
  IotaExec (..),
  IotaHList (.., IotaHNil, IotaHCons),
  IotaList (..),
  IotaAnyList,
) where

import Data.HList (HList (HCons, HNil), foldM)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Haskcasting.Pattern (Angle, Direction, angleShow, directionShow)
import Haskcasting.Serialize.A qualified as SA
import Language.Haskell.TH.Syntax qualified as TH
import Type.Reflection (TypeRep, Typeable, eqTypeRep, typeOf, typeRep, type (:~~:) (HRefl))

class Iota a where
  iotaShow :: a -> Text
  iotaSerializeA :: SA.MonadSerialize m => a -> m (Seq SA.Inst)
  iotaSerializeA a = pure $ Seq.singleton $ SA.Suspend $ iotaShow a

class (Iota a, Iota b) => IotaCast a b where
  iotaCast :: a -> b
class (Iota a, Iota b) => IotaTryCast a b where
  iotaTryCast :: a -> Maybe b

instance {-# OVERLAPPING #-} Iota a => IotaCast a a where
  iotaCast = id

instance {-# OVERLAPPING #-} Iota a => IotaTryCast a a where
  iotaTryCast = Just
instance {-# OVERLAPPABLE #-} (Iota a, Iota b) => IotaTryCast a b where
  iotaTryCast = const Nothing

data IotaAny where
  IotaAny :: (Typeable a, Iota a) => (TypeRep a) -> a -> IotaAny
instance Iota IotaAny where
  iotaShow (IotaAny _ a) = iotaShow a
  iotaSerializeA (IotaAny _ a) = iotaSerializeA a

instance {-# OVERLAPPABLE #-} (Typeable a, Iota a) => IotaCast a IotaAny where
  iotaCast a = IotaAny (typeOf a) a
instance {-# OVERLAPPABLE #-} (Typeable a, Iota a) => IotaTryCast a IotaAny where
  iotaTryCast a = Just $ iotaCast a
instance {-# OVERLAPPABLE #-} (Typeable a, Iota a) => IotaTryCast IotaAny a where
  iotaTryCast (IotaAny ty a)
    | Just HRefl <- eqTypeRep ty (typeRep @IotaAny) = iotaTryCast a
    | Just HRefl <- eqTypeRep ty (typeRep @a) = Just a
    | otherwise = Nothing

data IotaNull = IotaNull deriving (Eq, Ord, Bounded, Enum)
instance Iota IotaNull where
  iotaShow IotaNull = "Null"
  iotaSerializeA IotaNull = pure $ Seq.singleton SA.Null

newtype IotaBoolean = IotaBoolean Bool deriving (Eq, Ord, Bounded, Enum)
instance Iota IotaBoolean where
  iotaShow (IotaBoolean b) = T.show b
  iotaSerializeA (IotaBoolean b) = pure $ Seq.singleton $ SA.Bool b

newtype IotaNumber = IotaNumber Double deriving (Eq, Ord)
instance Iota IotaNumber where
  iotaShow (IotaNumber n) = T.show n
  iotaSerializeA (IotaNumber n) = pure $ Seq.singleton $ SA.Number n

data IotaVector = IotaVector Double Double Double deriving (Eq)
instance Iota IotaVector where
  iotaShow (IotaVector x y z) = "(" <> T.show x <> ", " <> T.show y <> ", " <> T.show z <> ")"
  iotaSerializeA (IotaVector x y z) = pure $ Seq.singleton $ SA.Vector x y z

data IotaEntity = IotaEntity Text
instance Iota IotaEntity where
  iotaShow (IotaEntity tag) = "<entity " <> tag <> ">"

data IotaPattern = IotaPattern Direction [Angle] deriving (Eq, TH.Lift)
instance Iota IotaPattern where
  iotaShow (IotaPattern dir angles) =
    "HexPattern["
      <> directionShow dir
      <> ", "
      <> (T.pack $ map angleShow angles)
      <> "]"
  iotaSerializeA (IotaPattern dir angles) =
    pure $ Seq.singleton $ SA.Pattern dir angles

data IotaGreatPattern = IotaGreatPattern Text IotaPattern deriving (Eq, TH.Lift)
instance Iota IotaGreatPattern where
  iotaShow (IotaGreatPattern tag pat) =
    "Great["
      <> tag
      <> ", "
      <> iotaShow pat
      <> "]"
  iotaSerializeA iota@(IotaGreatPattern tag _pat) = do
    gp <- SA.findGreatPattern tag
    let def = SA.Suspend $ iotaShow iota
    pure $ Seq.singleton $ fromMaybe def $ fmap (uncurry SA.Pattern) gp

newtype IotaExec as bs where
  IotaExec :: forall (as :: [Type]) (bs :: [Type]). IotaAnyList -> IotaExec as bs
instance Iota (IotaExec as bs) where
  iotaShow (IotaExec inner) = iotaShow inner
  iotaSerializeA (IotaExec inner) = iotaSerializeA inner

instance IotaCast (IotaExec as bs) IotaAny where
  iotaCast (IotaExec inner) = iotaCast inner
instance IotaTryCast (IotaExec as bs) IotaAny where
  iotaTryCast = Just . iotaCast
instance (IotaTryCast IotaAnyList a) => IotaTryCast (IotaExec as bs) a where
  iotaTryCast (IotaExec inner) = iotaTryCast inner

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
  iotaSerializeHListA :: SA.MonadSerialize m => IotaHList as -> m (Seq SA.Inst)
instance IotaHListImpl '[] where
  iotaShowHList IotaHNil = "]"
  iotaSerializeHListA IotaHNil = pure Seq.empty
instance {-# OVERLAPPING #-} Iota a => IotaHListImpl '[a] where
  iotaShowHList (x `IotaHCons` IotaHNil) = iotaShow x <> "]"
  iotaSerializeHListA (x `IotaHCons` IotaHNil) = do
    x' <- iotaSerializeA x
    pure $ x' Seq.|> SA.Push
instance (Iota a, IotaHListImpl as) => IotaHListImpl (a ': as) where
  iotaShowHList (x `IotaHCons` xs) = iotaShow x <> ", " <> iotaShowHList xs
  iotaSerializeHListA (x `IotaHCons` xs) = do
    x' <- iotaSerializeA x
    xs' <- iotaSerializeHListA xs
    pure $ (x' Seq.|> SA.Push) <> xs'

instance IotaHListImpl as => Iota (IotaHList as) where
  iotaShow xs = "[" <> iotaShowHList xs
  iotaSerializeA = fmap (SA.EmptyList Seq.<|) . iotaSerializeHListA

newtype IotaList a = IotaList (Seq a) deriving (Semigroup, Monoid)
instance Iota a => Iota (IotaList a) where
  iotaShow (IotaList xs) = "[" <> go xs
   where
    go Empty = "]"
    go (y :<| Empty) = iotaShow y <> "]"
    go (y :<| ys) = iotaShow y <> ", " <> go ys
  iotaSerializeA (IotaList xs) = foldM go (Seq.singleton SA.EmptyList) xs
   where
    go is x = do
      x' <- iotaSerializeA x
      pure $ is <> (x' Seq.|> SA.Push)
type IotaAnyList = IotaList IotaAny

instance IotaCast a b => IotaCast (IotaList a) (IotaList b) where
  iotaCast (IotaList xs) = IotaList $ fmap iotaCast xs
instance {-# OVERLAPPING #-} IotaCast IotaAnyList IotaAnyList where
  iotaCast = id

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
