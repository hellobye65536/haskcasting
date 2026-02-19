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

import Data.HList (HList (HCons, HNil))
import Data.HashMap.Strict qualified as HM
import Data.Kind (Type)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Haskcasting.Pattern (Pattern, patternShow)
import Haskcasting.Serialize.A qualified as SA
import Language.Haskell.TH.Syntax qualified as TH
import Type.Reflection (TypeRep, Typeable, eqTypeRep, typeOf, typeRep, type (:~~:) (HRefl))

class Iota a where
  iotaShow :: a -> Text
  iotaSerializeA :: SA.SerializeOptions -> a -> Seq SA.Inst
  iotaSerializeA _opt a = Seq.singleton $ SA.Suspend $ iotaShow a

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
  iotaSerializeA opt (IotaAny _ a) = iotaSerializeA opt a

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
  iotaSerializeA _opt IotaNull = Seq.singleton SA.Null

newtype IotaBoolean = IotaBoolean Bool deriving (Eq, Ord, Bounded, Enum)
instance Iota IotaBoolean where
  iotaShow (IotaBoolean b) = T.show b
  iotaSerializeA _opt (IotaBoolean b) = Seq.singleton $ SA.Bool b

newtype IotaNumber = IotaNumber Double deriving (Eq, Ord)
instance Iota IotaNumber where
  iotaShow (IotaNumber n) = T.show n
  iotaSerializeA _opt (IotaNumber n) = Seq.singleton $ SA.Number n

data IotaVector = IotaVector Double Double Double deriving (Eq)
instance Iota IotaVector where
  iotaShow (IotaVector x y z) = "(" <> T.show x <> ", " <> T.show y <> ", " <> T.show z <> ")"
  iotaSerializeA _opt (IotaVector x y z) = Seq.singleton $ SA.Vector x y z

data IotaEntity = IotaEntity Text
instance Iota IotaEntity where
  iotaShow (IotaEntity tag) = "<entity: " <> tag <> ">"

data IotaPattern = IotaPattern Pattern deriving (Eq, TH.Lift)
instance Iota IotaPattern where
  iotaShow (IotaPattern pat) = patternShow pat
  iotaSerializeA _opt (IotaPattern pat) = Seq.singleton $ SA.Pattern pat

data IotaGreatPattern = IotaGreatPattern Text Pattern deriving (Eq, TH.Lift)
instance Iota IotaGreatPattern where
  iotaShow (IotaGreatPattern tag pat) =
    "Great["
      <> tag
      <> ", "
      <> patternShow pat
      <> "]"
  iotaSerializeA
    (SA.SerializeOptions {serOptGreatSpells = gps})
    iota@(IotaGreatPattern tag _pat) = Seq.singleton $ case HM.lookup tag gps of
      Just gp -> SA.Pattern gp
      Nothing -> SA.Suspend $ iotaShow iota

newtype IotaExec as bs where
  IotaExec :: forall (as :: [Type]) (bs :: [Type]). IotaAnyList -> IotaExec as bs
instance Iota (IotaExec as bs) where
  iotaShow (IotaExec inner) = iotaShow inner
  iotaSerializeA opt (IotaExec inner) = iotaSerializeA opt inner

instance IotaCast (IotaExec as bs) IotaAny where
  iotaCast (IotaExec inner) = iotaCast inner
instance IotaTryCast (IotaExec as bs) IotaAny where
  iotaTryCast = Just . iotaCast
instance IotaTryCast IotaAnyList a => IotaTryCast (IotaExec as bs) a where
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
  iotaSerializeAHList :: SA.SerializeOptions -> IotaHList as -> (Seq SA.Inst, Int)
instance IotaHListImpl '[] where
  iotaShowHList IotaHNil = "]"
  iotaSerializeAHList _opt IotaHNil = (Seq.empty, 0)
instance {-# OVERLAPPING #-} Iota a => IotaHListImpl '[a] where
  iotaShowHList (x `IotaHCons` IotaHNil) = iotaShow x <> "]"
  iotaSerializeAHList opt (x `IotaHCons` IotaHNil) = (iotaSerializeA opt x, 1)
instance (Iota a, IotaHListImpl as) => IotaHListImpl (a ': as) where
  iotaShowHList (x `IotaHCons` xs) = iotaShow x <> ", " <> iotaShowHList xs
  iotaSerializeAHList opt (x `IotaHCons` xs) =
    let (xs', len) = iotaSerializeAHList opt xs
     in (iotaSerializeA opt x <> xs', 1 + len)

instance IotaHListImpl as => Iota (IotaHList as) where
  iotaShow xs = "[" <> iotaShowHList xs
  iotaSerializeA opt xs =
    let (xs', len) = iotaSerializeAHList opt xs
     in xs' Seq.|> SA.MergeN len

newtype IotaList a = IotaList (Seq a) deriving (Semigroup, Monoid)
instance Iota a => Iota (IotaList a) where
  iotaShow (IotaList xs) = "[" <> go xs
   where
    go Empty = "]"
    go (y :<| Empty) = iotaShow y <> "]"
    go (y :<| ys) = iotaShow y <> ", " <> go ys
  iotaSerializeA opt (IotaList xs) = foldMap (iotaSerializeA opt) xs Seq.|> SA.MergeN (Seq.length xs)
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
