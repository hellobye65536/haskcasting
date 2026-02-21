{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskcasting.Compound.Hexcasting (dupN, mergeTopN, hlistNth) where

import Data.Sequence qualified as Seq
import GHC.TypeNats (KnownNat, type (-))
import Haskcasting.Fragment (Fragment (Fragment))
import Haskcasting.Iota (IotaCast (iotaCast), IotaHList)

import Haskcasting.Patterns.Hexcasting

type family DupN n a as where
  DupN 0 a as = as
  DupN n a as = a ': DupN (n - 1) a as

dupN :: forall n a as. KnownNat n => Fragment (a ': as) (DupN n a as)
dupN =
  Fragment $
    Seq.fromList
      [ iotaCast $ iotaNumericalReflection @n
      , iotaCast $ iotaGeminiGambit
      ]

type family MergeTopNHList n as where
  MergeTopNHList 0 as = '[]
  MergeTopNHList n (a ': as) = a ': MergeTopNHList (n - 1) as

type family MergeTopNStack n as where
  MergeTopNStack 0 as = as
  MergeTopNStack n (a ': as) = MergeTopNStack (n - 1) as

mergeTopN :: forall n as. KnownNat n => Fragment as (IotaHList (MergeTopNHList n as) ': (MergeTopNStack n as))
mergeTopN =
  Fragment $
    Seq.fromList
      [ iotaCast $ iotaNumericalReflection @n
      , iotaCast $ iotaFlocksGambit
      ]

type family HListNth n as where
  HListNth 0 (a ': as) = a
  HListNth n (a ': as) = HListNth (n - 1) as

hlistNth :: forall n as s. KnownNat n => Fragment (IotaHList as ': s) (HListNth n as ': s)
hlistNth =
  Fragment $
    Seq.fromList
      [ iotaCast $ iotaNumericalReflection @n
      , iotaCast $ iotaSelectionDistillation
      ]
