{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskcasting.Compound.Hexcasting (
  natValInt,
  dupN,
  mergeTopN,
  hlistNth,
  fish,
  fishDup,
) where

import Data.Sequence qualified as Seq
import GHC.Exts (proxy#)
import GHC.TypeNats (KnownNat, natVal', type (-))
import Haskcasting.Fragment (Fragment (Fragment))
import Haskcasting.Iota (IotaCast (iotaCast), IotaHList)

import Haskcasting.Patterns.Hexcasting

natValInt :: forall n. KnownNat n => Int
natValInt = fromIntegral $ natVal' (proxy# @n)

type family DupN n a as where
  DupN 0 a as = as
  DupN n a as = a ': DupN (n - 1) a as

dupN :: forall n a as. KnownNat n => Fragment (a ': as) (DupN n a as)
dupN =
  Fragment $
    Seq.fromList
      [ iotaCast $ iotaNumericalReflection $ natValInt @n
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
      [ iotaCast $ iotaNumericalReflection $ natValInt @n
      , iotaCast $ iotaFlocksGambit
      ]

type family HListNth n as where
  HListNth 0 (a ': as) = a
  HListNth n (a ': as) = HListNth (n - 1) as

type family HListRemoveNth n as where
  HListRemoveNth 0 (a ': as) = as
  HListRemoveNth n (a ': as) = a ': HListRemoveNth (n - 1) as

hlistNth :: forall n as s. KnownNat n => Fragment (IotaHList as ': s) (HListNth n as ': s)
hlistNth =
  Fragment $
    Seq.fromList
      [ iotaCast $ iotaNumericalReflection $ natValInt @n
      , iotaCast $ iotaSelectionDistillation
      ]

fish :: forall n s. KnownNat n => Fragment s (HListNth n s ': HListRemoveNth n s)
fish =
  Fragment $
    Seq.fromList
      [ iotaCast $ iotaNumericalReflection $ natValInt @n
      , iotaCast $ iotaFishermansGambit
      ]

fishDup :: forall n s. KnownNat n => Fragment s (HListNth n s ': s)
fishDup =
  Fragment $
    Seq.fromList
      [ iotaCast $ iotaNumericalReflection $ natValInt @n
      , iotaCast $ iotaFishermansGambitII
      ]
