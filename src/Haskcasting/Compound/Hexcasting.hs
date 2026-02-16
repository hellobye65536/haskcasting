{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskcasting.Compound.Hexcasting (dupN) where

import Data.Sequence qualified as Seq
import GHC.TypeNats (KnownNat, type (-))
import Haskcasting.Fragment (Fragment (Fragment))
import Haskcasting.Iota (IotaCast (iotaCast))
import Haskcasting.Patterns.Hexcasting (iotaGeminiGambit, iotaNumericalReflection)

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
