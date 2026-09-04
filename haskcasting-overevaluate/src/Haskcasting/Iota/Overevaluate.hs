{-# LANGUAGE OverloadedStrings #-}

module Haskcasting.Iota.Overevaluate where

import Haskcasting.Iota (Iota (..), IotaList, IotaNumber)

-- | A jumble iota representing a packed (list, number) pair.
-- Created by JumblingGambit, dissolved by JumblingDecomposition.
data IotaJumble = IotaJumble
  { jumbleList :: IotaList IotaNumber
  , jumbleSize :: IotaNumber
  }

instance Iota IotaJumble where
  iotaShow _ = "<jumble>"
