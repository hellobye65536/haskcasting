{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Iota.Hexcellular where

import Data.Text (Text)
import Haskcasting.Iota (Iota (..))

data IotaProperty = IotaProperty
  { propertyName :: Text
  , propertyReadonly :: Bool
  }

instance Iota IotaProperty where
  iotaShow (IotaProperty { propertyName = p, propertyReadonly = ro }) =
    "<property: " <> roText <> p <> ">"
    where roText = if ro then "readonly: " else ""

