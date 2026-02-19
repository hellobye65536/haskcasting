{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Iota.Hexical (IotaDye (..), IotaPigment (..)) where

import Data.Text (Text)
import Haskcasting.Iota (Iota (..))

newtype IotaDye = IotaDye Text
instance Iota IotaDye where
  iotaShow (IotaDye tag) = "<dye: " <> tag <> ">"

newtype IotaPigment = IotaPigment Text
instance Iota IotaPigment where
  iotaShow (IotaPigment tag) = "<pigment: " <> tag <> ">"
