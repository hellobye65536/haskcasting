module Haskcasting.Serialize (serializeA, serializeADefault) where

import Data.Text (Text)
import Haskcasting.Iota (Iota (iotaSerializeA))
import Haskcasting.Serialize.A qualified as SA

serializeA :: Iota a => SA.SerializeOptions -> a -> [Text]
serializeA = (.) <$> SA.serialize <*> iotaSerializeA

serializeADefault :: Iota a => a -> [Text]
serializeADefault = serializeA SA.defaultSerializeOptions
