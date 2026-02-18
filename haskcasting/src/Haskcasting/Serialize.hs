module Haskcasting.Serialize (serializeA) where

import Data.Text (Text)
import Haskcasting.Iota (Iota (iotaSerializeA))
import Haskcasting.Serialize.A qualified as SA

serializeA :: (Iota a, SA.MonadSerialize m) => a -> m [Text]
serializeA iota = SA.serialize =<< iotaSerializeA iota
