{-# LANGUAGE UndecidableInstances #-}

module Haskcasting.Embed (
  escapedPatterns,
  embedIntroRetro,
  embedConsideration,
) where

import Data.Foldable (Foldable (fold))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Haskcasting.Fragment (Fragment (Fragment))
import Haskcasting.Iota (
  Angle,
  IotaAny,
  IotaAnyList,
  IotaCast (iotaCast),
  IotaHList,
  IotaList (IotaList),
  IotaPattern (IotaPattern),
  IotaTryCast (iotaTryCast),
 )
import Haskcasting.Patterns.Hexcasting (iotaConsideration, iotaEvanition, iotaIntrospection, iotaRetrospection)

escapedPatterns :: [[Angle]]
escapedPatterns =
  map
    (\(IotaPattern _ angles) -> angles)
    [ iotaIntrospection
    , iotaRetrospection
    , iotaConsideration
    , iotaEvanition
    ]

class IotaCast ls IotaAnyList => EmbedIntroRetro ls where
  embedIntroRetro :: ls -> Fragment as (ls ': as)
  embedIntroRetro ls_ =
    Fragment $
      fold
        [ Seq.singleton $ iotaCast iotaIntrospection
        , foldMap go ls
        , Seq.singleton $ iotaCast iotaRetrospection
        ]
   where
    IotaList ls = iotaCast ls_ :: IotaAnyList
    go :: IotaAny -> Seq IotaAny
    go iota
      | Just (IotaPattern _ angles) <- iotaTryCast iota
      , angles `elem` escapedPatterns =
          Seq.fromList [iotaCast iotaConsideration, iota]
      | otherwise = Seq.singleton iota
instance IotaCast (IotaList a) IotaAnyList => EmbedIntroRetro (IotaList a)
instance IotaCast (IotaHList as) IotaAnyList => EmbedIntroRetro (IotaHList as)

embedConsideration :: IotaCast a IotaAny => a -> Fragment as (a ': as)
embedConsideration iota = Fragment $ Seq.fromList [iotaCast iotaConsideration, iotaCast iota]
