{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskcasting.Embed (
  escapedPatterns,
  embedIntroRetro,
  embedConsideration,
) where

import Data.Foldable (Foldable (fold))
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Haskcasting.Fragment (Fragment (Fragment))
import Haskcasting.Iota (
  IotaAny,
  IotaAnyList,
  IotaCast (iotaCast),
  IotaExec,
  IotaHList,
  IotaList (IotaList),
  IotaPattern (IotaPattern),
  IotaTryCast (iotaTryCast),
 )
import Haskcasting.Pattern (Angle)
import Haskcasting.Patterns.Hexcasting (
  iotaConsideration,
  iotaEvanition,
  iotaIntrospection,
  iotaRetrospection,
 )

anglesIntrospection, anglesRetrospection :: [Angle]
(IotaPattern _ anglesIntrospection) = iotaIntrospection
(IotaPattern _ anglesRetrospection) = iotaRetrospection

escapedPatterns :: [[Angle]]
escapedPatterns =
  map
    (\(IotaPattern _ angles) -> angles)
    [ iotaIntrospection
    , iotaRetrospection
    , iotaConsideration
    , iotaEvanition
    ]

class IotaCast a IotaAnyList => EmbedIntroRetro a where
  embedIntroRetro :: a -> Fragment as (a ': as)
  embedIntroRetro a_ =
    Fragment $
      fold
        [ Seq.singleton $ iotaCast iotaIntrospection
        , fst $ go Seq.empty a False
        , Seq.singleton $ iotaCast iotaRetrospection
        ]
   where
    IotaList a = iotaCast a_ :: IotaAnyList
    go acc (x Seq.:<| xs) retro = fromMaybe (go (acc Seq.|> x) xs retro) $ do
      IotaPattern _ angles <- iotaTryCast x
      if
        | angles == anglesIntrospection ->
            let (x', xs') = go (Seq.singleton x) xs True
             in pure $ go (acc <> x') xs' retro
        | angles == anglesRetrospection && retro ->
            pure $ (acc Seq.|> x, xs)
        | angles `elem` escapedPatterns ->
            pure $ go (acc Seq.|> iotaCast iotaConsideration Seq.|> x) xs retro
        | otherwise -> Nothing
    go acc Seq.Empty False = (acc, Seq.empty)
    go acc Seq.Empty True = (iotaCast iotaConsideration Seq.<| acc, Seq.empty)

instance IotaCast (IotaList a) IotaAnyList => EmbedIntroRetro (IotaList a)
instance IotaCast (IotaHList as) IotaAnyList => EmbedIntroRetro (IotaHList as)
instance IotaCast (IotaExec as bs) IotaAnyList => EmbedIntroRetro (IotaExec as bs)

embedConsideration :: IotaCast a IotaAny => a -> Fragment as (a ': as)
embedConsideration iota = Fragment $ Seq.fromList [iotaCast iotaConsideration, iotaCast iota]
