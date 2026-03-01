{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Foldable (Foldable (toList))
import Data.Maybe (fromJust)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Haskcasting.Embed (embedConsideration, embedIntroRetro)
import Haskcasting.Fragment
import Haskcasting.Iota
import Haskcasting.Iota.Moreiotas (IotaString (IotaString))
import Haskcasting.Serialize (serializeA)
import Haskcasting.Serialize.A (SerializeOptions (..), defaultSerializeOptions, serializePattern, strokeList)

import Haskcasting.Compound.Hexcasting (mergeTopN)
import Haskcasting.Patterns.Hexcasting
import Haskcasting.Patterns.Hexical
import Haskcasting.Patterns.Moreiotas

castExecIdPair :: forall a x y s. Fragment (x ': y ': s) (IotaExec a a ': IotaExec a a ': s)
castExecIdPair = fragVeryUnsafeCast

popInst :: Fragment as (IotaString ': as)
popInst =
  fragMuninnsReflection
    +.+ fragUnsafeCast @'[IotaList IotaString]
    +.+ fragSpeakersDecomposition
    +.+ fragJestersGambit
    +.+ fragHuginnsGambit

embedParseNum :: Fragment as (IotaExec (IotaString : s) '[IotaNumber] : as)
embedParseNum =
  embedIntroRetro $ fragAsIota $ fragSehkmetsGambit @1 +.+ fragInputPurification

deserializePatternBootstrap
  , deserializePattern
  , deserializeMergeN
  , deserializeString
  , deserializeSuspend
  , deserializeNumber
  , deserializeVector
  , deserializeExec
  , deserializeReuse ::
    IotaAny
deserializePatternBootstrap =
  iotaCast $
    fragAsList $
      embedParseNum
        +.+ popInst
        +.+ fragBlankReflection
        +.+ fragSeparationDistillation
        +.+ fragSpeakersDecomposition
        +.+ fragBookkeepersGambit @'[False]
        +.+ fragDerivationDecomposition
        +.+ fragBookkeepersGambit @'[False]
        +.+ fragThothsGambit @'[_, IotaExec '[IotaString] _]
        +.+ fragChirographersPurification
deserializePattern =
  iotaCast $
    fragAsList $
      fragEmpty
        +.+ embedConsideration (IotaString $ T.pack strokeList)
        +.+ embedConsideration (fragAsIota inner)
        +.+ popInst
        +.+ fragBlankReflection
        +.+ fragSeparationDistillation
        +.+ fragSpeakersDecomposition
        +.+ fragBookkeepersGambit @'[False]
        +.+ fragDerivationDecomposition
        +.+ fragBookkeepersGambit @'[False]
        +.+ fragThothsGambit
        +.+ fragChirographersPurification
        +.+ fragBookkeepersGambit @'[True, False]
 where
  inner :: Fragment '[IotaString, IotaString] '[IotaNumber]
  inner =
    fragAssertStack
      +.+ fragSehkmetsGambit @2
      +.+ fragLocatorsDistillation
      +.+ fragNumericalReflection 6
      -- 6, n
      +.+ fragDioscuriGambit
      -- 6, n, 6, n
      +.+ fragMinimusDistillation
      -- (n < 6), 6, n
      +.+ fragRotationGambitII
      -- 6, n, (n < 6)
      +.+ fragProspectorsGambit
      -- n, 6, n, (n < 6)
      +.+ fragSinglesPurification
      +.+ fragRotationGambitII
      -- 6, n, [n], (n < 6)
      +.+ fragUndertakersGambit
      +.+ fragSubtractiveDistillation
      +.+ fragJestersGambit
      -- 6, (n-6), [n], (n < 6)
      +.+ fragDioscuriGambit
      -- 6, (n-6), 6, (n-6), [n], (n < 6)
      +.+ fragModulusDistillation
      -- ((n-6) % 6), 6, (n-6), [n], (n < 6)
      +.+ fragRotationGambitII
      -- 6, (n-6), ((n-6) % 6), [n], (n < 6)
      +.+ fragDivisionDistillation
      +.+ fragFloorPurification
      -- ((n-6) // 6), ((n-6) % 6), [n], (n < 6)
      +.+ mergeTopN @2
      +.+ fragUnsafeCast @'[IotaHList '[IotaNumber]]
      +.+ fragAugursExaltation
      --
      +.+ fragFlocksDisintegration
deserializeMergeN =
  iotaCast $
    fragAsList $
      popInst
        +.+ fragInputPurification
        +.+ fragSingleton iotaFlocksGambit
deserializeString = iotaCast $ fragAsList $ popInst
deserializeSuspend =
  iotaCast $
    fragAsList $
      embedIntroRetro (fragAsIota $ popInst +.+ fragJanusGambit)
        +.+ fragIrisGambit
deserializeNumber =
  iotaCast $
    fragAsList $
      popInst
        +.+ fragInputPurification
deserializeVector =
  iotaCast $
    fragAsList $
      embedParseNum
        +.+ popInst
        +.+ fragCommaReflection
        +.+ fragSeparationDistillation
        +.+ fragUnsafeCast @'[IotaHList '[IotaString, IotaString, IotaString]]
        +.+ fragThothsGambit @'[_, IotaExec '[IotaString] _]
        +.+ fragFlocksDisintegration
        +.+ fragVectorExaltation
deserializeExec = iotaCast iotaHermesGambit
deserializeReuse =
  iotaCast $
    fragAsList $
      popInst
        +.+ fragInputPurification
        +.+ fragSingleton iotaFishermansGambitII

deserializeIntrinsic :: IotaPattern -> IotaAny
deserializeIntrinsic iota = iotaCast $ fragAsList $ embedConsideration iota

serializeBootstrap0 :: IotaList IotaPattern -> Text
serializeBootstrap0 (IotaList pats) = T.intercalate "," $ map inner $ toList pats
 where
  inner (IotaPattern p) = foldMap T.show $ serializePattern p

bootstrap0 :: Fragment '[IotaString] '[IotaList IotaPattern]
bootstrap0 =
  fragAssertStack @'[IotaString]
    +.+ fragCommaReflection
    +.+ fragSeparationDistillation
    +.+ embedIntroRetro (fragAsIota parsePattern)
    +.+ fragJestersGambit
    +.+ fragThothsGambit
 where
  parsePattern :: Fragment '[IotaString] '[IotaPattern]
  parsePattern =
    fragBlankReflection
      +.+ fragSeparationDistillation
      +.+ fragSpeakersDecomposition
      +.+ fragBookkeepersGambit @'[False]
      +.+ fragDerivationDecomposition
      +.+ fragBookkeepersGambit @'[False]
      +.+ embedIntroRetro (fragAsIota (fragInputPurification :: Fragment '[IotaString] '[IotaNumber]))
      +.+ fragJestersGambit
      +.+ fragThothsGambit
      +.+ fragChirographersPurification

bootstrap1 :: Fragment '[IotaString] '[]
bootstrap1 =
  fragAssertStack @'[IotaString]
    +.+ embedConsideration (iotaBookkeepersGambit @'[True]) -- semicolon placeholder
    +.+ fragUnsafeCast @'[IotaString]
    +.+ fragSeparationDistillation
    +.+ fragHuginnsGambit
    +.+ embedIntroRetro (fragAsIota inner)
    +.+ fragSisyphusGambit
    +.+ fragVeryUnsafeCast
    +.+ fragEmpty
 where
  embedInstDefs =
    fragAssertStack
      +.+ embedIntroRetro (fromJust @IotaAnyList $ iotaTryCast deserializePatternBootstrap)
      +.+ embedIntroRetro (fromJust @IotaAnyList $ iotaTryCast deserializeMergeN)
      +.+ embedIntroRetro (fromJust @IotaAnyList $ iotaTryCast deserializeString)
      +.+ mergeTopN @3
      +.+ fragUnsafeCast @'[IotaAnyList]
  inner =
    fragAssertStack @'[]
      +.+ embedInstDefs
      +.+ popInst
      +.+ fragGeminiDecomposition
      +.+ fragAugursPurification
      +.+ fragVacantReflection
      +.+ embedIntroRetro (fragAsList $ fragBookkeepersGambit @'[False, False] +.+ fragJanusGambit)
      +.+ castExecIdPair @'[IotaString, IotaAnyList]
      +.+ fragAugursExaltation
      +.+ fragHermesGambit
      +.+ fragInputPurification
      +.+ fragSelectionDistillation
      +.+ fragUnsafeCast @'[IotaExec '[] '[]]
      +.+ fragHermesGambit

deserializer :: Fragment '[IotaString] '[]
deserializer =
  fragAssertStack @'[IotaString]
    +.+ embedConsideration (iotaBookkeepersGambit @'[True]) -- semicolon placeholder
    +.+ fragUnsafeCast @'[IotaString]
    +.+ fragSeparationDistillation
    +.+ fragHuginnsGambit
    +.+ embedConsideration (fragAsIota $ inner)
    +.+ embedConsideration (fragAsIota $ quineHelper)
    +.+ fragGeminiDecomposition
    +.+ fragSingleton iotaHermesGambit
 where
  instDefs =
    IotaList $
      Seq.fromList
        [ deserializePattern
        , deserializeMergeN
        , deserializeString
        , deserializeSuspend
        , deserializeNumber
        , deserializeVector
        , deserializeExec
        , deserializeReuse
        , deserializeIntrinsic iotaConsideration
        , deserializeIntrinsic iotaIntrospection
        , deserializeIntrinsic iotaRetrospection
        ]
  inner =
    fragAssertStack @'[]
      +.+ embedConsideration instDefs
      +.+ popInst
      +.+ fragGeminiDecomposition
      +.+ fragAugursPurification
      +.+ fragVacantReflection
      +.+ embedIntroRetro (fragAsList $ fragBookkeepersGambit @'[False, False] +.+ fragJanusGambit)
      +.+ castExecIdPair @'[IotaString, IotaAnyList]
      +.+ fragAugursExaltation
      +.+ fragHermesGambit
      +.+ fragInputPurification
      +.+ fragSelectionDistillation
      +.+ fragUnsafeCast @'[IotaExec '[] '[]]
      +.+ fragHermesGambit
  quineHelper =
    fragAssertStack @'[IotaAnyList, IotaAnyList]
      +.+ fragProspectorsGambit
      -- inner, quineHelper, inner
      +.+ embedConsideration iotaConsideration
      +.+ fragCast @'[IotaAny]
      -- Consideration, inner, quineHelper, inner
      +.+ fragUndertakersGambit
      -- Consideration, inner, Consideration, quineHelper, inner
      +.+ mergeTopN @4
      +.+ fragRetrogradePurification
      -- [Consideration, inner, Consideration, quineHelper], inner
      +.+ fragUnsafeCast @'[IotaAnyList]
      +.+ fragAdditiveDistillation
      -- [*inner, Consideration, inner, Consideration, quineHelper]
      +.+ embedIntroRetro
        ( iotaCast $
            IotaList $
              Seq.fromList
                [ iotaGeminiDecomposition
                , iotaHermesGambit
                ] ::
            IotaAnyList
        )
      +.+ fragAdditiveDistillation
      +.+ fragUnsafeCast @'[IotaExec '[] '[]]
      -- [*inner, Consideration, inner, Consideration, quineHelper, HermesGambit]
      +.+ fragHermesGambit

main :: IO ()
main = do
  T.putStrLn "\n==== bootstrap0 ===="
  T.putStrLn $ iotaShow $ fragAsList bootstrap0

  T.putStrLn "\n==== bootstrap1 ===="
  T.putStrLn $
    serializeBootstrap0 $
      fromJust $
        iotaTryCast $
          fragAsList $
            bootstrap1

  T.putStrLn "\n==== deserializer ===="
  mapM_ T.putStrLn $
    serializeA defaultSerializeOptions {serOptBootstrap = True} $
      fragAsIota $
        deserializer
