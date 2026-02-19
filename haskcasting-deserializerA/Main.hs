{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Sequence qualified as Seq
import Data.Text.IO qualified as T

import Haskcasting.Embed (embedConsideration, embedIntroRetro)
import Haskcasting.Fragment
import Haskcasting.Iota
import Haskcasting.Iota.Moreiotas (IotaString)
import Haskcasting.Pattern (pattern)
import Haskcasting.Serialize (serializeA)
import Haskcasting.Serialize.A (SerializeOptions (..), defaultSerializeOptions, serializePattern)

import Data.Foldable (Foldable (toList))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Haskcasting.Compound.Hexcasting (mergeTopN)
import Haskcasting.Patterns.Hexcasting
import Haskcasting.Patterns.Hexical
import Haskcasting.Patterns.Moreiotas

popInst :: Fragment as (IotaString ': as)
popInst =
  fragMuninnsReflection
    +.+ fragUnsafeCast @'[IotaList IotaString]
    +.+ fragSpeakersDecomposition
    +.+ fragJestersGambit
    +.+ fragHuginnsGambit

embedParseNum :: Fragment as (IotaExec (IotaString : s) '[IotaNumber] : as)
embedParseNum =
  ( embedIntroRetro $
      IotaList $
        Seq.fromList
          [ -- hexical sekhmet's gambit 1
            IotaPattern [pattern| SOUTH_WEST qaqddq |]
          , iotaInputPurification
          ]
  )
    +.+ fragUnsafeCast @'[IotaExec (IotaString ': _) '[IotaNumber]]

deserializeSuspend
  , deserializePattern
  , deserializeMergeN
  , deserializeNull
  , deserializeTrue
  , deserializeFalse
  , deserializeNumber
  , deserializeVector
  , deserializeString ::
    IotaAny
deserializeSuspend =
  iotaCast $
    (fragmentAsList $ embedIntroRetro $ fragmentAsList $ popInst +.+ fragJanusGambit)
      <> (IotaList $ Seq.singleton $ iotaCast iotaIrisGambit)
deserializePattern =
  iotaCast $
    fragmentAsList $
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
deserializeMergeN =
  iotaCast $
    (fragmentAsList $ popInst +.+ fragInputPurification)
      <> IotaList (Seq.singleton $ iotaCast iotaFlocksGambit)
deserializeNull = iotaCast iotaNullaryReflection
deserializeTrue = iotaCast iotaTrueReflection
deserializeFalse = iotaCast iotaFalseReflection
deserializeNumber = iotaCast $ fragmentAsList $ popInst +.+ fragInputPurification
deserializeVector =
  iotaCast $
    fragmentAsList $
      embedParseNum
        +.+ popInst
        +.+ fragCommaReflection
        +.+ fragSeparationDistillation
        +.+ fragUnsafeCast @'[IotaHList '[IotaString, IotaString, IotaString]]
        +.+ fragThothsGambit @'[_, IotaExec '[IotaString] _]
        +.+ fragFlocksDisintegration
        +.+ fragVectorExaltation
deserializeString = iotaCast $ fragmentAsList $ popInst

deserializeIntrinsic :: IotaPattern -> IotaAny
deserializeIntrinsic iota = iotaCast $ fragmentAsList $ embedConsideration iota

serializeBootstrap0 :: IotaList IotaPattern -> Text
serializeBootstrap0 (IotaList pats) = T.intercalate "," $ map inner $ toList pats
 where
  inner (IotaPattern p) = foldMap T.show $ serializePattern p

bootstrap0 :: Fragment '[IotaString] '[IotaList IotaPattern]
bootstrap0 =
  fragAssertStack @'[IotaString]
    +.+ fragCommaReflection
    +.+ fragSeparationDistillation
    +.+ embedIntroRetro (fragmentAsList parsePattern)
    +.+ fragUnsafeCast @'[IotaExec '[IotaString] '[IotaPattern]]
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
      +.+ embedIntroRetro (fragmentAsList (fragInputPurification :: Fragment '[IotaString] '[IotaNumber]))
      +.+ fragUnsafeCast @'[IotaExec '[IotaString] '[IotaNumber]]
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
    +.+ embedIntroRetro (fragmentAsList $ inner)
    +.+ Fragment (Seq.singleton $ iotaCast iotaSisyphusGambit)
    +.+ fragVeryUnsafeCast
    +.+ fragEmpty
 where
  embedInstDefs =
    embedConsideration (iotaBookkeepersGambit @'[True])
      +.+ embedIntroRetro (fromJust @IotaAnyList $ iotaTryCast deserializePattern)
      +.+ embedIntroRetro (fromJust @IotaAnyList $ iotaTryCast deserializeMergeN)
      +.+ mergeTopN @3
      +.+ fragUnsafeCast @'[IotaAnyList]
  inner =
    fragAssertStack @'[]
      +.+ embedInstDefs
      +.+ popInst
      +.+ fragGeminiDecomposition
      +.+ fragAugursPurification
      +.+ fragVacantReflection
      +.+ embedIntroRetro (fragmentAsList $ fragBookkeepersGambit @'[False, False] +.+ fragJanusGambit)
      +.+ fragUnsafeCast @'[IotaExec '[] '[], IotaExec '[] '[]]
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
    +.+ embedConsideration (fragmentAsIota $ inner)
    +.+ embedConsideration (fragmentAsIota $ quineHelper)
    +.+ fragUnsafeCast @'[IotaExec '[] '[], IotaExec '[] '[]]
    +.+ fragGeminiDecomposition
    +.+ fragHermesGambit
    +.+ fragVeryUnsafeCast
    +.+ fragEmpty
 where
  instDefs =
    IotaList $
      Seq.fromList
        [ deserializeSuspend
        , deserializePattern
        , deserializeMergeN
        , deserializeNull
        , deserializeTrue
        , deserializeFalse
        , deserializeNumber
        , deserializeVector
        , deserializeString
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
      +.+ embedIntroRetro (fragmentAsList $ fragBookkeepersGambit @'[False, False] +.+ fragJanusGambit)
      +.+ fragUnsafeCast @'[IotaExec '[] '[], IotaExec '[] '[]]
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
  T.putStrLn $ iotaShow $ fragmentAsList bootstrap0

  T.putStrLn "\n==== bootstrap1 ===="
  T.putStrLn $
    serializeBootstrap0 $
      fromJust $
        iotaTryCast $
          fragmentAsList $
            bootstrap1

  T.putStrLn "\n==== deserializer ===="
  mapM_ T.putStrLn $
    serializeA defaultSerializeOptions {serOptPatternIntrinsics = False} $
      fragmentAsIota $
        deserializer
