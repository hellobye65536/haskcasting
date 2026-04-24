{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Data.Foldable (Foldable (toList))
import Data.List (findIndices)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Typeable (Typeable)
import Data.Vector.Unboxed qualified as VU
import Haskcasting.Embed (
  EmbedIntroRetro,
  embedConsideration,
  embedIntroRetro,
  iotaConsideration,
  iotaIntrospection,
  iotaRetrospection,
 )
import Haskcasting.Fragment
import Haskcasting.Iota
import Haskcasting.Iota.Moreiotas (IotaString (IotaString))
import Haskcasting.Serialize (serializeA)
import Haskcasting.Serialize.A (
  Inst (IBootstrapHalt, IBootstrapSemi),
  defaultSerializeOptions,
  patternToStrokes,
  strokeChars,
 )
import Haskcasting.Serialize.A qualified as SA
import Haskcasting.Util (AnySeqLit (anySeqLit))

import Haskcasting.Compound.Hexcasting (mergeTopN)
import Haskcasting.Patterns.Hexcasting
import Haskcasting.Patterns.Hexical
import Haskcasting.Patterns.Moreiotas

data IotaSemiPlaceholder = IotaSemiPlaceholder
instance Iota IotaSemiPlaceholder where
  iotaShow _ = "<semi placeholder>"
  iotaSerializeA _opt _ = Seq.singleton IBootstrapSemi

data IotaHaltPlaceholder = IotaHaltPlaceholder
instance Iota IotaHaltPlaceholder where
  iotaShow _ = "<halt placeholder>"
  iotaSerializeA _opt _ = Seq.singleton IBootstrapHalt

keep1 :: Fragment (a ': as) '[a]
keep1 =
  Fragment $
    anySeqLit
      ( iotaFlocksReflection
      , iotaFlocksGambit
      , iotaDerivationDecomposition
      , iotaBookkeepersGambit [True, False]
      )

popInst :: Fragment as (IotaString ': as)
popInst =
  fragMuninnsReflection
    +.+ fragUnsafeCast @'[IotaHList '[IotaList IotaString, IotaList IotaString]]
    +.+ fragSpeakersDecomposition
    +.+ fragSpeakersDecomposition
    +.+ fragRotationGambitII
    +.+ fragSpeakersDistillation
    +.+ fragHuginnsGambit

embedAny :: (Typeable a, IotaCast a IotaAny) => Bool -> a -> Fragment s (a ': s)
embedAny bootstrap a =
  if bootstrap
    then
      Fragment $
        anySeqLit
          ( iotaIntrospection
          , a
          , iotaRetrospection
          , iotaFlocksDisintegration
          )
    else embedConsideration a

deserializePatternBootstrap :: IotaAnyList
deserializePatternBootstrap =
  fragAsList @'[] @'[IotaPattern] $
    embedIntroRetro
      ( fragAsIota @'[IotaString] @'[IotaNumber] $
          fragInputPurification
            +.+ keep1
      )
      +.+ popInst
      +.+ fragBlankReflection
      +.+ fragSeparationDistillation
      +.+ fragSpeakersDecomposition
      +.+ fragBookkeepersGambit @'[False]
      +.+ fragDerivationDecomposition
      +.+ fragBookkeepersGambit @'[False]
      +.+ fragThothsGambit
      +.+ fragCalligraphersPurification

deserializePattern :: IotaAnyList
deserializePattern =
  fragAsList $
    fragEmpty
      +.+ embedConsideration (IotaString $ T.pack $ VU.toList strokeChars)
      +.+ embedConsideration (fragAsIota inner)
      +.+ popInst
      +.+ fragBlankReflection
      +.+ fragSeparationDistillation
      +.+ fragSpeakersDecomposition
      +.+ fragBookkeepersGambit @'[False]
      +.+ fragDerivationDecomposition
      +.+ fragBookkeepersGambit @'[False]
      +.+ fragThothsGambit
      +.+ fragCalligraphersPurification
      +.+ fragBookkeepersGambit @'[True, False]
 where
  inner :: Fragment '[IotaString, IotaString] '[IotaNumber]
  inner =
    fragAssertStack
      +.+ fragLocatorsDistillation
      +.+ keep1
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

deserializeMergeN :: IotaAnyList
deserializeMergeN =
  fragAsList $
    popInst
      +.+ fragInputPurification
      +.+ fragSingleton iotaFlocksGambit

deserializeString :: IotaAnyList
deserializeString = fragAsList $ popInst

deserializeSuspend :: IotaAnyList
deserializeSuspend =
  fragAsList $
    embedIntroRetro
      ( fragAsIota $
          popInst
            +.+ fragSingleton IotaHaltPlaceholder
      )
      +.+ fragIrisGambit

deserializeNumber :: IotaAnyList
deserializeNumber =
  fragAsList $
    popInst
      +.+ fragInputPurification

deserializeVector :: IotaAnyList
deserializeVector =
  fragAsList $
    embedIntroRetro (fragAsIota $ keep1 +.+ fragInputPurification)
      +.+ popInst
      +.+ fragCommaReflection
      +.+ fragSeparationDistillation
      +.+ fragUnsafeCast @'[IotaHList '[IotaString, IotaString, IotaString]]
      +.+ fragThothsGambit @'[_, IotaExec '[IotaString] _]
      +.+ fragFlocksDisintegration
      +.+ fragVectorExaltation

deserializeExec :: IotaPattern
deserializeExec = iotaHermesGambit

deserializeReuse :: IotaAnyList
deserializeReuse =
  fragAsList $
    popInst
      +.+ fragInputPurification
      +.+ fragSingleton iotaFishermansGambitII

deserializeIntrinsic :: (Typeable a, IotaCast a IotaAny) => Bool -> a -> IotaAnyList
deserializeIntrinsic bootstrap iota = fragAsList $ embedAny bootstrap iota

serializeBootstrap0 :: Foldable t => t IotaPattern -> Text
serializeBootstrap0 pats = T.intercalate "," $ map inner $ toList pats
 where
  inner (IotaPattern p) = foldMap T.show $ patternToStrokes p

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
      +.+ embedIntroRetro
        ( fragAsIota @'[IotaString] @'[IotaNumber] $
            fragInputPurification
        )
      +.+ fragJestersGambit
      +.+ fragThothsGambit
      +.+ fragCalligraphersPurification

deserializeLoop :: Bool -> (forall s. Fragment s (IotaAnyList ': s)) -> Fragment '[IotaList IotaString] '[]
deserializeLoop bootstrap embedInsts =
  fragAssertStack
    +.+ fragSinglesPurification
    +.+ fragVacantReflection
    +.+ fragSpeakersDistillation
    +.+ fragHuginnsGambit
    +.+ embed (fragAsIota inner)
    +.+ fragNumericalReflection injectIndex
    +.+ fragProspectorsGambit
    +.+ (fragSingleton iotaSurgeonsExaltation :: forall a b s. Fragment (a ': IotaNumber ': b ': s) (a ': s))
    +.+ fragHermesGambit
    +.+ fragSingleton (iotaBookkeepersGambit [False, False, False])
 where
  injectIndex = 1 :: Int
  placeholder :: Fragment s' (IotaAnyList ': s')
  placeholder =
    Fragment $
      if bootstrap
        then anySeqLit (iotaIntrospection, iotaBookkeepersGambit [True], iotaRetrospection, iotaFlocksDisintegration)
        else anySeqLit (iotaConsideration, iotaBookkeepersGambit [True])
  embed :: (EmbedIntroRetro a, IotaCast a IotaAny) => a -> Fragment s (a ': s)
  embed = if bootstrap then embedIntroRetro else embedConsideration
  ifElseCharon :: Bool -> Fragment a b -> Fragment (IotaBoolean ': a) b
  ifElseCharon exitOn pat =
    let pat' = case pat of
          Fragment [iotaTryCast -> Just x] -> x
          _ -> error "expected single pattern"
     in if bootstrap
          then
            embedIntroRetro
              ( if exitOn
                  then iotaCharonsGambit `IotaHCons` pat' `IotaHCons` IotaHNil
                  else pat' `IotaHCons` iotaCharonsGambit `IotaHCons` IotaHNil
              )
              +.+ fragFlocksDisintegration
              +.+ fragAugursExaltation
              +.+ fragSingleton iotaHermesGambit
          else
            ( if exitOn
                then embedConsideration iotaCharonsGambit +.+ fragVacantReflection +.+ fragUnsafeCast @'[IotaPattern]
                else fragVacantReflection +.+ fragUnsafeCast @'[IotaPattern] +.+ embedConsideration iotaCharonsGambit
            )
              +.+ fragAugursExaltation
              +.+ fragUnsafeCast @'[IotaExecId _]
              +.+ fragHermesGambit
              +.+ pat
  inner :: Fragment s' (IotaList IotaString ': IotaList IotaAnyList ': IotaAnyList ': s')
  inner =
    placeholder
      +.+ embedInsts
      +.+ fragUnsafeCast @'[IotaList IotaAnyList]
      +.+ fragMuninnsReflection
      +.+ fragUnsafeCast @'[IotaHList '[IotaList IotaString, IotaList IotaString]]
      +.+ embed
        ( fragAsIota $
            fragAssertStack
              +.+ fragSpeakersDecomposition
              +.+ fragAugursPurification
              +.+ ifElseCharon True fragFlocksDisintegration
              --
              +.+ fragGeminiDecomposition
              +.+ fragAugursPurification
              +.+ ifElseCharon False fragSpeakersDecomposition
              +.+ embedAny bootstrap IotaSemiPlaceholder
              +.+ fragUnsafeCast @'[IotaString]
              +.+ fragSeparationDistillation
              +.+ fragSinglesPurification
              +.+ fragJestersGambit
              +.+ fragIntegrationDistillation
              +.+ fragHuginnsGambit
              +.+ fragNullaryReflection
        )
      +.+ fragHermesGambit
      +.+ fragBookkeepersGambit @'[False]
      +.+ embed
        ( fragAsIota $
            fragAssertStack
              +.+ popInst
              +.+ fragGeminiDecomposition
              +.+ fragAugursPurification
              +.+ ifElseCharon False fragInputPurification
              +.+ fragSelectionDistillation
              +.+ fragJestersGambit
              +.+ fragNumericalReflection injectIndex
              +.+ fragProspectorsGambit
              +.+ (fragSingleton iotaSurgeonsExaltation :: forall a b s. Fragment (a ': IotaNumber ': b ': s) (a ': s))
              +.+ fragAdditiveDistillation
              +.+ fragSingleton iotaHermesGambit
        )
      +.+ fragHermesGambit

bootstrap1 :: Fragment '[IotaList IotaString] '[]
bootstrap1 =
  deserializeLoop True $
    fragAssertStack
      +.+ embedIntroRetro (deserializePatternBootstrap)
      +.+ embedIntroRetro (deserializeMergeN)
      +.+ embedIntroRetro (deserializeString)
      +.+ embedIntroRetro (deserializeIntrinsic True IotaSemiPlaceholder)
      +.+ embedIntroRetro (deserializeIntrinsic True IotaHaltPlaceholder)
      +.+ mergeTopN @5
      +.+ fragCast @'[IotaAnyList]

deserializer :: Fragment '[IotaList IotaString] '[]
deserializer =
  deserializeLoop False $
    embedConsideration $
      IotaList $
        anySeqLit
          ( deserializePattern
          , deserializeMergeN
          , deserializeString
          , deserializeSuspend
          , deserializeNumber
          )
          <> anySeqLit
            ( deserializeVector
            , deserializeExec
            , deserializeReuse
            , deserializeIntrinsic False iotaConsideration
            , deserializeIntrinsic False iotaIntrospection
            , deserializeIntrinsic False iotaRetrospection
            )

main :: IO ()
main = do
  T.putStrLn "\n==== bootstrap0 ===="
  T.putStrLn $ iotaShow $ fragAsList bootstrap0

  let IotaList (toList -> bootstrap1Iotas) =
        fragAsList $
          bootstrap1
      bootstrap1Cast iota
        | Just IotaHaltPlaceholder <- iotaTryCast iota = iotaBookkeepersGambit [True]
        | Just IotaSemiPlaceholder <- iotaTryCast iota = iotaBookkeepersGambit [True]
        | Just pat <- iotaTryCast iota = pat
        | otherwise = error $ "invalid iota: " <> T.unpack (iotaShow iota)
      isSemiPlaceholder iota
        | Just IotaSemiPlaceholder <- iotaTryCast iota = True
        | otherwise = False
      isHaltPlaceholder iota
        | Just IotaHaltPlaceholder <- iotaTryCast iota = True
        | otherwise = False

  T.putStrLn "\n==== bootstrap1 ===="
  mapM_ T.putStrLn $
    T.chunksOf 250 $
      serializeBootstrap0 $
        map bootstrap1Cast bootstrap1Iotas

  T.putStrLn "\n==== bootstrap1 semicolon placeholders ===="
  print $
    findIndices isSemiPlaceholder bootstrap1Iotas
  T.putStrLn "\n==== bootstrap1 halt jump placeholders ===="
  print $
    findIndices isHaltPlaceholder bootstrap1Iotas

  T.putStrLn "\n==== deserializer ===="
  mapM_ T.putStrLn $
    serializeA defaultSerializeOptions {SA.soBootstrap = True} $
      fragAsIota $
        deserializer
