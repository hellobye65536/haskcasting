{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Iota.Moreiotas where

import Data.List (transpose)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T

import Haskcasting.ExprLang.TH (mkFragExprInstance)
import Haskcasting.Fragment (Fragment)
import Haskcasting.Iota (Iota (..), IotaList (IotaList), IotaNumber (IotaNumber), IotaVector)
import Haskcasting.Pattern (pattern)
import Haskcasting.Serialize.A qualified as SA

import Haskcasting.Patterns.Hexcasting

newtype IotaString = IotaString Text
instance Iota IotaString where
  iotaShow (IotaString s) = "\"" <> s <> "\""
  iotaSerializeA _opt (IotaString s) = Seq.singleton $ SA.IString s

-- list of *rows*
newtype IotaMatrix = IotaMatrix [[Double]]
instance Iota IotaMatrix where
  iotaShow _ = "<matrix>"
  iotaSerializeA opt (IotaMatrix rows)
    | rows /= transpose cols = error "matrix not rectangular"
    | [[n]] <- rows =
        Seq.fromList
          [ SA.INumber n
          , SA.IPattern patTransformationPurification
          , SA.IExec 1 1
          ]
    | ([_, _, _] : _) <- cols =
        foldMap
          ( \vec -> case vec of
              [x, y, z] -> Seq.singleton $ SA.IVector x y z
              _ -> error "column not of length 3"
          )
          cols
          <> Seq.fromList
            [ SA.IMergeN (length cols)
            , SA.IPattern patTransformationPurification
            , SA.IExec 1 1
            ]
    | otherwise =
        iotaSerializeA
          opt
          (IotaList $ Seq.fromList $ map (\col -> IotaList $ Seq.fromList $ map IotaNumber col) cols)
          <> Seq.fromList
            [ SA.IPattern patTransformationPurification
            , SA.IExec 1 1
            ]
   where
    cols = transpose rows
    patTransformationPurification = [pattern| SOUTH_WEST awwaeawwaadwa |]

data IotaItemStack = IotaItemStack Text Int
instance Iota IotaItemStack where
  iotaShow (IotaItemStack item count) =
    "<item stack: "
      <> T.show count
      <> " "
      <> item
      <> ">"

newtype IotaItemType = IotaItemType Text
instance Iota IotaItemType where
  iotaShow (IotaItemType tag) = "<item type: " <> tag <> ">"

newtype IotaEntityType = IotaEntityType Text
instance Iota IotaEntityType where
  iotaShow (IotaEntityType tag) = "<entity type: " <> tag <> ">"

newtype IotaIotaType = IotaIotaType Text
instance Iota IotaIotaType where
  iotaShow (IotaIotaType tag) = "<iota type: " <> tag <> ">"

-- overloads

$( mkFragExprInstance
     "AdditiveDistillation"
     [ [t|Fragment '[IotaString, IotaString] '[IotaString]|]
     , [t|Fragment '[IotaMatrix, IotaMatrix] '[IotaMatrix]|]
     ]
 )

$( mkFragExprInstance
     "MultiplicativeDistillation"
     [ [t|Fragment '[IotaMatrix, IotaMatrix] '[IotaMatrix]|]
     , [t|Fragment '[IotaVector, IotaMatrix] '[IotaMatrix]|]
     ]
 )

$( mkFragExprInstance
     "DivisionDistillation"
     [[t|Fragment '[IotaMatrix, IotaMatrix] '[IotaMatrix]|]]
 )

$( mkFragExprInstance
     "LengthPurification"
     [ [t|Fragment '[IotaString] '[IotaNumber]|]
     , [t|Fragment '[IotaItemStack] '[IotaNumber]|]
     ]
 )

$( mkFragExprInstance
     "PowerDistillation"
     [[t|Fragment '[IotaNumber, IotaMatrix] '[IotaMatrix]|]]
 )

$( mkFragExprInstance
     "LocatorsDistillation"
     [[t|Fragment '[IotaString, IotaString] '[IotaNumber]|]]
 )

$( mkFragExprInstance
     "RetrogradePurification"
     [[t|Fragment '[IotaMatrix] '[IotaMatrix]|]]
 )

$( mkFragExprInstance
     "SelectionExaltation"
     [[t|Fragment '[IotaNumber, IotaNumber, IotaString] '[IotaString]|]]
 )
