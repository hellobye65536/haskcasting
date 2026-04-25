{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskcasting.Util (
  natValInt,
  HListLen,
  hListLen,
  AnySeq,
  AnySeqLit (anySeqLit),
  IotaPlaceholder (IotaPlaceholder),
  findFragPlaceholders,
  findFragPlaceholder,
) where

import Data.HList (HLength, HNat2Nat, Typeable)
import Data.Kind (Type)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import GHC.Exts (proxy#)
import GHC.TypeNats (KnownNat, natVal')
import Haskcasting.Fragment (Fragment, unwrapFragment)
import Haskcasting.Iota (Iota (..), IotaAny, IotaCast (iotaCast), IotaTryCast (iotaTryCast))
import Haskcasting.Serialize.A (Inst (IString))

natValInt :: forall n. KnownNat n => Int
natValInt = fromIntegral $ natVal' (proxy# @n)

type HListLen xs = KnownNat (HNat2Nat (HLength xs))
hListLen :: forall (xs :: [Type]). HListLen xs => Int
hListLen = natValInt @(HNat2Nat (HLength xs))

type AnySeq = Seq IotaAny
class AnySeqLit ts where
  anySeqLit :: ts -> AnySeq
instance AnySeqLit () where
  anySeqLit () = Seq.empty
instance {-# OVERLAPPABLE #-} (Typeable a, Iota a) => AnySeqLit a where
  anySeqLit a = Seq.singleton $ iotaCast a
instance (Typeable a, Iota a, Typeable b, Iota b) => AnySeqLit (a, b) where
  anySeqLit (a, b) = Seq.fromList [iotaCast a, iotaCast b]
instance
  (Typeable a, Iota a, Typeable b, Iota b, Typeable c, Iota c) =>
  AnySeqLit (a, b, c)
  where
  anySeqLit (a, b, c) = Seq.fromList [iotaCast a, iotaCast b, iotaCast c]
instance
  (Typeable a, Iota a, Typeable b, Iota b, Typeable c, Iota c, Typeable d, Iota d) =>
  AnySeqLit (a, b, c, d)
  where
  anySeqLit (a, b, c, d) = Seq.fromList [iotaCast a, iotaCast b, iotaCast c, iotaCast d]
instance
  (Typeable a, Iota a, Typeable b, Iota b, Typeable c, Iota c, Typeable d, Iota d, Typeable e, Iota e) =>
  AnySeqLit (a, b, c, d, e)
  where
  anySeqLit (a, b, c, d, e) = Seq.fromList [iotaCast a, iotaCast b, iotaCast c, iotaCast d, iotaCast e]
instance
  (Typeable a, Iota a, Typeable b, Iota b, Typeable c, Iota c, Typeable d, Iota d, Typeable e, Iota e, Typeable f, Iota f) =>
  AnySeqLit (a, b, c, d, e, f)
  where
  anySeqLit (a, b, c, d, e, f) = Seq.fromList [iotaCast a, iotaCast b, iotaCast c, iotaCast d, iotaCast e, iotaCast f]
instance
  ( Typeable a
  , Iota a
  , Typeable b
  , Iota b
  , Typeable c
  , Iota c
  , Typeable d
  , Iota d
  , Typeable e
  , Iota e
  , Typeable f
  , Iota f
  , Typeable g
  , Iota g
  ) =>
  AnySeqLit (a, b, c, d, e, f, g)
  where
  anySeqLit (a, b, c, d, e, f, g) = Seq.fromList [iotaCast a, iotaCast b, iotaCast c, iotaCast d, iotaCast e, iotaCast f, iotaCast g]
instance
  ( Typeable a
  , Iota a
  , Typeable b
  , Iota b
  , Typeable c
  , Iota c
  , Typeable d
  , Iota d
  , Typeable e
  , Iota e
  , Typeable f
  , Iota f
  , Typeable g
  , Iota g
  , Typeable h
  , Iota h
  ) =>
  AnySeqLit (a, b, c, d, e, f, g, h)
  where
  anySeqLit (a, b, c, d, e, f, g, h) = Seq.fromList [iotaCast a, iotaCast b, iotaCast c, iotaCast d, iotaCast e, iotaCast f, iotaCast g, iotaCast h]

newtype IotaPlaceholder = IotaPlaceholder Text
  deriving (Eq)
instance Iota IotaPlaceholder where
  iotaShow (IotaPlaceholder tag) = "<placeholder: " <> tag <> ">"
  iotaSerializeA _opt _ = Seq.singleton (IString "")

findFragPlaceholders :: Fragment a b -> Text -> [Int]
findFragPlaceholders frag tag = Seq.findIndicesL ((== Just (IotaPlaceholder tag)) . iotaTryCast) $ unwrapFragment frag

findFragPlaceholder :: Fragment a b -> Text -> Int
findFragPlaceholder frag tag = case findFragPlaceholders frag tag of
  [ind] -> ind
  _ -> error "expected placeholder exactly once"
