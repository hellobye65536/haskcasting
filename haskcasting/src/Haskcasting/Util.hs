{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}

module Haskcasting.Util (natValInt, HListLen, hListLen) where

import Data.HList (HLength, HNat2Nat)
import Data.Kind (Type)
import GHC.Exts (proxy#)
import GHC.TypeNats (KnownNat, natVal')

natValInt :: forall n. KnownNat n => Int
natValInt = fromIntegral $ natVal' (proxy# @n)

type HListLen xs = KnownNat (HNat2Nat (HLength xs))
hListLen :: forall (xs :: [Type]). HListLen xs => Int
hListLen = natValInt @(HNat2Nat (HLength xs))
