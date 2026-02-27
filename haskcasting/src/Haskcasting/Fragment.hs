{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskcasting.Fragment (
  Fragment (..),
  (+.+),
  fragAssertStack,
  fragEmpty,
  fragAsList,
  fragAsIota,
  fragSingleton,
  fragCast,
  fragUnsafeCast,
  fragVeryUnsafeCast,
) where

import Data.Kind (Type)
import Data.Sequence (Seq (Empty))
import Data.Sequence qualified as Seq
import Haskcasting.Iota (IotaAny, IotaAnyList, IotaCast (iotaCast), IotaExec (IotaExec), IotaList (IotaList))

data Fragment :: [Type] -> [Type] -> Type where
  Fragment :: Seq IotaAny -> Fragment a b

infixl 9 +.+
(+.+) :: Fragment as bs -> Fragment bs cs -> Fragment as cs
Fragment l +.+ Fragment r = Fragment (l <> r)

fragVeryUnsafeCast :: forall bs as. Fragment as bs
fragVeryUnsafeCast = Fragment Empty

fragAssertStack :: Fragment as as
fragAssertStack = fragVeryUnsafeCast

fragEmpty :: Fragment '[] '[]
fragEmpty = fragAssertStack @'[]

fragAsList :: Fragment a b -> IotaAnyList
fragAsList (Fragment xs) = IotaList xs

fragAsIota :: Fragment a b -> IotaExec a b
fragAsIota = IotaExec . fragAsList

fragSingleton :: IotaCast a IotaAny => a -> Fragment as bs
fragSingleton iota = Fragment $ Seq.singleton $ iotaCast iota

class FragCast as' asbs as'bs | as' asbs -> as'bs where
  fragCast :: Fragment asbs as'bs
  fragCast = fragVeryUnsafeCast
instance FragCast '[] asbs asbs
instance (FragCast as' asbs as'bs, IotaCast a a') => FragCast (a' ': as') (a ': asbs) (a' : as'bs)

class FragUnsafeCast as' asbs as'bs | as' asbs -> as'bs where
  fragUnsafeCast :: Fragment asbs as'bs
  fragUnsafeCast = fragVeryUnsafeCast
instance FragUnsafeCast '[] asbs asbs
instance FragUnsafeCast as' asbs as'bs => FragUnsafeCast (a' ': as') (a ': asbs) (a' : as'bs)
