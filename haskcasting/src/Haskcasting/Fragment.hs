{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskcasting.Fragment (
  Fragment (..),
  (+.+),
  fragAssertStack,
  fragEmpty,
  fragmentAsList,
  fragmentAsIota,
  fragCast,
  fragUnsafeCast,
  fragVeryUnsafeCast,
) where

import Data.Kind (Type)
import Data.Sequence (Seq (Empty))
import Haskcasting.Iota (IotaAny, IotaCast (iotaCast), IotaExec (IotaExec), IotaList (IotaList), IotaAnyList)

data Fragment :: [Type] -> [Type] -> Type where
  Fragment :: Seq IotaAny -> Fragment a b

infixl 9 +.+
(+.+) :: Fragment as bs -> Fragment bs cs -> Fragment as cs
Fragment l +.+ Fragment r = Fragment (l <> r)

fragAssertStack :: Fragment as as
fragAssertStack = Fragment Empty

fragEmpty :: Fragment '[] '[]
fragEmpty = fragAssertStack @'[]

fragmentAsList :: Fragment a b -> IotaAnyList
fragmentAsList (Fragment xs) = IotaList xs

fragmentAsIota :: Fragment a b -> IotaExec a b
fragmentAsIota (Fragment xs) = IotaExec $ iotaCast $ IotaList xs

class FragCast as' asbs as'bs | as' asbs -> as'bs where
  fragCast :: Fragment asbs as'bs
  fragCast = Fragment Empty
instance FragCast '[] asbs asbs
instance (FragCast as' asbs as'bs, IotaCast a a') => FragCast (a' ': as') (a ': asbs) (a' : as'bs)

class FragUnsafeCast as' asbs as'bs | as' asbs -> as'bs where
  fragUnsafeCast :: Fragment asbs as'bs
  fragUnsafeCast = Fragment Empty
instance FragUnsafeCast '[] asbs asbs
instance FragUnsafeCast as' asbs as'bs => FragUnsafeCast (a' ': as') (a ': asbs) (a' : as'bs)

fragVeryUnsafeCast :: forall bs as. Fragment as bs
fragVeryUnsafeCast = Fragment Empty
