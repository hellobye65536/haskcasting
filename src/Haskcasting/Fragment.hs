module Haskcasting.Fragment (Fragment (..), fragmentAsList, (+.+), fragCast) where

import Data.HList.HList (HAppendListR, HStripPrefix)
import Data.Kind (Type)
import Data.Sequence (Seq (Empty))
import Haskcasting.Iota (IotaAny, IotaAnyList, IotaList (IotaList), IotaCast)

data Fragment :: [Type] -> [Type] -> Type where
  Fragment :: Seq IotaAny -> Fragment a b

infixl 9 +.+
(+.+) :: HStripPrefix as asbs bs => Fragment input asbs -> Fragment as as' -> Fragment input (HAppendListR as' bs)
Fragment l +.+ Fragment r = Fragment (l <> r)

fragmentAsList :: Fragment a b -> IotaAnyList
fragmentAsList (Fragment xs) = IotaList xs

class FragCast as bs where
  fragCast :: Fragment as bs
  fragCast = Fragment Empty
instance FragCast '[] '[]
instance (FragCast as bs, IotaCast a b) => FragCast (a ': as) (b ': bs)

-- class LiftFragment as as' asbs as'bs | as as' asbs -> as'bs, as as' as'bs -> asbs
-- instance LiftFragment '[] '[] bs bs
-- instance LiftFragment as as' asbs as'bs => LiftFragment (a ': as) as' (a ': asbs) as'bs
-- instance LiftFragment as as' asbs as'bs => LiftFragment as (a' ': as') asbs (a' ': as'bs)

-- infixl 9 +.+
-- (+.+) :: LiftFragment as as' asbs as'bs => Fragment input asbs -> Fragment as as' -> Fragment input as'bs
-- Fragment l +.+ Fragment r = Fragment (l <> r)

-- class FragmentCompose asbs as as' as'bs where
--   (+.+) :: Fragment input asbs -> Fragment as as' -> Fragment input as'bs
--   Fragment l +.+ Fragment r = Fragment (l <> r)
-- instance LiftFragment as as' asbs as'bs => FragmentCompose asbs as as' as'bs

-- class FragmentCompose asbs as as' where
--   type FragmentComposed asbs as as' :: [Type]
--   (+.+) :: Fragment input asbs -> Fragment as as' -> Fragment input (FragmentComposed asbs as as')
--   Fragment l +.+ Fragment r = Fragment (l <> r)
-- instance FragmentCompose asbs '[] '[] where
--   type FragmentComposed asbs '[] '[] = asbs
-- instance FragmentCompose asbs '[] as' => FragmentCompose asbs '[] (a' ': as') where
--   type FragmentComposed asbs '[] (a' ': as') = a' ': FragmentComposed asbs '[] as'
-- instance FragmentCompose asbs as as' => FragmentCompose (a : asbs) (a : as) as' where
--   type FragmentComposed (a : asbs) (a ': as) as' = FragmentComposed asbs as as'

-- infixr 9 +.+
-- (+.+) ::
--   (ToFragment l input asbs, ToFragment r as as', SwapPrefix asbs as as' ~ as'bs) =>
--   l -> r -> Fragment input as'bs
-- l +.+ r = fragmentCompose l' r'
--  where
--   l' :: Fragment input asbs
--   l' = toFragment l
--   r' :: Fragment as as'
--   r' = toFragment r
