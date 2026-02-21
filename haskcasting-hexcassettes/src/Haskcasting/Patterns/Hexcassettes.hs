{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Patterns.Hexcassettes where

import Haskcasting.Fragment (Fragment)
import Haskcasting.Iota (IotaExec, IotaNumber, IotaAnyList)
import Haskcasting.Pattern (pattern)
import Haskcasting.Patterns.TH (mkIotaFrag)

$( mkIotaFrag
     "Enqueue"
     [pattern| EAST qeqwqwqwqwqeqaweqqqqqwweeweweewqdwwewewwewweweww |]
     [[t|forall a bs. Fragment '[IotaExec '[] bs, IotaNumber, a] '[]|]]
 )

$( mkIotaFrag
     "Dequeue"
     [pattern| WEST eqeweweweweqedwqeeeeewwqqwqwqqweawwqwqwwqwwqwqww |]
     [[t|forall a. Fragment '[a] '[]|]]
 )

$( mkIotaFrag
     "Disqueue"
     [pattern| WEST eqeweweweweqedwqeeeeewwqqwqwqqw |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFrag
     "ThreadingReflection"
     [pattern| EAST qeqwqwqwqwqeqaweqqqqq |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "ThreadingReflectionII"
     [pattern| EAST qeqwqwqwqwqeqaweqqqqqwweeweweew |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFrag
     "ProgramPurification"
     [pattern| WEST eqeweweweweqedwqeeeee |]
     [[t|forall a. Fragment '[a] '[IotaAnyList]|]]
 )

$( mkIotaFrag
     "ProgramPurificationII"
     [pattern| WEST eqeweweweweqedwqeeeeedww |]
     [[t|forall a. Fragment '[a] '[IotaNumber]|]]
 )
