{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Patterns.Hexcassettes where

import Haskcasting.ExprLang.TH (mkIotaFragExpr)
import Haskcasting.Fragment (Fragment)
import Haskcasting.Iota (IotaAnyList, IotaExec, IotaNumber)
import Haskcasting.Pattern (pattern)

$( mkIotaFragExpr
     "Enqueue"
     [pattern| EAST qeqwqwqwqwqeqaweqqqqqwweeweweewqdwwewewwewweweww |]
     [[t|forall a bs. Fragment '[a, IotaNumber, IotaExec '[] bs] '[]|]]
 )

$( mkIotaFragExpr
     "Dequeue"
     [pattern| WEST eqeweweweweqedwqeeeeewwqqwqwqqweawwqwqwwqwwqwqww |]
     [[t|forall a. Fragment '[a] '[]|]]
 )

$( mkIotaFragExpr
     "Disqueue"
     [pattern| WEST eqeweweweweqedwqeeeeewwqqwqwqqw |]
     [[t|Fragment '[] '[]|]]
 )

$( mkIotaFragExpr
     "ThreadingReflection"
     [pattern| EAST qeqwqwqwqwqeqaweqqqqq |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "ThreadingReflectionII"
     [pattern| EAST qeqwqwqwqwqeqaweqqqqqwweeweweew |]
     [[t|Fragment '[] '[IotaNumber]|]]
 )

$( mkIotaFragExpr
     "ProgramPurification"
     [pattern| WEST eqeweweweweqedwqeeeee |]
     [[t|forall a. Fragment '[a] '[IotaAnyList]|]]
 )

$( mkIotaFragExpr
     "ProgramPurificationII"
     [pattern| WEST eqeweweweweqedwqeeeeedww |]
     [[t|forall a. Fragment '[a] '[IotaNumber]|]]
 )
