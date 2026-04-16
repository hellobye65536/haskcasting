{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Patterns.Hexcassettes where

import Haskcasting.Fragment (Fragment)
import Haskcasting.Iota (IotaExec, IotaNumber, IotaAnyList)
import Haskcasting.Pattern (pattern)
import Haskcasting.ExprLang.TH (mkIotaFragExpr)

$( mkIotaFragExpr
     "Enqueue"
     [pattern| EAST qeqwqwqwqwqeqaweqqqqqwweeweweewqdwwewewwewweweww |]
     [[t|forall a bs. Fragment '[IotaExec '[] bs, IotaNumber, a] '[]|]]
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
