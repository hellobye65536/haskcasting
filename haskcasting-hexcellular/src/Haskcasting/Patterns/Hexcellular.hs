{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskcasting.Patterns.Hexcellular where

import Haskcasting.ExprLang.TH (mkIotaFragExpr)
import Haskcasting.Fragment (Fragment)
import Haskcasting.Iota (IotaAny)
import Haskcasting.Iota.Hexcellular (IotaProperty (..))
import Haskcasting.Pattern (pattern)

$( mkIotaFragExpr
     "SchrodingersReflection"
     [pattern| SOUTH_WEST aawe |]
     [[t|Fragment '[] '[IotaProperty]|]]
 )

$( mkIotaFragExpr
     "ObservationPurification"
     [pattern| SOUTH_WEST aawd |]
     [[t|Fragment '[IotaProperty] '[IotaAny]|]]
 )

$( mkIotaFragExpr
     "SchrodingersGambit"
     [pattern| SOUTH_WEST aawq |]
     [[t|forall a. Fragment '[a, IotaProperty] '[]|]]
 )

$( mkIotaFragExpr
     "SchrodingersPurification"
     [pattern| SOUTH_WEST aawa |]
     [[t|Fragment '[IotaProperty] '[IotaProperty]|]]
 )
