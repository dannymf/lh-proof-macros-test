{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-@ LIQUID "--compile-spec" @-}
{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple-local" @-}

module Prop1 where

import Data
import Proof (Proof, trivial)
import Tactic.Core.Quote

{-@ reflect prop @-}
{-@ prop:: N -> Bool @-}
prop :: N -> Bool
prop m = (subN m m) == Z

{-@ automatic-instances prop_proof @-}
{-@
prop_proof :: m:N -> {prop m}
@-}
prop_proof :: N -> Proof
prop_proof m =
  case m of
    Z -> trivial
    S n_0 -> prop_proof n_0

-- [tactic|
-- prop_proof :: N -> Proof
-- prop_proof m = induct m

-- | ]
