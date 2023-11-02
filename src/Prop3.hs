{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

{-@ LIQUID "--compile-spec" @-}
{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple-local" @-}

module Prop3 where

import Data
import Proof
import Tactic.Core.Quote

{-@ reflect prop @-}
{-@ prop :: ListN -> Bool @-}
prop :: ListN -> Bool
prop xs = dropWhileListN (constant False) xs == xs

{-@ automatic-instances prop @-}
{-@
proof :: xs:ListN -> {prop xs}
@-}
[tactic|
  proof :: ListN -> Proof
  proof xs = \
    induct xs as [/x' xs'];
    [x']: condition {constant False x'}
|]

--  %tactic:begin:proof
--  proof :: ListN -> Proof
--  proof = \xs -> case xs of
--                     Data.Nil -> trivial
--                     Data.Cons x' xs' -> if constant False x' then trivial else trivial
--  %tactic:end:proof
