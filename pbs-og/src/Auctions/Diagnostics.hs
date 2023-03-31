{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Auctions.Diagnostics
  where

import OpenGames.Engine.Engine

{-
Defines hand-rolled diagnostics for ease of analysis
-}

--------------------------
-- 0. Show all diagnostics
showDiagnosticInfo :: (Show y, Ord y, Show x) => DiagnosticInfoBayesian x y -> String
showDiagnosticInfo info =  
     "\n"    ++ "Player: " ++ player info
     ++ "\n" ++ "Optimal Move: " ++ (show $ optimalMove info)
     ++ "\n" ++ "Current Strategy: " ++ (show $ strategy info)
     ++ "\n" ++ "Optimal Payoff: " ++ (show $ optimalPayoff info)
     ++ "\n" ++ "Current Payoff: " ++ (show $ payoff info)
     ++ "\n" ++ "Observable State: " ++ (show $ state info)
     ++ "\n" ++ "Unobservable State: " ++ (show $ unobservedState info)

-- output string information for a subgame expressions containing information from several players - bayesian 
showDiagnosticInfoL :: (Show y, Ord y, Show x) => [DiagnosticInfoBayesian x y] -> String
showDiagnosticInfoL [] = "\n --No more information--"
showDiagnosticInfoL (x:xs)  = showDiagnosticInfo x ++ "\n --other game-- " ++ showDiagnosticInfoL xs 

-- checks equilibrium for the branching case
-- checks equilibrium and if not outputs relevant deviations
showDiagnosticInfoMaybeL :: (Show y, Ord y, Show x) => Maybe [DiagnosticInfoBayesian x y] -> String
showDiagnosticInfoMaybeL ls =
  case ls of
    Just ls' -> showDiagnosticInfoL ls'
    Nothing  -> "\n NOTHING CASE"

 -- checks equilibrium for the branching case -  one level nested 
showDiagnosticInfoMaybe2L :: (Show y, Ord y, Show x) => Maybe (Maybe [DiagnosticInfoBayesian x y]) -> String
showDiagnosticInfoMaybe2L ls =
  case ls of
    Just ls' -> showDiagnosticInfoMaybeL ls' 
    Nothing  -> "\n NOTHING CASE"

 -- checks equilibrium for the branching case -  two level nested 
showDiagnosticInfoMaybe3L :: (Show y, Ord y, Show x) => Maybe (Maybe (Maybe [DiagnosticInfoBayesian x y])) -> String
showDiagnosticInfoMaybe3L ls =
  case ls of
    Just ls' -> showDiagnosticInfoMaybe2L ls' 
    Nothing  -> "\n NOTHING CASE"

--------------------------------
-- 2. Only equilibrium analytics
-- checks equilibrium and if not outputs relevant deviations
checkEqL :: (Show y, Ord y, Show x) => [DiagnosticInfoBayesian x y] -> String
checkEqL ls = let xs = fmap equilibrium ls
                  ys = filter (\x -> equilibrium x == False) ls
                  isEq = and xs
                  in if isEq == True then "\n Strategies are in equilibrium"
                                      else "\n Strategies are NOT in equilibrium. Consider the following profitable deviations: \n"  ++ showDiagnosticInfoL ys

-- checks equilibrium for the branching case
-- checks equilibrium and if not outputs relevant deviations
checkEqMaybeL :: (Show y, Ord y, Show x) => Maybe [DiagnosticInfoBayesian x y] -> String
checkEqMaybeL ls =
  case ls of
    Just ls' -> checkEqL ls'
    Nothing  -> "\n NOTHING CASE"

 -- checks equilibrium for the branching case -  one level nested 
checkEqMaybe2L :: (Show y, Ord y, Show x) => Maybe (Maybe [DiagnosticInfoBayesian x y]) -> String
checkEqMaybe2L ls =
  case ls of
    Just ls' -> checkEqMaybeL ls' 
    Nothing  -> "\n NOTHING CASE"

 -- checks equilibrium for the branching case -  two level nested 
checkEqMaybe3L :: (Show y, Ord y, Show x) => Maybe (Maybe (Maybe [DiagnosticInfoBayesian x y])) -> String
checkEqMaybe3L ls =
  case ls of
    Just ls' -> checkEqMaybe2L ls' 
    Nothing  -> "\n NOTHING CASE"
   

