{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Auctions.Analytics
   where

import Auctions.AuctionSupportFunctions
import Auctions.Diagnostics
import Auctions.Model
import Auctions.Types

import OpenGames.Engine.Engine hiding (Payoff)
import OpenGames.Preprocessor


import qualified Control.Monad.State  as ST
import qualified Numeric.Probability.Distribution as P

{-
Contains the basic parameterization shared across different auctions.
There are two type of analyses:
1. (Bayesian) Nash eq. checks
2. Simulations
-}

-------------------------
-- 0. Auxiliary functions
-------------------------

-- Computes the expected payment for each bidder
formExpectedPayment
  :: Stochastic [AuctionOutcome] -> [BidValue]
formExpectedPayment ls =
  let transformedLS = fmap (\(valueLS,p) -> fmap (\(_,b,_) -> b *p) valueLS) (P.decons ls)
      in foldr1 (zipWith (+)) transformedLS

--------------------------
-- 1. Equilibrium checking
--------------------------

-- The current status quo
equilibriumCurrentAuction Parameters{..} strategy = evaluate (currentAuctionGame nameProposer name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 approxError) strategy ctxt
 where
   ctxt = StochasticStatefulContext (pure ((),())) (\_ _ -> pure ())

printEquilibriumCurrentAuction parameters strategy = generateIsEq $ equilibriumCurrentAuction parameters strategy

-- Simultaneous bid auction
equilibriumSimultaneousBidAuction Parameters{..} strategy = evaluate (reservePriceExogenous name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 winningPrice reservePrice approxError) strategy ctxt
 where
   ctxt = StochasticStatefulContext (pure ((),())) (\_ _ -> pure ())

printEquilibriumSimultaneousBidAuction parameters strategy = generateIsEq $ equilibriumSimultaneousBidAuction parameters strategy


-- All pay auction
equilibriumAllPayAuction Parameters{..} strategy = evaluate (biddingAllPay  name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 approxError) strategy ctxt
 where
   ctxt = StochasticStatefulContext (pure ((),())) (\_ _ -> pure ())

printEquilibriumAllPayAuction parameters strategy = generateIsEq $ equilibriumAllPayAuction parameters strategy

-----------------
-- 2. Simulations
-----------------

-- Current status quo
simulateCurrentAuction Parameters{..} strategy = play (currentAuctionGame nameProposer name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 approxError) strategy 

printSimulationCurrentAuction parameters strategy = formExpectedPayment $ nextState (simulateCurrentAuction parameters strategy) ()

-- Simultaneous bid auction
simulateSimultaneousBidAuction Parameters{..} strategy = play (reservePriceExogenous name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 winningPrice reservePrice approxError) strategy

printSimulationSimultaneousBidAuction parameters strategy = formExpectedPayment $ nextState (simulateSimultaneousBidAuction parameters strategy) ()

-- All pay auction
simulateAllPayAuction Parameters{..} strategy = play (biddingAllPay  name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 approxError) strategy

printSimulationAllPayAuction parameters strategy = formExpectedPayment $ nextState (simulateAllPayAuction parameters strategy) ()


---------------------
-- 3 Dynamic auctions
---------------------

----------------------------------
-- Preparing Markov game structure

-- Determine continuation payoff with the same repeated strategy
determineContinuationPayoffs par 1        strat action = pure ()
determineContinuationPayoffs par iterator strat action = do
   nextContinuation executeStrat action
   nextInput <- ST.lift $ nextState executeStrat action
   determineContinuationPayoffs par  (pred iterator) strat nextInput
 where
   executeStrat =  play (game par) strat
   game ParametersJapaneseAuction{..}  = fullStateGame jname1 jname2 jname3 jname4 jvalueSpace1 jvalueSpace2 jvalueSpace3 jvalueSpace4 jactionSpace1 jactionSpace2 jactionSpace3 jactionSpace4 japproxError jincreasePerRound terminationRuleJapaneseAuction japaneseAuctionPayments

-- Context used for the evaluation of the current stage game 
contextCont par iterator strat initialAction = StochasticStatefulContext (pure ((),initialAction)) (\_ action -> determineContinuationPayoffs par iterator strat action)
  where
   game ParametersJapaneseAuction{..}  = fullStateGame jname1 jname2 jname3 jname4 jvalueSpace1 jvalueSpace2 jvalueSpace3 jvalueSpace4 jactionSpace1 jactionSpace2 jactionSpace3 jactionSpace4 japproxError jincreasePerRound terminationRuleJapaneseAuction japaneseAuctionPayments

--------------
-- Equilibrium

-- Define eq of repeated game
repeatedStageGameEq par iterator strat initialAction = evaluate (game par) strat context
  where
    context  = contextCont par iterator strat initialAction
    game ParametersJapaneseAuction{..}  = fullStateGame jname1 jname2 jname3 jname4 jvalueSpace1 jvalueSpace2 jvalueSpace3 jvalueSpace4 jactionSpace1 jactionSpace2 jactionSpace3 jactionSpace4 japproxError jincreasePerRound terminationRuleJapaneseAuction japaneseAuctionPayments

-- Show equilibrium output
printEquilibriumDynamicAuction par iterator strat initialAction = do
  let bidder1 ::- bidder2 ::- bidder3 ::- bidder4 ::- Nil = repeatedStageGameEq par iterator strat initialAction
  putStrLn "Bidder1: "
  putStrLn $ checkEqMaybe2L bidder1
  putStrLn "Bidder2: "
  putStrLn $ checkEqMaybe2L bidder2
  putStrLn "Bidder3: "
  putStrLn $ checkEqMaybe2L bidder3
  putStrLn "Bidder4: "
  putStrLn $ checkEqMaybe2L bidder4

-- Show full output
printOutputDynamicAuction par iterator strat initialAction = do
  let bidder1 ::- bidder2 ::- bidder3 ::- bidder4 ::- Nil = repeatedStageGameEq par iterator strat initialAction
  putStrLn "Bidder1: "
  putStrLn $ showDiagnosticInfoMaybe2L bidder1
  putStrLn "Bidder2: "
  putStrLn $ showDiagnosticInfoMaybe2L bidder2
  putStrLn "Bidder3: "
  putStrLn $ showDiagnosticInfoMaybe2L bidder3
  putStrLn "Bidder4: "
  putStrLn $ showDiagnosticInfoMaybe2L bidder4



-- Run step-wise simulation
simulateRepeatedStageGame par iterator strat = play (game par) strat 
  where
    game ParametersJapaneseAuction{..}  = fullStateGame jname1 jname2 jname3 jname4 jvalueSpace1 jvalueSpace2 jvalueSpace3 jvalueSpace4 jactionSpace1 jactionSpace2 jactionSpace3 jactionSpace4 japproxError jincreasePerRound terminationRuleJapaneseAuction japaneseAuctionPayments

printSimulationRepeatedStageGame par iterator strat initialAction = print $ P.decons $ nextState (simulateRepeatedStageGame par iterator strat) initialAction

