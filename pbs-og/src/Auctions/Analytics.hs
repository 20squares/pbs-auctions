{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Auctions.Analytics
   where

import Auctions.Model
import Auctions.Types

import OpenGames.Engine.Engine
import OpenGames.Preprocessor


{-
Contains the basic parameterization shared across different auctions.
There are two type of analyses:
1. (Bayesian) Nash eq. checks
2. Simulations
-}

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

printSimulationCurrentAuction parameters strategy = print $ nextState (simulateCurrentAuction parameters strategy) ()

-- Simultaneous bid auction
simulateSimultaneousBidAuction Parameters{..} strategy = play (reservePriceExogenous name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 winningPrice reservePrice approxError) strategy

printsimulateSimultaneousBidAuction parameters strategy = print $ nextState (simulateSimultaneousBidAuction parameters strategy) ()
