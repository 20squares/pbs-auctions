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
The idea is to make the components comparable.
-}

-- The current status quo
equilibriumCurrentAuction Parameters{..} strategy = evaluate (currentAuctionGame nameProposer name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4) strategy ctxt
 where
   ctxt = StochasticStatefulContext (pure ((),())) (\_ _ -> pure ())

printEquilibriumCurrentAuction parameters strategy = generateIsEq $ equilibriumCurrentAuction parameters strategy

-- First price auction
equilibriumFPAuction Parameters{..} strategy = evaluate (reservePriceExogenous name1 name2 (winningPrice reservePrice valueSpace1 valueSpace2 actionSpace1 actionSpace2) strategy ctxt
 where
   ctxt = StochasticStatefulContext (pure ((),())) (\_ _ -> pure ())

printEquilibriumFPAuction parameters strategy = generateIsEq $ equilibriumCurrentAuction parameters strategy

