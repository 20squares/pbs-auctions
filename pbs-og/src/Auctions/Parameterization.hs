module Auctions.Parameterization
  where

import Auctions.Types

{-
Defines the main parameterizations used in the analysis
-}

---------------------
-- Symmetric scenario
---------------------

zeroReservePrice = 0

privateValueLS = [0,3..9]
actionLS = [0,1..10] 

-- Current relay auction
parametersCurrentAuction = Parameters
  { nameProposer =  "proposer"
  , name1 = "bidder1"
  , name2 = "bidder2"
  , name3 = "bidder3"
  , name4 = "bidder4"
  , valueSpace1 = privateValueLS
  , valueSpace2 = privateValueLS
  , valueSpace3 = privateValueLS
  , valueSpace4 = privateValueLS
  , actionSpace1 = actionLS
  , actionSpace2 = actionLS
  , actionSpace3 = actionLS
  , actionSpace4 = actionLS
  , reservePrice = undefined
  , winningPrice = undefined
  , approxError = 0.4
  }

-- FP auction
parametersFPAuction = Parameters
  { nameProposer =  "proposer"
  , name1 = "bidder1"
  , name2 = "bidder2"
  , name3 = "bidder3"
  , name4 = "bidder4"
  , valueSpace1 = privateValueLS
  , valueSpace2 = privateValueLS
  , valueSpace3 = privateValueLS
  , valueSpace4 = privateValueLS
  , actionSpace1 = actionLS
  , actionSpace2 = actionLS
  , actionSpace3 = actionLS
  , actionSpace4 = actionLS
  , reservePrice = zeroReservePrice
  , winningPrice = 1
  , approxError  = 0.4
  }

-- 2ndP auction
parameters2ndPAuction = Parameters
  { nameProposer =  "proposer"
  , name1 = "bidder1"
  , name2 = "bidder2"
  , name3 = "bidder3"
  , name4 = "bidder4"
  , valueSpace1 = privateValueLS
  , valueSpace2 = privateValueLS
  , valueSpace3 = privateValueLS
  , valueSpace4 = privateValueLS
  , actionSpace1 = privateValueLS
  , actionSpace2 = privateValueLS
  , actionSpace3 = privateValueLS
  , actionSpace4 = privateValueLS
  , reservePrice = zeroReservePrice
  , winningPrice = 2
  , approxError  = 0
  }

-- All pay auction
parametersAllPayAuction = Parameters
  { nameProposer =  "proposer"
  , name1 = "bidder1"
  , name2 = "bidder2"
  , name3 = "bidder3"
  , name4 = "bidder4"
  , valueSpace1 = privateValueLS
  , valueSpace2 = privateValueLS
  , valueSpace3 = privateValueLS
  , valueSpace4 = privateValueLS
  , actionSpace1 = privateValueLS
  , actionSpace2 = privateValueLS
  , actionSpace3 = privateValueLS
  , actionSpace4 = privateValueLS
  , reservePrice = undefined
  , winningPrice = undefined
  , approxError = 0.4
  }
