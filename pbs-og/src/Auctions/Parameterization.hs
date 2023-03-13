module Auctions.Parameterization
  where

import Auctions.Types

{-
Defines the main parameterizations used in the analysis
-}

zeroReservePrice = 0

privateValueLS = [0,0.5..10]

-- We describe a symmetric scenario first
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
  , actionSpace1 = privateValueLS
  , actionSpace2 = privateValueLS
  , actionSpace3 = privateValueLS
  , actionSpace4 = privateValueLS
  , reservePrice = zeroReservePrice
  , winningPrice = 2
  }

-- We describe a symmetric scenario first
parameters2PAuction = Parameters
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
  }
