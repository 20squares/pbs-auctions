{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PolyKinds #-}

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

privateValueLS :: [PrivateValue]
privateValueLS = [0,3..9]
actionLS :: [BidValue]
actionLS = [0,1..10]

boolLs = [True,False]

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

-- Japanese auction
parametersJapaneseAuction = ParametersJapaneseAuction
  { jname1 = "bidder1"
  , jname2 = "bidder2"
  , jname3 = "bidder3"
  , jname4 = "bidder4"
  , jvalueSpace1 = privateValueLS
  , jvalueSpace2 = privateValueLS
  , jvalueSpace3 = privateValueLS
  , jvalueSpace4 = privateValueLS
  , jactionSpace1 = boolLs 
  , jactionSpace2 = boolLs
  , jactionSpace3 = boolLs
  , jactionSpace4 = boolLs
  , japproxError  = 0.2
  , jincreasePerRound = 1
  } 

initialAction
  :: Either
       a
       ([(String, Bool)], [(String, Bool)], Double,
        Maybe (String, Double), Maybe (String, Double),
        Maybe (String, Double), Maybe (String, Double), Bool, Bool, Bool,
        Bool)
initialAction = Right ([("bidder1",True),("bidder2",True),("bidder3",True),("bidder4",True)],[("bidder1",False),("bidder2",False),("bidder3",True),("bidder4",True)],5,Just ("bidder1",5),Just ("bidder2",5),Just ("bidder3",7),Just ("bidder4",10),False,False,True,True)
