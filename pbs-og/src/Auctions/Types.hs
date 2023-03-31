{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PolyKinds #-}

module Auctions.Types
  where

import OpenGames.Engine.Engine (Agent)
import GHC.Records (HasField)

-- Basic types

type PrivateValue = Double        -- The private valuation that a player has

type PrivateNameValue = (Agent,PrivateValue)        -- The private valuation that a player has

type BidValue = Double            -- The public valuation that the player bids

type ReservePrice = BidValue      -- The minimum price that the seller would accept from a bid

type WinningBidValue = BidValue   -- The value of the winning bid

-- Simultaneous bids

type Bid = (Agent, BidValue)
type Relay = [Bid]                -- At this stage a Relay is just a list of bids

type AuctionOutcome = (Agent, BidValue, BlockWon) 

type BlockWon = Bool

type WinningPrice = Int           -- Establishes if the winning price is 1st price, 2nd price, etc.

type Payoff = Double

data Parameters = Parameters
  { nameProposer :: Agent
  , name1 :: Agent
  , name2 :: Agent
  , name3 :: Agent
  , name4 :: Agent
  , valueSpace1 :: [PrivateValue]
  , valueSpace2 :: [PrivateValue]
  , valueSpace3 :: [PrivateValue]
  , valueSpace4 :: [PrivateValue]
  , actionSpace1 :: [BidValue]
  , actionSpace2 :: [BidValue]
  , actionSpace3 :: [BidValue]
  , actionSpace4 :: [BidValue]
  , reservePrice :: BidValue
  , winningPrice :: WinningPrice
  , approxError  :: Double
  } deriving (Show)

-------------------------
-- Dynamic Auctions types

-- Stay in the auction or leave
type BidJapaneseAuction = (Agent,Bool)

type BidsJapaneseAuction =[BidJapaneseAuction]

data ParametersJapaneseAuction = ParametersJapaneseAuction
  { jname1 :: Agent
  , jname2 :: Agent
  , jname3 :: Agent
  , jname4 :: Agent
  , jvalueSpace1 :: [PrivateValue]
  , jvalueSpace2 :: [PrivateValue]
  , jvalueSpace3 :: [PrivateValue]
  , jvalueSpace4 :: [PrivateValue]
  , jactionSpace1 :: (BidValue,Bool,PrivateNameValue) -> [Bool]
  , jactionSpace2 :: (BidValue,Bool,PrivateNameValue) -> [Bool]
  , jactionSpace3 :: (BidValue,Bool,PrivateNameValue) -> [Bool]
  , jactionSpace4 :: (BidValue,Bool,PrivateNameValue) -> [Bool]
  , japproxError  :: Double
  , jincreasePerRound :: Double
  }  

