module Auctions.Types
  where

import OpenGames.Engine.Engine (Agent)



type PrivateValue = Double        -- The private valuation that a player has

type BidValue = Double            -- The public valuation that the player bids

type ReservePrice = BidValue      -- The minimum price that the seller would accept from a bid

type WinningBidValue = BidValue   -- The value of the winning bid

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
  } deriving (Show)
