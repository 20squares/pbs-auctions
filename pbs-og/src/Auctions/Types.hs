module Auctions.Types
  where

import OpenGames.Engine.Engine (Agent)

type PrivateValue = Double

type BidValue = Double

type ReservePrice = BidValue

type WinningBidValue = BidValue

type Bid = (Agent, BidValue)

type Relay = [Bid]

type AuctionOutcome = (Agent, BidValue, BlockWon)

type BlockWon = Bool

-- | 1st price, snd price etc.
type WinningPrice = Int 

type Payoff = Double
