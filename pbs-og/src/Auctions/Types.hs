module Auctions.Types
  where

import OpenGames.Engine.Engine (Agent)

type BidValue = Double

type ReservePrice = BidValue

type WinningBidValue = BidValue

type Bid = (Agent, BidValue)

-- | 1st price, snd price etc.
type WinningPrice = Int 
