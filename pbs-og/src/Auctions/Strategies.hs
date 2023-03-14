{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Auctions.Strategies
  where


import Auctions.Types

import OpenGames.Engine.Engine

import Data.List (maximumBy)
import Data.Ord (comparing)

{-
Defines the strategies
-}

------------------------------
-- Individual bidding behavior

-- | Truth telling strategy
truthTellingStrategy
  :: Kleisli
       Stochastic
       (Agent, PrivateValue)
       BidValue
truthTellingStrategy =
  Kleisli (\((_,value)) -> playDeterministically $ value)

-- | Bid a fraction of the valuation
-- NOTE this can be used for manually adjusting bids
bidShareOfValue
  :: Double
  -> Kleisli
       Stochastic
       (Agent, PrivateValue)
       BidValue
bidShareOfValue share =
  Kleisli (\((_,value)) -> playDeterministically $ roundTo 1 (share * value))

roundTo :: RealFrac a => a -> a -> a
roundTo threshold x = fromIntegral (round (x / threshold)) * threshold

matchValues x
  | x == 0 = 0
  | x == 2 = 1.5
  | x == 4 = 3
  | x == 6 = 4.5
  | x == 8 = 6
  | x == 10 = 6.5
--------------------
-- Proposer strategy
proposerMaxBid
  :: Kleisli
       Stochastic
       [Bid]
       Bid
proposerMaxBid = 
 Kleisli
   (\bidLS ->
      let maxBid = maximumBy (comparing snd) bidLS
          in playDeterministically $ maxBid)

------------------------------
-- Aggregating complete tuples

-- | truth-telling
truthTellingStrategyTuple =
      truthTellingStrategy
  ::- truthTellingStrategy
  ::- truthTellingStrategy
  ::- truthTellingStrategy
  ::- Nil

-- | share bidding
-- NOTE we assume symmetry for now
bidShareOfValueStrategyTuple x =
      bidShareOfValue x
  ::- bidShareOfValue x
  ::- bidShareOfValue x
  ::- bidShareOfValue x
  ::- Nil

currentAuctionShareOfValueStrategy x =
      bidShareOfValue x
  ::- bidShareOfValue x
  ::- bidShareOfValue x
  ::- bidShareOfValue x
  ::- proposerMaxBid
  ::- Nil
