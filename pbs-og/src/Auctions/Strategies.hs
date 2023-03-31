{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Auctions.Strategies
  where


import Auctions.Types

import OpenGames.Engine.Engine

import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Bool (Bool(True))

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

bidAllPay
  :: Kleisli
       Stochastic
       (Agent, PrivateValue)
       BidValue
bidAllPay =
  Kleisli (\((_,value)) -> matchDiscreteSchedule  value)
  where matchDiscreteSchedule 0 = playDeterministically 0
        matchDiscreteSchedule 3 = playDeterministically 0
        matchDiscreteSchedule 6 = uniformDist [0,3]
        matchDiscreteSchedule 9 = uniformDist [3,6]


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
      let kmax = maximum $ fmap snd bidLS
          maxLS = filter (\(_,v) -> v == kmax) bidLS
          in case length maxLS of
               1 -> playDeterministically $ head maxLS
               _ -> uniformDist maxLS)

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

-- | Bidding for current auction
currentAuctionShareOfValueStrategy x =
      bidShareOfValue x
  ::- bidShareOfValue x
  ::- bidShareOfValue x
  ::- bidShareOfValue x
  ::- proposerMaxBid
  ::- Nil

-- | Bidding for all pay auction
allPayAuctionStrategyTuple  =
      bidAllPay
  ::- bidAllPay
  ::- bidAllPay
  ::- bidAllPay
  ::- Nil

---------------------
-- Dynamic strategies

-- | Truth telling strategy
-- Restrict the strategy space
participateBelowPriceStrategy
  :: Kleisli
       Stochastic
       (BidValue,Bool,PrivateNameValue)
       Bool
participateBelowPriceStrategy =
  Kleisli
    (\(value,bool,(_,privateValue)) ->
       case bool of
         False -> playDeterministically False
         True  -> case value < privateValue of
           True  -> playDeterministically True
           False -> playDeterministically False)

japaneseAuctionStrategyTuple =
      participateBelowPriceStrategy
  ::- participateBelowPriceStrategy
  ::- participateBelowPriceStrategy
  ::- participateBelowPriceStrategy
  ::- Nil 
