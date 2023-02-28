module Auctions.InterfacePBS
  where

import Auctions.Types


import Data.List (maximumBy)
import Data.Ord (comparing)

{-
Describes how different bids from different relays get exposed in the auction setting
This needs to be coordinated with the different designs 
-}

-- FIXME think about equal bids and their handling
findMaxBidRelay :: Relay -> Bid
findMaxBidRelay ls = maximumBy (comparing snd) ls

-- Aggregate the max bids from several relays into the list of bids which then enter the auction
aggregateSeveralRelays :: [Relay] -> [Bid]
aggregateSeveralRelays = fmap findMaxBidRelay

