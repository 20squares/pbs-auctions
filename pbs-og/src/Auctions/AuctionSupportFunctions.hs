module Auctions.AuctionSupportFunctions where

import Auctions.Types

import OpenGames.Engine.Engine (Agent,Stochastic,uniformDist)

import Data.List (maximumBy, sortBy, permutations)
import Data.Ord (comparing)

{--
Contains basic functionality needed for different auction formats
-}




----------------------
-- Determine max bid
----------------------
-- Order bids from large to small
orderAllocation :: [Bid] -> [Bid]
orderAllocation  = sortBy (flip (\(_,v1) (_,v2) -> compare v1 v2 ))

-- Determine k-max bid
kMaxBid ::  WinningPrice -> [Bid] -> BidValue
kMaxBid k ls = snd $  orderAllocation ls !! (k-1)

-- Determine k-max bid
kMaxBidReservePrice :: BidValue -> WinningPrice -> [Bid] -> BidValue 
kMaxBidReservePrice resPrice k ls = snd $  orderAllocation (resPriceLs ls) !! (k-1)
   where   resPriceLs  []    = []
           resPriceLs ((n,v):xs) = if v > resPrice then (n,v)  : resPriceLs xs
                                             else (n,resPrice) : resPriceLs xs
-- Determine realized price
extractWinningBid :: [Bid] -> Bid
extractWinningBid = maximumBy (comparing snd) 

-- Mark the auctionWinners with reserve price
auctionWinnerReservePrice :: BidValue -> BidValue -> [Bid] -> [(Agent, BidValue,Bool)]
auctionWinnerReservePrice _            _    []            = []
auctionWinnerReservePrice reservePrice kmax ((name,b):bs) =
  if b < kmax || b < reservePrice
     then (name,b,False) : auctionWinner kmax bs
     else (name,b,True)  : auctionWinner kmax bs
  where
    -- Mark the auctionWinners
    auctionWinner :: BidValue -> [Bid] -> [(Agent, BidValue,Bool)]
    auctionWinner _    []            = []
    auctionWinner kmax ((name,b):bs) =
      if b < kmax then (name,b,False) : auctionWinner kmax bs
                  else (name,b,True)  : auctionWinner kmax bs



----------------
-- Payment rules
----------------

-- k- price auction rule, i.e. the sequence for winning bidders is ignored, winners always pay k-highest price
paymentReservePrice :: ReservePrice -> WinningBidValue ->  [(Agent,BidValue,Bool)] -> [Bid]
paymentReservePrice _        _     []                     = []
paymentReservePrice resPrice kmax  ((name,bid,winner):ls)
  | winner && bid >= resPrice = (name,bid) : payment resPrice kmax  ls
  | winner && bid <  resPrice = (name,resPrice) : payment resPrice kmax  ls
  | otherwise                 =  (name,0) : payment resPrice kmax  ls
  where
    -- k- price auction rule,  winners always pay k-highest price
    payment :: ReservePrice -> WinningBidValue -> [(Agent,BidValue,Bool)] -> [Bid]
    payment _        _     []                     = []
    payment resPrice kmax  ((name,bid,winner):ls) =
      if winner
          then (name,kmax) : payment resPrice kmax ls
          else (name,0) : payment resPrice kmax ls


-- Select the payment for a player given the list of payments
selectPayoffs :: Agent -> [Bid] -> BidValue
selectPayoffs name [] = 0
selectPayoffs name ((n,p):ls) = if name == n then p else selectPayoffs name ls

-- Determines the payoff for each player
setPayoff ::  Bid -> [Bid] -> BidValue
setPayoff (name,value) payments =
  if pay == 0 then 0 else value - pay
  where
    pay =  selectPayoffs name payments

-- Determine the payments given k-highest price (1,2..) 
auctionPaymentResPrice :: (ReservePrice -> WinningBidValue -> [(Agent,BidValue,Bool)] -> [Bid])
                 -- ^ Payment function
                 -> WinningPrice
                 -- ^ Which price determines outcome?
                 -> ([Bid],BidValue)
                 -- ^ List of bids with highest value
                 -> [Bid]
auctionPaymentResPrice paymentFunction winningPrice (ls,reservePrice) =
   paymentFunction reservePrice kMax (auctionWinnerReservePrice reservePrice kMax ls)
   where kMax = kMaxBidReservePrice reservePrice winningPrice ls
