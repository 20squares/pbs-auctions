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

-- Mark the auctionWinners
auctionWinner :: BidValue -> [Bid] -> [AuctionOutcome]
auctionWinner _    []            = []
auctionWinner kmax ((name,b):bs) =
  if b < kmax then (name,b,False) : auctionWinner kmax bs
              else (name,b,True)  : auctionWinner kmax bs



----------------
-- Payment rules
----------------

-- k- price auction rule, i.e. the sequence for winning bidders is ignored, winners always pay k-highest price
paymentReservePrice :: ReservePrice -> WinningBidValue ->  [(Agent,BidValue,Bool)] -> [AuctionOutcome]
paymentReservePrice _        _     []                     = []
paymentReservePrice resPrice kmax  ((name,bid,winner):ls)
  | winner && bid >= resPrice = (name,bid, winner) : payment resPrice kmax  ls
  | winner && bid <  resPrice = (name,resPrice, winner) : payment resPrice kmax  ls
  | otherwise                 =  (name,0, winner ) : payment resPrice kmax  ls
  where
    -- k- price auction rule,  winners always pay k-highest price
    payment :: ReservePrice -> WinningBidValue -> [(Agent,BidValue,Bool)] -> [AuctionOutcome]
    payment _        _     []                     = []
    payment resPrice kmax  ((name,bid,winner):ls) =
      if winner
          then (name,kmax, winner) : payment resPrice kmax ls
          else (name,0, winner) : payment resPrice kmax ls

-- Determine the payments given k-highest price (1,2..) 
auctionPaymentResPrice :: (ReservePrice -> WinningBidValue -> [(Agent,BidValue,Bool)] -> [AuctionOutcome])
                 -- ^ Payment function
                 -> WinningPrice
                 -- ^ Which price determines outcome?
                 -> ([Bid],ReservePrice)
                 -- ^ List of bids with reserve price
                 -> [AuctionOutcome]
auctionPaymentResPrice paymentFunction winningPrice (ls,reservePrice) =
   paymentFunction reservePrice kMax (auctionWinnerReservePrice reservePrice kMax ls)
   where kMax = kMaxBidReservePrice reservePrice winningPrice ls

-- All pay auction rule, i.e. every player pays the bid
-- NOTE that we include the information on winning 
auctionPaymentAllPay :: [Bid] -> [AuctionOutcome]
auctionPaymentAllPay ls =
  let kmax = kMaxBid 1 ls -- ^ Determine the winning bid
      in  auctionWinner kmax ls 

-----------------
-- Select payoffs
-----------------

-- Select the payment for a player given the list of payments
selectPayoffs :: Agent -> [AuctionOutcome] -> (BidValue,BlockWon)
selectPayoffs name [] = (0,False)
selectPayoffs name ((n,p,w):ls) = if name == n then (p,w) else selectPayoffs name ls

-- Determines the payoff for each player
setPayoff :: (Agent, PrivateValue) -> [AuctionOutcome] -> Payoff
setPayoff (name,value) payments =
  if won == True
     then value - pay
     else - pay
  where
    (pay,won) =  selectPayoffs name payments

----------------------------------------------------
-- Additional functionality for hierarchical auctions
----------------------------------------------------

-- Extract winning agent and overall bids available for the next stage
-- FIXME clarify whether values are observable from intermediate auctions and change type accordingly
-- NOTE: this allows for non winning players having to make payments
bidsAvailableToAuctioneer :: Agent -> [AuctionOutcome] -> (Agent,PrivateValue)
bidsAvailableToAuctioneer auctioneer ls =  
  let winner = head [a | (a,_,won) <- ls, won == True]
      value  = sum [v | (_,v,_) <- ls]
      in (auctioneer ++ "winner" ++ winner, value)
