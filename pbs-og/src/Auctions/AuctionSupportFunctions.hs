module Auctions.AuctionSupportFunctions where

import Auctions.Types

import OpenGames.Engine.Engine (Agent,Stochastic,uniformDist,pureAction,playDeterministically)

import Data.List (maximumBy, sortBy, permutations)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Numeric.Probability.Distribution (decons)

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

-- Mark the auctionWinners
-- In case of a tie, a single bidder is determined (with probability 1)
auctionWinner :: [Bid] -> Stochastic [AuctionOutcome]
auctionWinner bids =
  let outcomes = fmap generateOutcome bids
      winnersOnly = filter (\(_,_,won) -> won == True) outcomes
      in case length winnersOnly of
           1 -> playDeterministically outcomes
           _ -> do
                (winnerName,_,_) <- uniformDist winnersOnly
                let outcomes' = fmap (\(name,b,won) -> if name /= winnerName then (name,b,False) else (name,b,won)) outcomes
                    in playDeterministically outcomes'
  where kmax = snd $ extractWinningBid bids
        generateOutcome (name,b) =
            if b < kmax
              then (name,b,False)
              else (name,b,True)

----------------
-- Payment rules
----------------

-- k- price auction rule, i.e. the sequence for winning bidders is ignored, winners always pay k-highest price
paymentReservePrice :: ReservePrice -> WinningBidValue ->  [(Agent,BidValue,Bool)] -> [AuctionOutcome]
paymentReservePrice _        _     []                     = []
paymentReservePrice resPrice kmax  ((name,bid,winner):ls)
  | winner && bid >= resPrice = (name,kmax, winner) : payment ls
  | winner && bid <  resPrice = (name,resPrice, winner) : payment ls
  | otherwise                 =  (name,0, winner ) : payment ls
  where
    -- k- price auction rule,  winners always pay k-highest price
    payment :: [(Agent,BidValue,Bool)] -> [AuctionOutcome]
    payment []                     = []
    payment ((name,bid,winner):ls) =
      if winner
          then (name,kmax, winner) : payment ls
          else (name,0, winner) : payment ls
    
-- Determine the payments given k-highest price (1,2..) 
auctionPaymentResPrice :: (ReservePrice -> WinningBidValue -> [(Agent,BidValue,Bool)] ->  [AuctionOutcome])
                 -- ^ Payment function
                 -> WinningPrice
                 -- ^ Which price determines outcome?
                 -> ([Bid],ReservePrice)
                 -- ^ List of bids with reserve price
                 -> Stochastic [AuctionOutcome]
auctionPaymentResPrice paymentFunction winningPrice (ls,reservePrice) = do
   winnerAuction <- (auctionWinner ls)
   -- ^^ Extract the winner
   let kmax = kMaxBidReservePrice reservePrice winningPrice ls
   -- ^^ Determine the max value bid
   pure $ paymentFunction reservePrice kmax winnerAuction

-- All pay auction rule, i.e. every player pays the bid
-- NOTE that we include the information on winning 
auctionPaymentAllPay :: [Bid] -> Stochastic [AuctionOutcome]
auctionPaymentAllPay = auctionWinner

-----------------
-- Select payoffs
-----------------

-- Select the payment for a player given the list of payments
-- TODO Fix possible non defined agent
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


--------------
-- Scalability
--------------

-- Aggregate lists of bids
aggregateBidsLS :: ([Bid],[Bid]) -> [Bid]
aggregateBidsLS (b1,b2) = b1 ++ b2

------------------------------------------
-- Additional functionality for status quo
------------------------------------------
-- Aggregate a pair of bids
aggregateBids :: (Bid,Bid) -> Relay
aggregateBids (b1,b2) = [b1,b2]

-- Find max bid in relay
findMaxBidRelay :: Relay -> Stochastic Bid
findMaxBidRelay bidLS = do
  let kmax = maximum $ fmap snd bidLS
      maxLS = filter (\(_,v) -> v == kmax) bidLS
      in case length maxLS of
            1 -> playDeterministically $ head maxLS
            _ -> uniformDist maxLS


-- Extract payment for proposer
extractProposerPayment :: Bid -> BidValue
extractProposerPayment (name,value) = value


-- Compute payoffs for sending them back
-- This takes the winning bid and then transforms all bids into an auction outcome
computeOutcomeFunction :: Bid -> [Bid] -> [AuctionOutcome]
computeOutcomeFunction (agent,bid) ls =
  let updateFunction (k,v) =
        if k == agent
           then (k,bid,True)
           else (k,0,False)
     in fmap updateFunction ls

----------------------------------------------------
-- EXPERIMENTAL Additional functionality for hierarchical auctions
----------------------------------------------------

-- Extract winning agent and overall bids available for the next stage
-- FIXME clarify whether values are observable from intermediate auctions and change type accordingly
-- NOTE: this allows for non winning players having to make payments
bidsAvailableToAuctioneer :: Agent -> [AuctionOutcome] -> (Agent,PrivateValue)
bidsAvailableToAuctioneer auctioneer ls =  
  let winner = head [a | (a,_,won) <- ls, won == True]
      value  = sum [v | (_,v,_) <- ls]
      in (auctioneer ++ "winner" ++ winner, value)

-- FIXME improve and make it more robust
payoffsSplitAuctioneer :: Agent -> [AuctionOutcome] -> (Payoff,Bool)
payoffsSplitAuctioneer agent lsOutcomes =
  let (agent',bidValue,blockWon) = head $ filter (\(agent',bidValue,blockWon) -> agent'== agent ) lsOutcomes
      in (bidValue,blockWon)

-- Account for payoffs players
accountForPayoffs :: Agent -> (Payoff,Bool) -> [AuctionOutcome] -> ((Agent,Payoff),[AuctionOutcome])
accountForPayoffs agent (bid,won) lsOutcomes
  | won == False = ((agent,0),[(a,0,False)| (a,_,_) <- lsOutcomes])
  | otherwise    =
    let profit = sum $ fmap (\(_,v,_) -> v) lsOutcomes
        in ((agent,profit - bid), lsOutcomes)
      
