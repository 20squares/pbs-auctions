{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Auctions.Components
  where

import Auctions.AuctionSupportFunctions

import OpenGames.Engine.Engine
import OpenGames.Preprocessor

---------------
-- 1 Components
---------------

----------
-- Bidding

-- Draws a value and creates a pair of _value_ _name_
natureDrawsTypeStage name valueSpace = [opengame|

    inputs    :   ;
    feedback  :   ;

    :-----:
    inputs    :   ;
    feedback  :   ;
    operation : nature (uniformDist valueSpace) ;
    outputs   : value ;
    returns   :  ;
    :-----:

    outputs   :  (name,value) ;
    returns   :    ;
  |]

-- Individual bidding stage
biddingStage name actionSpace approxError = [opengame|

    inputs    :  nameValuePair  ;
    feedback  :   ;

    :---------------------------:
    inputs    :  nameValuePair  ;
    feedback  :   ;
    operation :  dependentEpsilonDecision approxError name (const actionSpace) ;
    outputs   :  bid ;
    returns   :  setPayoff nameValuePair payments  ;
    :---------------------------:

    outputs   :  bid ;
    returns   :  payments  ;
  |]

-- Individual bidding stage for dynamic auction format
biddingStageDynamic name actionSpace approxError = [opengame|

    inputs    :  state, ownBidding, nameValuePair  ;
    feedback  :   ;

    :---------------------------:
    inputs    :  state, ownBidding, nameValuePair  ;
    feedback  :   ;
    operation :  dependentEpsilonDecision approxError name (const actionSpace) ;
    outputs   :  bid ;
    returns   :  0  ;
    // Payments are determined at the terminal stage
    :---------------------------:

    outputs   :  bid ;
    returns   :  ;
  |]


-----------
-- Payments
  
-- Transforms the bids and the relevant reservePrice into the payments by players
transformPaymentsReservePrice winningPrice  = [opengame|

   inputs    : (bids,reservePrice) ;
   feedback  :      ;

   :-----------------:
   inputs    : (bids,reservePrice) ;
   feedback  :      ;
   operation : forwardFunction (auctionPaymentResPrice paymentReservePrice winningPrice) ;
   outputs   : paymentsLottery ;
   returns   :      ;

   inputs    : paymentsLottery ;
   feedback  :      ;
   operation : natureEndInput ;
   outputs   : payments ;
   returns   :      ;
   :-----------------:

   outputs   : payments ;
   returns   :      ;
  |]

-- Transforms the bids into payments with exogenous reserve price
transformPayments winningPrice reservePrice = [opengame|

   inputs    : bids ;
   feedback  :      ;

   :-----------------:
   inputs    : (bids, reservePrice) ;
   feedback  :      ;
   operation : forwardFunction (auctionPaymentResPrice paymentReservePrice winningPrice) ;
   outputs   : paymentsLottery ;
   returns   :      ;

   inputs    : paymentsLottery ;
   feedback  :      ;
   operation : natureEndInput ;
   outputs   : payments ;
   returns   :      ;
   :-----------------:

   outputs   : payments ;
   returns   :      ;
  |]

-- Transforms the bids into payments for the all pay case 
transformAllPayPayments  = [opengame|

   inputs    : bids ;
   feedback  :      ;

   :-----------------:
   inputs    : bids ;
   feedback  :      ;
   operation : forwardFunction auctionPaymentAllPay ;
   outputs   : paymentsLottery ;
   returns   :      ;

   inputs    : paymentsLottery ;
   feedback  :      ;
   operation : natureEndInput ;
   outputs   : payments ;
   returns   :      ;
   :-----------------:

   outputs   : payments ;
   returns   :      ;
  |]

-- Given results of a previous auction, determine new possible input values for auctions
transformAuctionResultNewParticipant name = [opengame|

   inputs    : resultsAuction ;
   feedback  :      ;

   :-----------------:
   inputs    : resultsAuction ;
   feedback  :      ;
   operation : forwardFunction $ bidsAvailableToAuctioneer name ;
   outputs   : (newName,newValue) ;
   returns   :      ;
   :-----------------:

   outputs   : (newName,newValue) ;
   returns   :      ;

  |]

-- Given the result of an auction, parse out the result
transformAuctionOutcome name = [opengame|

   inputs    : resultsAuction ;
   feedback  :      ;

   :-----------------:
   inputs    : resultsAuction ;
   feedback  :      ;
   operation : forwardFunction $ payoffsSplitAuctioneer name ;
   outputs   : outcome ;
   returns   :      ;
   :-----------------:

   outputs   : outcome ;
   returns   :      ;

  |]

-- Given the result of an auction, parse out the result
determinePayoffs name = [opengame|

   inputs    : resultAuctioneer, resultsAuction ;
   feedback  :      ;

   :-----------------:
   inputs    : resultAuctioneer, resultsAuction ;
   feedback  :      ;
   operation : forwardFunction $ uncurry $ accountForPayoffs name ;
   outputs   : payoffAuctioneer,payoffBiders ;
   returns   :      ;
   :-----------------:

   outputs   : payoffAuctioneer,payoffBiders ;
   returns   :      ;

  |]

-- Given the bids, and the proposer's choice, determine outcome 
computeOutcomes  = [opengame|

   inputs    : winningBid, bids ;
   feedback  :      ;

   :-----------------:
   inputs    : winningBid, bids ;
   feedback  :      ;
   operation : forwardFunction $ uncurry $ computeOutcomeFunction ;
   outputs   : paymentsBidders ;
   returns   :      ;
   :-----------------:

   outputs   : paymentsBidders ;
   returns   :      ;

  |]

-------------------
-- Dynamic auctions
-------------------

-- Check if private information already exists; if not draw it; else use it

determinePrivateValue name valueSpace = [opengame|

   inputs    : nameValuePair  ;
   feedback  :      ;

   :-----------------:
   inputs    : nameValuePair ;
   feedback  :      ;
   operation : forwardFunction $ createOrUpdatePrivateValue name valueSpace;
   outputs   : nameValuePairUpdated1 ;
   returns   :      ;

   inputs    : nameValuePairUpdated1 ;
   feedback  :      ;
   operation : natureEndInput ;
   outputs   : nameValuePairUpdate2 ;
   returns   :      ;

   inputs    : nameValuePairUpdate2 ;
   feedback  :      ;
   operation : forwardFunction embedMaybe ;
   outputs   : nameValuePairFinal ;
   returns   :      ;

   :-----------------:

   outputs   : nameValuePairFinal ;
   returns   :      ;

  |]

 
  
-- Check if game is finished, if so determine payoffs
-- Given the bids, and the proposer's choice, determine outcome 
updateOrTerminateAuction increasePerRound terminationRuleAuction = [opengame|

   inputs    : bidsLsOld,bidLs, state, valuePairs, bids  ;
   feedback  :      ;

   :-----------------:
   inputs    : bidsLsOld,bidLs, state, valuePairs, bids ;
   feedback  :      ;
   operation : forwardFunction $ terminationRuleAuction increasePerRound;
   outputs   : endedOrNextState ;
   returns   :      ;
   :-----------------:

   outputs   : endedOrNextState ;
   returns   :      ;

  |]

-- If the game ended; assign payments
transformPaymentsDynamicAuction paymentRule  = [opengame|

   inputs    : bids, state ;
   feedback  :      ;

   :-----------------:
   inputs    : bids, state ;
   feedback  :      ;
   operation : forwardFunction $ uncurry $ paymentRule ;
   outputs   : paymentsLottery ;
   returns   :      ;

   inputs    : paymentsLottery ;
   feedback  :      ;
   operation : natureEndInput ;
   outputs   : payments ;
   returns   :      ;
   :-----------------:

   outputs   : payments ;
   returns   :      ;
  |]

