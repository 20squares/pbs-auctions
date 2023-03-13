{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}


module Auctions.Model
  where


import Auctions.AuctionSupportFunctions
import Auctions.Components

import OpenGames.Engine.Engine
import OpenGames.Preprocessor


{-
Contains the relevant simultaneous bid auctions needed
-}

-------------------
-- 1 Bidding module
-------------------

-- | Describes the bidder stage; the number of players, individual evaluations etc. can be changed here w/o affecting the assembled auctions further below
bidders name1 name2  valueSpace1 valueSpace2 actionSpace1 actionSpace2  = [opengame| 
   inputs    :      ;
   feedback  :      ;

   :-----------------:
   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage name1 valueSpace1 ;
   outputs   :  name1Value ;
   returns   :      ;

   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage name2 valueSpace2 ;
   outputs   :  name2Value ;
   returns   :      ;

   inputs    :  name1Value    ;
   feedback  :      ;
   operation :  biddingStage name1 actionSpace1 ;
   outputs   :  name1Dec ;
   returns   :  payments  ;

   inputs    :  name2Value    ;
   feedback  :      ;
   operation :  biddingStage name2 actionSpace2 ;
   outputs   :  name2Dec ;
   returns   :  payments  ;

   :-----------------:

   outputs   :   [(name1,name1Dec),(name2,name2Dec)]   ;
   returns   :   payments   ;
   |]


-----------------------
-- 2 Current Status quo
-----------------------


-----------------------
-- Builders to relay
buildersToRelay name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4  = [opengame| 

   inputs    :      ;
   feedback  :      ;

   :-----------------:

   inputs    :  ;
   feedback  :  ;
   operation : bidders name1 name2  valueSpace1 valueSpace2 actionSpace1 actionSpace2 ;
   outputs   : bids1 ;
   returns   : payments ;

   inputs    :  ;
   feedback  :  ;
   operation : bidders name3 name4  valueSpace1 valueSpace2 actionSpace1 actionSpace2 ;
   outputs   : bids2 ;
   returns   : payments ;

   inputs    :  bids1;
   feedback  :      ;
   operation :  forwardFunction findMaxBidRelay ;
   outputs   :  bidRelay1 ;
   returns   :   ;

   inputs    :  bids2;
   feedback  :      ;
   operation :  forwardFunction findMaxBidRelay ;
   outputs   :  bidRelay2 ;
   returns   :   ;

   inputs    :  bidRelay1, bidRelay2;
   feedback  :      ;
   operation :  forwardFunction aggregateBids ;
   outputs   :  bidsFromRelay ;
   returns   :   ;
   // Aggregate the two winning bids from a relay into list of available blocks
   // for the propser to choose from

   :-----------------:

   outputs   : bidsFromRelay ;
   returns   : payments;
|]

validatorsDecision nameProposer  = [opengame| 

   inputs    : bids ;
   feedback  : ;

   :-----------------:

   inputs    :  bids ;
   feedback  :  ;
   operation :  dependentDecision nameProposer id;
   outputs   :  winningBid ;
   returns   :  returnProposer ;

   inputs    :  winningBid    ;
   feedback  :  ;
   operation :  forwardFunction extractProposerPayment  ;
   outputs   :  returnProposer ;
   returns   :    ;

   :-----------------:

   outputs   :  winningBid    ;
   returns   :      ;
   |]


---------------------
-- Assemble full game
currentAuctionGame nameProposer name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 = [opengame| 

   inputs    : ;
   feedback  : ;

   :-----------------:

   inputs    :  ;
   feedback  :  ;
   operation :  buildersToRelay name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 ;
   outputs   :  bids ;
   returns   :  paymentsBidders;

   inputs    :  bids ;
   feedback  :  ;
   operation :  validatorsDecision nameProposer ;
   outputs   :  winningBid ; 
   returns   :   ;

   inputs    :  winningBid,bids ;
   feedback  :  ;
   operation :  computeOutcomes ;
   outputs   :  paymentsBidders ;
   returns   :   ;


   :-----------------:

   outputs   :      ;
   returns   :      ;
   |]


-----------------------
-- 3 Assembled auctions
-----------------------

-- 3.1: 2 players with exogenous reserve price
-- NOTE this format allows for first price, second price w/o reserve price
reservePriceExogenous name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 winningPrice reservePrice = [opengame| 

   inputs    :      ;
   feedback  :      ;

   :-----------------:
   inputs    :  ;
   feedback  :  ;
   operation : bidders name1 name2  valueSpace1 valueSpace2 actionSpace1 actionSpace2 ;
   outputs   : bids1 ;
   returns   : payments ;

   inputs    :  ;
   feedback  :  ;
   operation : bidders name3 name4  valueSpace1 valueSpace2 actionSpace1 actionSpace2 ;
   outputs   : bids2 ;
   returns   : payments ;

   inputs    : bids1, bids2 ;
   feedback  :  ;
   operation : forwardFunction aggregateBidsLS ;
   outputs   : bids ;
   returns   : ;

   inputs    : bids;
   feedback  : ;
   operation : transformPayments winningPrice reservePrice ;
   outputs   : payments ;
   returns   : ;
   :-----------------:

   outputs   : payments   ;
   returns   :     ;
   |]

-- 3.1:  allpay auction with 2 players
biddingAllPay  name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4  = [opengame| 

   inputs    : ;
   feedback  : ;

   :-----------------:
   inputs    :  ;
   feedback  :  ;
   operation : bidders name1 name2  valueSpace1 valueSpace2 actionSpace1 actionSpace2 ;
   outputs   : bids1 ;
   returns   : payments ;

   inputs    :  ;
   feedback  :  ;
   operation : bidders name3 name4  valueSpace1 valueSpace2 actionSpace1 actionSpace2 ;
   outputs   : bids2 ;
   returns   : payments ;

   inputs    : bids1, bids2 ;
   feedback  :  ;
   operation : forwardFunction aggregateBidsLS ;
   outputs   : bids ;
   returns   : ;

   inputs    : bids;
   feedback  : ;
   operation : transformAllPayPayments ;
   outputs   : payments ;
   returns   : ;
   :-----------------:

   outputs   : payments ;
   returns   : ;
   |]

