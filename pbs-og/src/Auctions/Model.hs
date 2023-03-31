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
import Auctions.Types

import OpenGames.Engine.Engine
import OpenGames.Preprocessor


{-
Contains the relevant simultaneous bid auctions needed
-}

--------------------
-- 1 Bidding modules
--------------------

-- | Describes the bidder stage; the number of players, individual evaluations etc. can be changed here w/o affecting the assembled auctions further below
bidders name1 name2  valueSpace1 valueSpace2 actionSpace1 actionSpace2 approxError = [opengame| 
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
   operation :  biddingStage name1 actionSpace1 approxError ;
   outputs   :  name1Dec ;
   returns   :  payments  ;

   inputs    :  name2Value    ;
   feedback  :      ;
   operation :  biddingStage name2 actionSpace2 approxError ;
   outputs   :  name2Dec ;
   returns   :  payments  ;

   :-----------------:

   outputs   :   [(name1,name1Dec),(name2,name2Dec)]   ;
   returns   :   payments   ;
   |]


biddersDynamic name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 approxError = [opengame| 
   inputs    :  bidsOld, state, nameValuePair1, nameValuePair2, nameValuePair3, nameValuePair4, bidOld1, bidOld2, bidOld3, bidOld4   ;
   feedback  :      ;

   :-----------------:
   
   inputs    :  state, bidOld1, nameValuePair1    ;
   feedback  :      ;
   operation :  biddingStageDynamic name1 actionSpace1 approxError ;
   outputs   :  bid1 ;
   returns   :   ;

   inputs    :  state, bidOld2, nameValuePair2   ;
   feedback  :      ;
   operation :  biddingStageDynamic name2 actionSpace2 approxError ;
   outputs   :  bid2 ;
   returns   :  ;

   inputs    :  state, bidOld3, nameValuePair3    ;
   feedback  :      ;
   operation :  biddingStageDynamic name3 actionSpace3 approxError ;
   outputs   :  bid3 ;
   returns   :   ;

   inputs    :  state, bidOld4, nameValuePair4   ;
   feedback  :      ;
   operation :  biddingStageDynamic name4 actionSpace4 approxError ;
   outputs   :  bid4 ;
   returns   :  ;



   :-----------------:

   outputs   :   bidsOld, [(name1,bid1),(name2,bid2),(name3,bid3),(name4,bid4)],state, nameValuePair1, nameValuePair2,nameValuePair3,nameValuePair4, bid1, bid2, bid3, bid4  ;
   returns   :   ;
   |]


  
-----------------------
-- 2 Current Status quo
-----------------------

-----------------------
-- Builders to relay
buildersToRelay name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 approxError= [opengame| 

   inputs    :      ;
   feedback  :      ;

   :-----------------:

   inputs    :  ;
   feedback  :  ;
   operation : bidders name1 name2  valueSpace1 valueSpace2 actionSpace1 actionSpace2 approxError;
   outputs   : bids1 ;
   returns   : payments ;

   inputs    :  ;
   feedback  :  ;
   operation : bidders name3 name4  valueSpace1 valueSpace2 actionSpace1 actionSpace2 approxError;
   outputs   : bids2 ;
   returns   : payments ;

   inputs    : bids1;
   feedback  :      ;
   operation : forwardFunction findMaxBidRelay ;
   outputs   : bidRelayLottery1 ;
   returns   :  ;

   inputs    : bidRelayLottery1 ;
   feedback  :      ;
   operation : natureEndInput ;
   outputs   : bidRelay1 ;
   returns   :      ;

   inputs    : bids2;
   feedback  : ;
   operation : forwardFunction findMaxBidRelay ;
   outputs   : bidRelayLottery2 ;
   returns   : ;

   inputs    : bidRelayLottery2 ;
   feedback  :      ;
   operation : natureEndInput ;
   outputs   : bidRelay2 ;
   returns   :      ;

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
currentAuctionGame nameProposer name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 approxError= [opengame| 

   inputs    : ;
   feedback  : ;

   :-----------------:

   inputs    :  ;
   feedback  :  ;
   operation :  buildersToRelay name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 approxError;
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

   outputs   :   paymentsBidders   ;
   returns   :      ;
   |]


------------------------------
-- 3 Assembled static auctions
------------------------------

-- 3.1: 2 players with exogenous reserve price
-- NOTE this format allows for first price, second price w/o reserve price
reservePriceExogenous name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 winningPrice reservePrice approxError = [opengame| 

   inputs    :      ;
   feedback  :      ;

   :-----------------:
   inputs    :  ;
   feedback  :  ;
   operation : bidders name1 name2  valueSpace1 valueSpace2 actionSpace1 actionSpace2 approxError ;
   outputs   : bids1 ;
   returns   : payments ;

   inputs    :  ;
   feedback  :  ;
   operation : bidders name3 name4  valueSpace1 valueSpace2 actionSpace1 actionSpace2 approxError ;
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
biddingAllPay  name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 approxError = [opengame| 

   inputs    : ;
   feedback  : ;

   :-----------------:
   inputs    :  ;
   feedback  :  ;
   operation : bidders name1 name2  valueSpace1 valueSpace2 actionSpace1 actionSpace2 approxError ;
   outputs   : bids1 ;
   returns   : payments ;

   inputs    :  ;
   feedback  :  ;
   operation : bidders name3 name4  valueSpace1 valueSpace2 actionSpace1 actionSpace2 approxError ;
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


-------------------------------
-- 4 Assembled dynamic auctions
-------------------------------

-- Account for payoffs 
payoffsDynamicAuction  name1 name2 name3 name4  = [opengame| 

   inputs    : payments, nameValuePair1, nameValuePair2,nameValuePair3, nameValuePair4;
   feedback  : ;

   :-----------------:

   inputs    :  nameValuePair1, payments;
   feedback  :  ;
   operation : forwardFunction $ uncurry setPayoff ;
   outputs   : payoff1 ;
   returns   :  ;

   inputs    : payoff1;
   feedback  :  ;
   operation : addPayoffs name1 ;
   outputs   :  ;
   returns   :  ;

   inputs    :  nameValuePair2, payments;
   feedback  :  ;
   operation : forwardFunction $ uncurry setPayoff ;
   outputs   : payoff2 ;
   returns   :  ;

   inputs    : payoff2;
   feedback  :  ;
   operation : addPayoffs name2 ;
   outputs   :  ;
   returns   :  ;

   inputs    :  nameValuePair3, payments;
   feedback  :  ;
   operation : forwardFunction $ uncurry setPayoff ;
   outputs   : payoff3 ;
   returns   :  ;

   inputs    : payoff3;
   feedback  :  ;
   operation : addPayoffs name3 ;
   outputs   :  ;
   returns   :  ;

   inputs    :  nameValuePair4, payments;
   feedback  :  ;
   operation : forwardFunction $ uncurry setPayoff ;
   outputs   : payoff4 ;
   returns   :  ;

   inputs    : payoff4;
   feedback  :  ;
   operation : addPayoffs name4 ;
   outputs   :  ;
   returns   :  ;

   :-----------------:

   outputs   : ;
   returns   : ;
   |]


-- If the auction ends, account for the payoffs
terminalGame  name1 name2 name3 name4 paymentRule = [opengame| 

   inputs    : bids, state , nameValuePair1, nameValuePair2,nameValuePair3, nameValuePair4;
   feedback  : ;

   :-----------------:

   inputs    : bids, state ;
   feedback  :  ;
   operation : transformPaymentsDynamicAuction paymentRule ;
   outputs   : payments ;
   returns   :  ;

   inputs    : payments, nameValuePair1, nameValuePair2,nameValuePair3, nameValuePair4;
   feedback  :  ;
   operation : payoffsDynamicAuction  name1 name2 name3 name4 ;
   outputs   :  ;
   returns   :  ;

  :-----------------:

   outputs   : payments ;
   returns   : ;
   |]



-- Branch into end game or continue
branchingAuction name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 approxError paymentRule = terminalGame  name1 name2 name3 name4 paymentRule +++ biddersDynamic name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 approxError

-- Combine the game into a state game which branches into an endstate or continues
stateGame name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 approxError increasePerRound terminationRuleAuction paymentRule = [opengame| 

   inputs    : bidsOld,bids, state , nameValuePair1, nameValuePair2,nameValuePair3, nameValuePair4, bid1, bid2, bid3, bid4;
   feedback  : ;

   :-----------------:

   inputs    : bidsOld, bids, state, (nameValuePair1, nameValuePair2,nameValuePair3, nameValuePair4), (bid1, bid2, bid3, bid4) ;
   feedback  :  ;
   operation : updateOrTerminateAuction increasePerRound terminationRuleAuction ;
   outputs   : endedOrNextState ;
   returns   :  ;

   inputs    : endedOrNextState;
   feedback  :  ;
   operation : branchingAuction name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 approxError paymentRule ;
   outputs   : continueGame ;
   returns   :  ;

  :-----------------:

   outputs   : continueGame;
   returns   : ;
   |]

-- Markov state game that can get repeated; if the auction ends, we stay in the empty game; otherwise continue the auction
fullStateGame name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 approxError increasePerRound terminationRuleAuction paymentRule =
  [opengame| 

   inputs    : stateOld ;
   feedback  : ;

   :-----------------:

   inputs    : stateOld ;
   feedback  :  ;
   operation : idBranching name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 approxError increasePerRound terminationRuleAuction paymentRule ;
   outputs   : stateUpdated ;
   returns   :  ;

   inputs    : stateUpdated ;
   feedback  :  ;
   operation : forwardFunction unifyEmptyBranching ;
   outputs   : stateNew ;
   returns   :  ;

  :-----------------:

   outputs   : stateNew ;
   returns   : ;
   |]
  where
    -- Unify the output - either stuck in empty game or continue the game
    idBranching name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 approxError increasePerRound terminationRuleAuction paymentRule = idGame +++ stateGame name1 name2 name3 name4 valueSpace1 valueSpace2 valueSpace3 valueSpace4 actionSpace1 actionSpace2 actionSpace3 actionSpace4 approxError increasePerRound terminationRuleAuction paymentRule
    -- Helper to construct an empty game
    idGame :: StochasticStatefulBayesianOpenGame '[] '[] a () a ()
    idGame = fromFunctions id id

