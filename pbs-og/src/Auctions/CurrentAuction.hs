{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Auctions.CurrentAuction
  where

import Auctions.AuctionSupportFunctions
import Auctions.Components
import Auctions.SimultaneousBidAuction
import Auctions.Types

import OpenGames.Engine.Engine
import OpenGames.Preprocessor

import Data.List (maximumBy)
import Data.Ord (comparing)



{-
Describes the current current market setup as provided in the documentation. Concretely,
* A validator is proposing a block for the current slot is selling that blockspace to participants in the builder market
* A validator can have access to several relays
* Relays put forward the block by the builder with the highest bid
* Note, that for now we treat the individual builders as the "smallest" unit being active.
* In the future, this can be extended by explicitly modelling the tx inclusion decision by builders
-}

--------------------------
-- Auxiliary functionality
--------------------------

-- Aggregate a pair of bids
-- TODO extend to more general bids
aggregateBids :: (Bid,Bid) -> Relay
aggregateBids (b1,b2) =  [b1,b2]

-- Find max bid in relay
findMaxBidRelay :: Relay -> Bid
findMaxBidRelay ls = maximumBy (comparing snd) ls

-- Extract payment for proposer
extractProposerPayment :: Bid -> BidValue
extractProposerPayment (name,value) = value

-----------------------
-- Builders to relay
buildersToRelay valueSpace1 valueSpace2 actionSpace1 actionSpace2  = [opengame| 

   inputs    :      ;
   feedback  :      ;

   :-----------------:

   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage "builder1" valueSpace1;
   outputs   : nameValuePair1 ;
   returns   :      ;

   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage "builder2" valueSpace2;
   outputs   : nameValuePair2 ;
   returns   :      ;

   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage "builder3" valueSpace1;
   outputs   : nameValuePair3 ;
   returns   :      ;

   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage "builder4" valueSpace2;
   outputs   : nameValuePair4 ;
   returns   :      ;

   inputs    :  nameValuePair1, nameValuePair2    ;
   feedback  :      ;
   operation :  forwardFunction aggregateBids ;
   outputs   :  bids1 ;
   returns   :    ;
   // Aggregate the two first builders into a relay

   inputs    :  nameValuePair3, nameValuePair4    ;
   feedback  :      ;
   operation :  forwardFunction aggregateBids ;
   outputs   :  bids2 ;
   returns   :    ;
   // Aggregate the two last builders into a relay

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
   outputs   :  bids ;
   returns   :   ;
   // Aggregate the two winning bids from a relay into list of available blocks
   // for the propser to choose from
   :-----------------:

   outputs   : bids ;
   returns   :      ;
   |]

---------------------
-- Validator decision
validatorsDecision   = [opengame| 

   inputs    : bids ;
   feedback  : ;

   :-----------------:

   inputs    :  bids ;
   feedback  :  ;
   operation :  dependentDecision "Proposer" id ;
   outputs   :  winningBid ;
   returns   :  returnProposer ;

   inputs    :  winningBid    ;
   feedback  :  ;
   operation :  forwardFunction extractProposerPayment  ;
   outputs   :  returnProposer ;
   returns   :    ;

   :-----------------:

   outputs   :      ;
   returns   :      ;
   |]

---------------------
-- Assemble full game
completeGame  valueSpace1 valueSpace2 actionSpace1 actionSpace2 = [opengame| 

   inputs    : ;
   feedback  : ;

   :-----------------:

   inputs    :  ;
   feedback  :  ;
   operation :  buildersToRelay valueSpace1 valueSpace2 actionSpace1 actionSpace2 ;
   outputs   :  bids ;
   returns   :  ;

   inputs    :  bids ;
   feedback  :  ;
   operation :  validatorsDecision  ;
   outputs   :   ; 
   returns   :   ;

   :-----------------:

   outputs   :      ;
   returns   :      ;
   |]

