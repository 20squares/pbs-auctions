{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}


module Auctions.NestedAuctions
  where


import Auctions.AuctionSupportFunctions
import Auctions.Components
import Auctions.Model

import OpenGames.Engine.Engine
import OpenGames.Preprocessor


{-
This is a sketch of several nested first price auctions. This is not complete and will be explored somewhere else in the future. But it is probably still interesting to have here as an idea. 
-}



-- Auxiliary component
-- NOTE this format allows for first price, second price w/o reserve price
reservePriceExogenousExternalValues name1 name2 valueSpace1 valueSpace2 actionSpace1 actionSpace2  winningPrice reservePrice approxError = [opengame| 

   inputs    :    name1Value, name2Value  ;
   feedback  :      ;

   :-----------------:

   inputs    :  name1Value    ;
   feedback  :      ;
   operation :  biddingStage name1 actionSpace1 approxError;
   outputs   :  name1Dec ;
   returns   :  payments  ;

   inputs    :  name2Value    ;
   feedback  :      ;
   operation :  biddingStage name2 actionSpace2 approxError;
   outputs   :  name2Dec ;
   returns   :  payments  ;

   inputs    :  [(name1,name1Dec),(name2,name2Dec)]  ;
   feedback  :      ;
   operation :   transformPayments winningPrice reservePrice ;
   outputs   :  payments ;
   returns   :      ;
   :-----------------:

   outputs   :  payments    ;
   returns   :      ;
   |]

reservePriceExogenous2Bidders name1 name2 valueSpace1 valueSpace2 actionSpace1 actionSpace2  winningPrice reservePrice approxError= [opengame| 

   inputs    :      ;
   feedback  :      ;

   :-----------------:
   inputs    :  ;
   feedback  :  ;
   operation : bidders name1 name2  valueSpace1 valueSpace2 actionSpace1 actionSpace2 approxError;
   outputs   : bids ;
   returns   : payments ;

   inputs    : bids;
   feedback  : ;
   operation : transformPayments winningPrice reservePrice ;
   outputs   : payments ;
   returns   : ;
   :-----------------:

   outputs   : payments   ;
   returns   :     ;
   |]



-----------------------
-- Nested Auction model
-- NOTE in this model, we assume the timing is such that lower ranked auctions take place first
-- In this case the bidders in the last auction have already received the bids from lower ranked auctions
nestedFirstPriceAuctions auctioneer1 auctioneer2 user1 user2 user3 user4 winningPrice reservePrice valueSpace1 valueSpace2 actionSpace1 actionSpace2  approxError= [opengame| 

   inputs    :      ;
   feedback  :      ;

   :-----------------:
   inputs    :      ;
   feedback  :      ;
   operation : reservePriceExogenous2Bidders user1 user2 valueSpace1 valueSpace2 actionSpace1 actionSpace2  winningPrice reservePrice approxError;
   outputs   : results1 ;
   returns   :      ;

   inputs    :      ;
   feedback  :      ;
   operation : reservePriceExogenous2Bidders user3 user4  valueSpace1 valueSpace2 actionSpace1 actionSpace2 winningPrice reservePrice approxError;
   outputs   : results2 ;
   returns   :      ;

   inputs    :  results1    ;
   feedback  :      ;
   operation :  transformAuctionResultNewParticipant auctioneer1;
   outputs   :  newValuePair1 ;
   returns   :    ;

   inputs    :  results2    ;
   feedback  :      ;
   operation :  transformAuctionResultNewParticipant auctioneer2;
   outputs   :  newValuePair2 ;
   returns   :    ;

   inputs    :  newValuePair1, newValuePair2  ;
   feedback  :      ;
   operation :  reservePriceExogenousExternalValues auctioneer1 auctioneer2  valueSpace1 valueSpace2 actionSpace1 actionSpace2 winningPrice reservePrice approxError;
   outputs   :  results3 ;
   returns   :      ;
   :-----------------:

   outputs   :      ;
   returns   :      ;
   |]

-- Nested Auction model 2
-- NOTE in this model, we assume the timing is such that the highest ranked auction takes place first
-- In this case the bidders do not have yet received actual bids from their lower ranked bidders
-- When they bid, they have to bid based on expectations; their profit is the residual.
-- NOTE initial willingness to participate are given exogenously
-- TODO Do we want to use a branching game or do both auctions run in parallel anyways?
-- FIXME the payoffs of the auction players need to be made contingent on the overall outcome
nestedFirstPriceAuctionsReverse auctioneer1 auctioneer2 user1 user2 user3 user4  valueSpace1 valueSpace2 actionSpace1 actionSpace2 newValuePair1 newValuePair2 winningPrice reservePrice approxError= [opengame| 

   inputs    :    name1Value, name2Value  ;
   feedback  :      ;

   :-----------------:
   inputs    :   name1Value, name2Value ;
   feedback  :      ;
   operation :  reservePriceExogenousExternalValues auctioneer1 auctioneer2 valueSpace1 valueSpace2 actionSpace1 actionSpace2 winningPrice reservePrice approxError;
   outputs   :  results3 ;
   returns   :      ;

   inputs    :  results3   ;
   feedback  :      ;
   operation :  transformAuctionOutcome auctioneer1;
   outputs   :  costsAuction1 ;
   returns   :    ;

   inputs    :  results3    ;
   feedback  :      ;
   operation :  transformAuctionOutcome auctioneer2;
   outputs   :  costsAuction2 ;
   returns   :    ;

   inputs    :      ;
   feedback  :      ;
   operation : reservePriceExogenous2Bidders user1 user2 valueSpace1 valueSpace2 actionSpace1 actionSpace2 winningPrice reservePrice approxError;
   outputs   : results1 ;
   returns   :      ;

   inputs    :      ;
   feedback  :      ;
   operation : reservePriceExogenous2Bidders user3 user4 valueSpace1 valueSpace2 actionSpace1 actionSpace2 winningPrice reservePrice approxError;
   outputs   : results2 ;
   returns   :      ;

      :-----------------:

   outputs   :      ;
   returns   :      ;
   |]
