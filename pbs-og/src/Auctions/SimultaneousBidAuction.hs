{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}


module Auctions.SimultaneousBidAuction
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
-- 2 Assembled auctions
-----------------------

-- 2.1: 2 players with exogenous reserve price
-- NOTE this format allows for first price, second price w/o reserve price
reservePriceExogenous name1 name2 winningPrice reservePrice valueSpace1 valueSpace2 actionSpace1 actionSpace2  = [opengame| 

   inputs    :      ;
   feedback  :      ;

   :-----------------:
   inputs    :  ;
   feedback  :  ;
   operation : bidders name1 name2  valueSpace1 valueSpace2 actionSpace1 actionSpace2 ;
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

-- 2.1:  allpay auction with 2 players
biddingAllPay  name1 name2 valueSpace1 valueSpace2 actionSpace1 actionSpace2  = [opengame| 

   inputs    : ;
   feedback  : ;

   :-----------------:
   inputs    : ;
   feedback  : ;
   operation : bidders name1 name2  valueSpace1 valueSpace2 actionSpace1 actionSpace2 ;
   outputs   : bids ;
   returns   : payments ;

   inputs    : bids;
   feedback  : ;
   operation : transformAllPayPayments ;
   outputs   : payments ;
   returns   : ;
   :-----------------:

   outputs   : payments ;
   returns   : ;
   |]

