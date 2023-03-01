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
biddingStage name actionSpace = [opengame|

    inputs    :  nameValuePair  ;
    feedback  :   ;

    :---------------------------:
    inputs    :  nameValuePair  ;
    feedback  :   ;
    operation :  dependentDecision name (const actionSpace) ;
    outputs   :  bid ;
    returns   :  setPayoff nameValuePair payments  ;
    :---------------------------:

    outputs   :  bid ;
    returns   :  payments  ;
  |]


-- Transforms the bids and the relevant reservePrice into the payments by players
transformPaymentsReservePrice winningPrice  = [opengame|

   inputs    : (bids,reservePrice) ;
   feedback  :      ;

   :-----------------:
   inputs    : (bids,reservePrice) ;
   feedback  :      ;
   operation : forwardFunction (auctionPaymentResPrice paymentReservePrice winningPrice) ;
   outputs   : payments ;
   returns   :      ;
   :-----------------:

   outputs   : payments ;
   returns   :      ;
  |]

-- Transforms the bids into a random reshuffling
transformPayments winningPrice reservePrice = [opengame|

   inputs    : bids ;
   feedback  :      ;

   :-----------------:
   inputs    : (bids, reservePrice) ;
   feedback  :      ;
   operation : forwardFunction (auctionPaymentResPrice paymentReservePrice winningPrice) ;
   outputs   : payments ;
   returns   :      ;
   :-----------------:

   outputs   : payments ;
   returns   :      ;
  |]

-- Transforms the bids into a random reshuffling
transformAllPayPayments  = [opengame|

   inputs    : bids ;
   feedback  :      ;

   :-----------------:
   inputs    : bids ;
   feedback  :      ;
   operation : forwardFunction auctionPaymentAllPay ;
   outputs   : payments ;
   returns   :      ;
   :-----------------:

   outputs   : payments ;
   returns   :      ;
  |]


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
