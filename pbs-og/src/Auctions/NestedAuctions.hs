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
import Auctions.SimultaneousBidAuction

import OpenGames.Engine.Engine
import OpenGames.Preprocessor


{-

Describes several nested auctions

-}

----------
-- A Model
---------- 
nestedFirstPriceAuctions auctioneer1 auctioneer2 user1 user2 user3 user4 winningPrice reservePrice valueSpace1 valueSpace2 actionSpace1 actionSpace2  = [opengame| 

   inputs    :      ;
   feedback  :      ;

   :-----------------:
   inputs    :      ;
   feedback  :      ;
   operation : bidding2ReservePriceExogenous user1 user2 winningPrice reservePrice valueSpace1 valueSpace2 actionSpace1 actionSpace2 ;
   outputs   : results1 ;
   returns   :      ;

   inputs    :      ;
   feedback  :      ;
   operation : bidding2ReservePriceExogenous user3 user4 winningPrice reservePrice valueSpace1 valueSpace2 actionSpace1 actionSpace2 ;
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
   operation :  bidding2ReservePriceExogenousExternalValues auctioneer1 auctioneer2 winningPrice reservePrice valueSpace1 valueSpace2 actionSpace1 actionSpace2 ;
   outputs   :  results3 ;
   returns   :      ;
   :-----------------:

   outputs   :      ;
   returns   :      ;
   |]

