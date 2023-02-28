{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}


module Auctions.SimultaneousBidAuction
  ( bidding2ReservePrice
  , bidding2
  , bidding2ExposeWinningBid
  , truthfulStrat
  , values)
  where


import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import Auctions.AuctionSupportFunctions

----------
-- A Model
----------

---------------
-- 0 Parameters

type Values = Double 

values :: [Values]
values = [0,20..100]

reservePriceParameter :: Double
reservePriceParameter = 1

---------------------
-- 1 The actual games

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


-- Transforms the payments into a random reshuffling
transformPaymentsReservePrice kPrice kSlots = [opengame|

   inputs    : (bids,reservePrice) ;
   feedback  :      ;

   :-----------------:
   inputs    : (bids,reservePrice) ;
   feedback  :      ;
   operation : forwardFunction (auctionPaymentResPrice noLotteryPayment kPrice kSlots 0) ;
   outputs   : payments ;
   returns   :      ;
   :-----------------:

   outputs   : payments ;
   returns   :      ;
  |]



bidding2ReservePrice kPrice kSlots  valueSpace1 valueSpace2 actionSpace1 actionSpace2 = [opengame| 

   inputs    : reservePrice    ;
   feedback  :      ;

   :-----------------:
   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage "Alice" valueSpace1 ;
   outputs   :  aliceValue ;
   returns   :      ;

   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage "Bob" valueSpace2;
   outputs   :  bobValue ;
   returns   :      ;

   inputs    :  aliceValue    ;
   feedback  :      ;
   operation :  biddingStage "Alice" actionSpace1 ;
   outputs   :  aliceDec ;
   returns   :  payments  ;

   inputs    :  bobValue    ;
   feedback  :      ;
   operation :  biddingStage "Bob" actionSpace2 ;
   outputs   :  bobDec ;
   returns   :  payments  ;

   inputs    :  ([("Alice",aliceDec),("Bob",bobDec)],reservePrice)  ;
   feedback  :      ;
   operation :   transformPaymentsReservePrice kPrice kSlots ;
   outputs   :  payments ;
   returns   :      ;
   :-----------------:

   outputs   :  payments    ;
   returns   :      ;
   |]


---- Without reserve price
-- Transforms the payments into a random reshuffling
transformPayments kPrice kSlots reservePrice = [opengame|

   inputs    : bids ;
   feedback  :      ;

   :-----------------:
   inputs    : bids ;
   feedback  :      ;
   operation : forwardFunction (auctionPayment noLotteryPayment reservePrice kPrice kSlots 0) ;
   outputs   : payments ;
   returns   :      ;
   :-----------------:

   outputs   : payments ;
   returns   :      ;
  |]


-- Instantiates a simplified version with two players
bidding2 kPrice kSlots reservePrice valueSpace1 valueSpace2 actionSpace1 actionSpace2  = [opengame| 

   inputs    :      ;
   feedback  :      ;

   :-----------------:
   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage "Alice" valueSpace1 ;
   outputs   :  aliceValue ;
   returns   :      ;

   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage "Bob" valueSpace2 ;
   outputs   :  bobValue ;
   returns   :      ;

   inputs    :  aliceValue    ;
   feedback  :      ;
   operation :  biddingStage "Alice" actionSpace1 ;
   outputs   :  aliceDec ;
   returns   :  payments  ;

   inputs    :  bobValue    ;
   feedback  :      ;
   operation :  biddingStage "Bob" actionSpace2 ;
   outputs   :  bobDec ;
   returns   :  payments  ;

   inputs    :  [("Alice",aliceDec),("Bob",bobDec)]  ;
   feedback  :      ;
   operation :   transformPayments kPrice kSlots reservePrice ;
   outputs   :  payments ;
   returns   :      ;
   :-----------------:

   outputs   :      ;
   returns   :      ;
   |]


 -- Instantiates a simplified version with two players and expose winning bid
bidding2ExposeWinningBid name1 name2 kPrice kSlots reservePrice valueSpace1 valueSpace2 actionSpace1 actionSpace2  = [opengame|

   inputs    :      ;
   feedback  :      ;

   :-----------------:
   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage name1 valueSpace1 ;
   outputs   :  aliceValue ;
   returns   :      ;

   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage name2 valueSpace2 ;
   outputs   :  bobValue ;
   returns   :      ;

   inputs    :  aliceValue    ;
   feedback  :      ;
   operation :  biddingStage name1 actionSpace1 ;
   outputs   :  aliceDec ;
   returns   :  payments  ;

   inputs    :  bobValue    ;
   feedback  :      ;
   operation :  biddingStage name2 actionSpace2 ;
   outputs   :  bobDec ;
   returns   :  payments  ;

   inputs    :  [(name1,aliceDec),(name2,bobDec)]  ;
   feedback  :      ;
   operation :   transformPayments kPrice kSlots reservePrice ;
   outputs   :  payments ;
   returns   :      ;

   inputs    :  payments  ;
   feedback  :      ;
   operation : forwardFunction $ extractWinningBid ;
   outputs   : (winner,price) ;
   returns   :      ;

   :-----------------:

   outputs   : (winner,price)  ;
   returns   :      ;
   |]

 

  
-- B Analysis
-------------

---------------
-- 0 Strategies

-- Truthful bidding
stratBidderTruth :: Kleisli Stochastic (String, Values) Values
stratBidderTruth  = Kleisli (\(_,x) -> playDeterministically x)

-- Constant bidding
constBidding :: Values -> Kleisli Stochastic (String,Values) Values
constBidding x = Kleisli (\(_,_) -> playDeterministically x)

-- Complete strategy for truthful bidding for 2 players
truthfulStrat ::
  List
    '[Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values]
truthfulStrat =
  stratBidderTruth
  ::- stratBidderTruth
  ::- Nil

-- Complete strategy for const bidding for 2 players
constBiddingStrat x y =
  constBidding x
  ::- constBidding y
  ::- Nil

---------------
-- 1 Equilibria
-- 1.0 Eq. game with 3 players
equilibriumGame kPrice kSlots reservePrice valueSpace1 valueSpace2 actionSpace1 actionSpace2 strat = evaluate (bidding2 kPrice kSlots reservePrice valueSpace1 valueSpace2 actionSpace1 actionSpace2) strat void


------------------------
-- 2 Interactive session

-- One object being auctioned off Once we exclude slots via lottery, and just auction off one slot, truthful bidding becomes an equilibrium
-- generateIsEq $ equilibriumGame 2 1 reservePriceParameter values values values values truthfulStrat

-- Not an equilibrium
-- generateIsEq $ equilibriumGame 2 1 reservePriceParameter values values values values (constBiddingStrat 30 30)

