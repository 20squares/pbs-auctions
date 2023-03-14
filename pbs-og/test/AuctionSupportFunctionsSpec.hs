module  AuctionSupportFunctionsSpec where

import Auctions.AuctionSupportFunctions
import Auctions.Types

import OpenGames.Engine.Engine (playDeterministically,Stochastic, distFromList)


import Test.Hspec
import Numeric.Probability.Distribution (decons)

spec :: Spec
spec = do
  selectPayoffsTest
  setPayoffsTest
  findMaxBidRelayTest
  computeOutcomeFunctionTest
  auctionWinnerReservePriceTest
  auctionPaymentAllPayTest
  auctionPaymentResPriceTest

-- test variables
-- Outcomes: From _testbids1_ all pay auction
testOutcomes1 = [("bidder1",20,True),("bidder2",10,False),("bidder3",7,False)]

-- Outcomes: From a sealed bid auction
testOutcomes2 = [("bidder1",10,True),("bidder2",0,False),("bidder3",0,False)]

-- Outcomes: From _testBids1_ below; fp
testOutcomes3 = [("bidder1",20,True),("bidder2",0,False),("bidder3",0,False)]

-- Outcomes: From _testBids1_ below; fp
testOutcomes4 = [("bidder1",10,True),("bidder2",0,False),("bidder3",0,False)]

-- Outcomes: From _testBids2_ belowl; 
testOutcomes5 :: Stochastic [AuctionOutcome]
testOutcomes5 = distFromList  [([("bidder1", 20.0, True), ("bidder2", 10.0, False), ("bidder3", 7.0, False), ("bidder4", 20.0, False)], 0.5), ([("bidder1", 20.0, False), ("bidder2", 10.0, False), ("bidder3", 7.0, False), ("bidder4", 20.0, True)], 0.5)]

-- Outcomes: From _testBids2_ belowl; 2nd price
testOutcomes6 :: Stochastic [AuctionOutcome]
testOutcomes6 = distFromList  [([("bidder1", 20.0, True), ("bidder2", 0, False), ("bidder3", 0, False), ("bidder4", 0, False)], 0.5), ([("bidder1", 0, False), ("bidder2", 0, False), ("bidder3", 0, False), ("bidder4", 20.0, True)], 0.5)]

-- Bids
testBids1 = [("bidder1",20),("bidder2",10),("bidder3",7)]

-- Bids
testBids2 = [("bidder1",20),("bidder2",10),("bidder3",7),("bidder4",20)]

-- Tests
selectPayoffsTest = describe
   "select payoffs" $ do
     it "correct name - 1" $ do
       shouldBe
         (selectPayoffs "bidder1" testOutcomes1)
         (20,True)
     it "correct name - 2" $ do
       shouldBe
         (selectPayoffs "bidder2" testOutcomes1)
         (10,False)
     it "incorrect name" $ do
       shouldBe
         (selectPayoffs "nonbidder" testOutcomes1)
         (0,False)

setPayoffsTest = describe
   "set payoffs" $ do
     it "winner correct - 1" $ do
       shouldBe
         (setPayoff ("bidder1",11) testOutcomes2)
         1
     it "winner correct - 2"  $ do
       shouldBe
         (setPayoff ("bidder1",15) testOutcomes2)
         5
     it "winner correct - 2"  $ do
       shouldBe
         (setPayoff ("bidder2",5) testOutcomes2)
         0

findMaxBidRelayTest = describe
   "find maximum bid relay" $ do
     it "correct bid - 1" $ do
       shouldBe
         (findMaxBidRelay testBids1)
         ("bidder1",20)

computeOutcomeFunctionTest = describe
   "compute outcomes" $ do
     it "correct outcomes - 1" $ do
       shouldBe
         (computeOutcomeFunction ("bidder1",20) testBids1)
         testOutcomes3

auctionWinnerReservePriceTest = describe
   "compute winning bids" $ do
     it "the winning bid is extracted correctly" $ do
       shouldBe
         (extractWinningBid testBids1)
         ("bidder1",20)
     it "correct winning bids - 1" $ do
       shouldBe
         (decons $ auctionWinner testBids1)
         (decons $ playDeterministically testOutcomes1)
     it "correct bids in case of several highest bids" $ do
       shouldBe
         (decons $ auctionWinner testBids2)
         (decons $ testOutcomes5)

auctionPaymentAllPayTest = describe
   "compute outcomes for all pay auction" $ do
     it "correct outcomes - 1" $ do
       shouldBe
         (decons $ auctionPaymentAllPay testBids1)
         (decons $ playDeterministically testOutcomes1)

auctionPaymentResPriceTest = describe
   "compute outcomes for simultaneous bid auction with reserve price" $ do
     it "correct outcomes - fp" $ do
       shouldBe
         (decons $ auctionPaymentResPrice paymentReservePrice 1 (testBids1,0))
         (decons $ playDeterministically testOutcomes3)
     it "correct outcomes - 2ndp" $ do
       shouldBe
         (decons $ auctionPaymentResPrice paymentReservePrice 2 (testBids1,0))
         (decons $ playDeterministically testOutcomes4)
     it "correct outcomes - 2ndp - with several highest bids" $ do
       shouldBe
         (decons $ auctionPaymentResPrice paymentReservePrice 2 (testBids2,0))
         (decons $ testOutcomes6)
