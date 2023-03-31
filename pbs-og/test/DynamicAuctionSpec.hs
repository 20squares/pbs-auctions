module  DynamicAuctionSpec where

import Auctions.AuctionSupportFunctions
import Auctions.Types

import OpenGames.Engine.Engine (playDeterministically,Stochastic, distFromList)


import Numeric.Probability.Distribution (decons)
import Test.Hspec


{-
Simple tests to test basic assumptions regarding the dynamic auction functionality
-}


spec :: Spec
spec = do
  terminationRuleJapaneseAuctionTest

 -- test variables
-- test bids
testBids1 = [("bidder1",True),("bidder2",False), ("bidder3",False),("bidder4",False)]
testBids2 = [("bidder1",False),("bidder2",False), ("bidder3",False),("bidder4",False)]


-- private values
privateValues = (("bidder1",10),("bidder2",9), ("bidder3",8),("bidder4",7))

-- Last bid behavior in t-1
lastBid = (True,False,False,False)
  
testInitialCondition1 = (testBids1,10,privateValues,lastBid)
testInitialCondition2 = (testBids2,10,privateValues,lastBid)

-- Tests
terminationRuleJapaneseAuctionTest = describe
   "Continue auction correctly" $ do
     it "correctly continued" $ do
       shouldBe
         (terminationRuleJapaneseAuction 1 testInitialCondition1)
         (Right (11,("bidder1",10),("bidder2",9), ("bidder3",8),("bidder4",7), True,False,False,False))
     it "correctly stops" $ do
       shouldBe
         (terminationRuleJapaneseAuction 1 testInitialCondition2)
         (Left (testBids2,10,("bidder1",10),("bidder2",9), ("bidder3",8),("bidder4",7)))

japaneseAuctionPaymentsTest = describe
   "Stop auction" $ do
     it "correctly continued" $ do
       shouldBe
         (terminationRuleJapaneseAuction 1 testInitialCondition1)
         (Right (11,("bidder1",10),("bidder2",9), ("bidder3",8),("bidder4",7), True,False,False,False))
     it "correctly stops" $ do
       shouldBe
         (terminationRuleJapaneseAuction 1 testInitialCondition2)
         (Left (testBids2,10,("bidder1",10),("bidder2",9), ("bidder3",8),("bidder4",7)))
