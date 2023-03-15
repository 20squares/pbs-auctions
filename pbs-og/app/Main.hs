module Main (main) where

import Auctions.Analytics
import Auctions.Parameterization
import Auctions.Strategies


main :: IO ()
main = do
--  putStrLn "Current Auction: "
--  printEquilibriumCurrentAuction parametersCurrentAuction (currentAuctionShareOfValueStrategy 0.75)
--  putStrLn "First Price Auction: "
--  printEquilibriumSimultaneousBidAuction parametersFPAuction (bidShareOfValueStrategyTuple 0.75)
--  putStrLn "Second Price Auction: "
--  printEquilibriumSimultaneousBidAuction parameters2ndPAuction truthTellingStrategyTuple
--  putStrLn "All pay Auction: "
--  printEquilibriumAllPayAuction parametersAllPayAuction (bidShareOfValueStrategyTuple 0.6)
  putStrLn "~~~~~~~Simulate current auction outcome~~~~~~~"
  putStrLn "List of expected bids: "
  let expectedOutcomeCurrent =  printSimulationCurrentAuction parametersCurrentAuction (currentAuctionShareOfValueStrategy 0.75)
  print expectedOutcomeCurrent
  putStrLn "Expected payment to auctioneer"
  print $ sum expectedOutcomeCurrent
  putStrLn "~~~~~~~Simulate first price auction outcome~~~~~~~"
  putStrLn "List of expected bids: "
  let expectedOutcomeFirstPrice =  printsimulateSimultaneousBidAuction parametersFPAuction (bidShareOfValueStrategyTuple 0.75)
  print expectedOutcomeFirstPrice
  putStrLn "Expected payment to auctioneer"
  print $ sum expectedOutcomeFirstPrice
  putStrLn "~~~~~~~Simulate second price auction outcome~~~~~~~"
  putStrLn "List of expected bids: "
  let expectedOutcomeSecondPrice =  printsimulateSimultaneousBidAuction parameters2ndPAuction truthTellingStrategyTuple
  print expectedOutcomeSecondPrice
  putStrLn "Expected payment to auctioneer"
  print $ sum expectedOutcomeSecondPrice
