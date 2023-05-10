module Main (main) where

import Auctions.Analytics
import Auctions.Parameterization
import Auctions.Strategies

onlyEquilibria = do
  putStrLn "~~~~~~~~EQUILIBRIUM CHECKING~~~~~~~~"
  putStrLn "~~Mainly as a form of sanity check~~"
  putStrLn "~~~~~~~~~~Current Auction~~~~~~~~~~~"
  printEquilibriumCurrentAuction parametersCurrentAuction (currentAuctionShareOfValueStrategy 0.75)
  putStrLn "~~~~~~~~First Price Auction~~~~~~~~~"
  printEquilibriumSimultaneousBidAuction parametersFPAuction (bidShareOfValueStrategyTuple 0.75)
  putStrLn "~~~~~~~~Second Price Auction~~~~~~~~"
  printEquilibriumSimultaneousBidAuction parameters2ndPAuction truthTellingStrategyTuple
  putStrLn "~~~~~~~~~~All pay Auction~~~~~~~~~~~"
  printEquilibriumAllPayAuction parametersAllPayAuction allPayAuctionStrategyTuple
  putStrLn "~~~~~~~~~~Japanese Auction~~~~~~~~~~"
  printEquilibriumDynamicAuction parametersJapaneseAuction 10 japaneseAuctionStrategyTuple initialAction
  putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

onlySimulations = do
  putStrLn "~~~~~~~~~~~~SIMULATIONS~~~~~~~~~~~~~"
  putStrLn "~~~~~~~~~~Current Auction~~~~~~~~~~~"
  putStrLn "List of expected bids: "
  let expectedOutcomeCurrent =  printSimulationCurrentAuction parametersCurrentAuction (currentAuctionShareOfValueStrategy 0.75)
  print expectedOutcomeCurrent
  putStrLn "Expected payment to auctioneer"
  print $ sum expectedOutcomeCurrent
  putStrLn "~~~~~~~~First Price Auction~~~~~~~~~"
  putStrLn "List of expected bids: "
  let expectedOutcomeFirstPrice =  printSimulationSimultaneousBidAuction parametersFPAuction (bidShareOfValueStrategyTuple 1)
  print expectedOutcomeFirstPrice
  putStrLn "Expected payment to auctioneer"
  print $ sum expectedOutcomeFirstPrice
  putStrLn "~~~~~~~~Second Price Auction~~~~~~~~"
  putStrLn "List of expected bids: "
  let expectedOutcomeSecondPrice =  printSimulationSimultaneousBidAuction parameters2ndPAuction truthTellingStrategyTuple
  print expectedOutcomeSecondPrice
  putStrLn "Expected payment to auctioneer"
  print $ sum expectedOutcomeSecondPrice
  putStrLn "~~~~~~~~~~All pay Auction~~~~~~~~~~~"
  putStrLn "List of expected bids: "
  let expectedOutcomeAllPay =  printSimulationAllPayAuction parametersAllPayAuction allPayAuctionStrategyTuple
  print expectedOutcomeAllPay
  putStrLn "Expected payment to auctioneer"
  print $ sum expectedOutcomeAllPay
  putStrLn "~~~~~~~~~~Japanese Auction~~~~~~~~~~"
  printSimulationRepeatedStageGame parametersJapaneseAuction 10 japaneseAuctionStrategyTuple initialAction
  putStrLn "~~~~~~~Show Full Output Japanese Auction~~~~~~~"
  printOutputDynamicAuction parametersJapaneseAuction 10 japaneseAuctionStrategyTuple initialAction


main :: IO ()
main = do
  onlyEquilibria
  onlySimulations
