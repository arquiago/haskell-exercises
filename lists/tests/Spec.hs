module Main where

import Part1
import Part2
import Test.Hspec

main :: IO ()
main = hspec $ do
  part1TestsGroup1
  part1TestsGroup2
  part2TestsGroup1
  part2TestsGroup2
  part2TestsGroup3

part1TestsGroup1 = do
  describe "Parte-1-1/2" $ do
    memberTests
    countTests
    forallTests
    existsTests

part1TestsGroup2 = do
  describe "Parte-1-2/2" $ do
    firstTests
    singleTests
    mostlyTests
    mostlyTrueTests

part2TestsGroup1 = do
  describe "Parte-2-1/3" $ do
    majorityTests
    collatz1Tests
    collatzTests

part2TestsGroup2 = do
  describe "Parte-2-2/3" $ do
    isFixpointTests
    findFixpointTests
    testCollatzConjectureTests

part2TestsGroup3 = do
  describe "Parte-2-3/3" $ do
    tooBigTests
    nearlyEqualTests
    sequenceTests
    seriesTests

--computationTests
