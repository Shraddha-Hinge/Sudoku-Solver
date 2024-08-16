module Main where

import Test.Hspec
import Sudoku (isValid, solve, initialBoard)

main :: IO ()
main = hspec $ do
  describe "Sudoku" $ do
    it "checks if the initial board is valid" $ do
      isValid initialBoard `shouldBe` True

    it "solves the initial board" $ do
      solve initialBoard `shouldSatisfy` isJust
