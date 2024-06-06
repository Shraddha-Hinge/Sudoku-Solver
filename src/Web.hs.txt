{-# LANGUAGE OverloadedStrings #-}

module Web (runServer) where

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)
import Sudoku (initialBoard, Board, isValid, solve)

data SudokuInput = SudokuInput { board :: Board } deriving (Show, Generic)

instance FromJSON SudokuInput
instance ToJSON SudokuInput

runServer :: IO ()
runServer = scotty 3000 $ do
  get "/" $ file "static/index.html"
  get "/board" $ json initialBoard

  post "/solve" $ do
    input <- jsonData `rescue` (\_ -> return $ SudokuInput initialBoard)
    let solution = solve (board input)
    case solution of
      Just solvedBoard -> json solvedBoard
      Nothing -> json ("Invalid Sudoku board" :: String)
