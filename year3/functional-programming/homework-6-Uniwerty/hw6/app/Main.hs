module Main (main) where

import Data.Grid
import Data.ListZipper
import HW6.T3
import Options.Applicative
import System.Random (randomIO)

data Options = Options
  { config     :: Config
  , gridSize   :: Int
  , iterations :: Int
  }

main :: IO ()
main = do
  ops <- options
  seed <- randomIO
  mapM_ (printGrid $ gridSize ops) (take (iterations ops) (simulate seed (config ops)))

options :: IO Options
options = execParser $ info optionsParser fullDesc

optionsParser :: Parser Options
optionsParser = Options
                   <$> (Config
                          <$> option (auto :: ReadM Double) (long "prob")
                          <*> option (auto :: ReadM Int) (long "incub")
                          <*> option (auto :: ReadM Int) (long "ill")
                          <*> option (auto :: ReadM Int) (long "immun")
                       )
                   <*> option (auto :: ReadM Int) (long "grid-size")
                   <*> option (auto :: ReadM Int) (long "iterations")

printGrid :: Int -> Comonad19Grid -> IO ()
printGrid size grid = printListZipper (printRow size) size (unGrid grid)

printRow :: Int -> ListZipper Cell -> IO ()
printRow = printListZipper printCell

printListZipper :: (a -> IO ()) -> Int -> ListZipper a -> IO ()
printListZipper printF size (LZ ls x rs) = do
    let half = div size 2
    mapM_ printF (reverse $ take half ls)
    printF x
    mapM_ printF (take (if odd size then half else half - 1) rs)
    putStrLn ""

printCell :: Cell -> IO ()
printCell cell = case cellState cell of
                    Healthy    -> putChar '_'
                    Infected _ -> putChar 'i'
                    Ill _      -> putChar '#'
                    Immune _   -> putChar '@'
