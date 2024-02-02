module HW6.T3
  ( Config (..)
  , Cell (..)
  , CellState (..)
  , Comonad19Grid

  , simulate
  ) where

import Control.Comonad (Comonad (..))
import Control.Monad
import Data.Functor.Identity
import Data.Grid
import Data.ListZipper
import Data.Word
import System.Random (StdGen, genWord32, mkStdGen, split)

data Config = Config
  { probability      :: Double
  , incubationPeriod :: Int
  , illnessDuration  :: Int
  , immunityDuration :: Int
  } deriving Show

data CellState
  = Healthy
  | Infected Int
  | Ill Int
  | Immune Int
  deriving Show

data Cell = Cell
  { cellState :: CellState
  , cellRand  :: StdGen
  }

type Comonad19Grid = Grid Cell

-- | Creates an infinite list of grids using the given configuration and seed.
-- Each element of this list represents one infection simulation step.
simulate :: Int -> Config -> [Comonad19Grid]
simulate seed config = iterate (extend (rule config)) (initGrid seed (incubationPeriod config))

-- | Update the cell state
rule :: Config -> Comonad19Grid -> Cell
rule config g = let cell = extract g
                    gen = cellRand cell
                in case cellState cell of
                  Healthy    -> contact config directions g cell
                  Infected 0 -> Cell (Ill $ illnessDuration config) gen
                  Ill 0      -> Cell (Immune $ immunityDuration config) gen
                  Immune 0   -> Cell Healthy gen
                  Infected n -> Cell (Infected $ n - 1) gen
                  Ill n      -> Cell (Ill $ n - 1) gen
                  Immune n   -> Cell (Immune $ n - 1) gen

-- | Makes contact between the cell and its neighbors
contact :: Config -> [Comonad19Grid -> Comonad19Grid] -> Comonad19Grid -> Cell -> Cell
contact config (direction : ds) g cell =
  let neighbor = extract $ direction g
  in case cellState neighbor of
    Infected _ -> infect cell neighbor g config ds
    Ill _      -> infect cell neighbor g config ds
    _          -> contact config ds g cell
contact _ [] _ cell = cell

-- | Infects the given cell from its neighbor with the given probability
infect :: Cell -> Cell -> Comonad19Grid -> Config -> [Comonad19Grid -> Comonad19Grid] -> Cell
infect cell neighbor g config ds = runIdentity $ do
         let (rand, newGen) = genWord32 $ cellRand neighbor
         void $ return neighbor { cellRand = newGen }
         return $ if fromInteger (toInteger rand) < probability config * fromInteger (toInteger (maxBound :: Word32))
                  then cell { cellState = Infected $ incubationPeriod config }
                  else contact config ds g cell

-- | Infection directions
directions :: [Comonad19Grid -> Comonad19Grid]
directions = [gLeft, gRight, gUp, gDown]

-- | Initial grid
initGrid :: Int -> Int -> Comonad19Grid
initGrid seed period = gWrite
                  (Cell (Infected period) (mkStdGen seed))
                  (Grid $ lGenerator lLeft lRight (initLZ seed))

-- | Initial row of the grid
initLZ :: Int -> ListZipper Cell
initLZ seed = lGenerator nextCell nextCell (Cell Healthy (mkStdGen seed))

-- | Generates the next cell of the row
nextCell :: Cell -> Cell
nextCell cell = let (_, gen) = split $ cellRand cell
                in Cell Healthy gen
