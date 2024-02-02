module Main (main) where

import System.Console.Haskeline
import Control.Exception
import Control.Monad.IO.Class
import HW5.Parser
import HW5.Base
import HW5.Evaluator
import HW5.Pretty
import HW5.Action
import Data.Set
import qualified Text.Megaparsec as MP

main :: IO ()
main = runInputT defaultSettings loop

loop :: InputT IO ()
loop = do
    minput <- getInputLine "hi> "
    case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          case parse input of
            Left parseError -> outputStrLn $ "Parse error: " ++ MP.errorBundlePretty parseError
            Right expr -> do
              result <- liftIO (try (runHIO (eval expr) (fromList [AllowRead, AllowWrite, AllowTime]))
                :: IO (Either SomeException (Either HiError HiValue)))
              case result of 
                Right (Right value) -> outputStrLn $ show $ prettyValue value
                Right (Left evalError) -> outputStrLn $ "Evaluation error: " ++ show evalError
                Left ioErr -> outputStrLn $ "IO error: " ++ show ioErr
              loop