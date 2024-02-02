{-# LANGUAGE DerivingVia #-}
module HW5.Action
  ( HiPermission (..)
  , PermissionException (..)
  , HIO (..)
  ) where

import Codec.Binary.UTF8.String
import Control.Exception
import Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import Data.Set
import qualified Data.Text as Text
import Data.Time.Clock
import HW5.Base
import System.Directory
import System.Random

-- | A permission for IO actions
data HiPermission = AllowRead
                  | AllowWrite
                  | AllowTime
                  deriving (Show, Eq, Ord)

-- | A permission absence exception
newtype PermissionException = PermissionRequired HiPermission deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad) via (ReaderT (Set HiPermission) IO)

instance HiMonad HIO where
  -- | An action of reading a file or listing a directory with the permission to read
  runAction (HiActionRead path) = HIO
      { runHIO = \permissions ->
              if AllowRead `elem` permissions
              then do
                isDirectory <- doesDirectoryExist path
                if isDirectory
                then do
                  contents <- listDirectory path
                  return $ HiValueList $ Seq.fromList $ Prelude.map (HiValueString . Text.pack) contents
                else do
                  fileContent <- BS.readFile path
                  let str = decode $ BS.unpack fileContent
                  if isUTF8Encoded str
                  then return $ HiValueString $ Text.pack str
                  else return $ HiValueBytes fileContent
              else throwIO $ PermissionRequired AllowRead
      }
  -- | An action of writing a file with the permission to write
  runAction (HiActionWrite path bytes) = HIO
        { runHIO = \permissions ->
                if AllowWrite `elem` permissions
                then do
                  BS.writeFile path bytes
                  return HiValueNull
                else throwIO $ PermissionRequired AllowWrite
        }
  -- | An action of creating a new directory with the permission to write
  runAction (HiActionMkDir path) = HIO
          { runHIO = \permissions ->
                if AllowWrite `elem` permissions
                then do
                  createDirectory path
                  return HiValueNull
                else throwIO $ PermissionRequired AllowWrite
          }
  -- | An action of changing a working directory with the permission to read
  runAction (HiActionChDir path) = HIO
        { runHIO = \permissions ->
                if AllowRead `elem` permissions
                then do
                  setCurrentDirectory path
                  return HiValueNull
                else throwIO $ PermissionRequired AllowRead
        }
  -- | An action of getting the current directory with the permission to read
  runAction HiActionCwd = HIO
        { runHIO = \permissions ->
                if AllowRead `elem` permissions
                then do HiValueString . Text.pack <$> getCurrentDirectory
                else throwIO $ PermissionRequired AllowRead
        }
  -- | An action of getting the current time with the permission to get time
  runAction HiActionNow = HIO
          { runHIO = \permissions ->
                if AllowTime `elem` permissions
                then do HiValueTime <$> getCurrentTime
                else throwIO $ PermissionRequired AllowTime
          }
  -- | An action of getting a random integral number from the specified range
  runAction (HiActionRand a b) = HIO
          { runHIO = \_ -> do
                n <- randomRIO (a, b)
                return $ HiValueNumber $ toRational n
          }
  -- | An action of printing a string to stdout
  runAction (HiActionEcho text) = HIO
          { runHIO = \permissions ->
                if AllowWrite `elem` permissions
                then do
                  putStrLn $ Text.unpack text
                  return HiValueNull
                else throwIO $ PermissionRequired AllowWrite
          }
