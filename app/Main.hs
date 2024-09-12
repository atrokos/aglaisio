module Main where
import System.Environment ( getArgs )
import DirectoryStructure (dirStructFromString, seekDirStruct, DirStruct)
import System.IO ( readFile' )
import Control.Monad.Trans.Except (runExceptT, except)

handleInput :: String -> String -> IO ()
handleInput configFilePath dirPath = do
  configContent <- readFile' configFilePath
  result <- runExceptT $ do
    configStructure <- except (dirStructFromString configContent)
    actualStructure <- seekDirStruct dirPath
    return $ compareDirStructs configStructure actualStructure

  either putStrLn putStrLn result
  
-- |For now, it compares only using the built-in way.
compareDirStructs :: DirStruct -> DirStruct -> String
compareDirStructs expected actual
  | expected == actual = "The directories match."
  | otherwise = "The directories do not match."

main :: IO ()
main = do
  argumentList <- getArgs
  case argumentList of
    [configFilePath, dirPath] -> handleInput configFilePath dirPath
    _ -> putStrLn "Only two arguments are allowed: path to the configuration file and path to the actual directory to be validated."