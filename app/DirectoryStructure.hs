module DirectoryStructure where

import System.Directory (doesDirectoryExist, listDirectory, doesFileExist)
import Control.Monad.Trans.Except ( throwE, ExceptT, runExceptT )
import Control.Monad.IO.Class (liftIO)
import Control.Monad (filterM)
import qualified Data.Set as S
import Data.Char (isSpace)


data DirStruct =
  DirStruct {
    dirName :: FilePath,
    allDirs :: S.Set DirStruct,
    allFiles :: S.Set FilePath
  }
  deriving (Show, Eq, Ord)

seekDirStruct :: FilePath -> ExceptT String IO DirStruct
seekDirStruct dirname = do
  isDir <- liftIO $ doesDirectoryExist dirname
  if not isDir 
  then throwE $ dirname ++ " is not a directory!"
  else liftIO $ do
    contents <- listDirectory dirname
    let fullPath = ((dirname ++ "/") ++)
    files <- filterM doesFileExist $ map fullPath contents
    dirs <- filterM doesDirectoryExist $ map fullPath contents
    recursiveDirs <- traverse (runExceptT . seekDirStruct) dirs
    let validDirs = [d | Right d <- recursiveDirs] -- Skipping invalid directories
    return $ DirStruct dirname (S.fromList validDirs) (S.fromList files)

data ContentType =
  Dir FilePath Int      -- ^The Content is a directory with a path and a certain nestedness level.
  | File FilePath Int   -- ^The same for a file.
  deriving Show

removeCommentFromLine :: String -> String
removeCommentFromLine line = reverse $ dropWhile isSpace $ reverse $ takeWhile (/= ';') line

removeComments :: [String] -> [FilePath]
removeComments contents = filter (not . isEmpty) $ map removeCommentFromLine contents
  where
    isEmpty :: String -> Bool
    isEmpty line = (length $ filter (not . isSpace) line) == 0

getName :: [Char] -> [Char]
getName = dropWhile isSpace

getNestLevel :: String -> Int
getNestLevel line = length $ takeWhile (== '\t') line

getContentLevel :: ContentType -> Int
getContentLevel (Dir _ level) = level
getContentLevel (File _ level) = level

parseDirLine :: String -> ContentType
parseDirLine line =
  let
    name = getName line
    level = getNestLevel line
  in case name of
    ('/':rest) -> Dir rest level
    _ -> File name level

parseDirLines :: String -> [ContentType]
parseDirLines content = parseDirLine <$> (removeComments $ lines content)

emptyDirStruct :: String -> DirStruct
emptyDirStruct name = DirStruct name S.empty S.empty

createDirStruct :: [ContentType] -> Int -> DirStruct -> (DirStruct, [ContentType])
createDirStruct [] _ currDirStruct = (currDirStruct, [])
createDirStruct (content:rest) currLevel currDirStruct =
  case content of
    (File name level) ->  if level < currLevel then
                            (updatedDirStruct, (content:rest))
                          else createDirStruct rest currLevel updatedDirStruct
                          where
                            updatedFiles = S.insert ((dirName currDirStruct) ++ "/" ++ name) (allFiles currDirStruct)
                            updatedDirStruct = currDirStruct {allFiles = updatedFiles}
    (Dir name level) -> if level < currLevel then
                            (currDirStruct, (content:rest))
                        else let
                          (newDirStruct, nextContent) = createDirStruct rest level (emptyDirStruct ((dirName currDirStruct) ++ "/" ++ name))
                          updatedDirs = S.insert newDirStruct (allDirs currDirStruct)
                          updatedDirStruct = currDirStruct {allDirs = updatedDirs}
                          in createDirStruct nextContent level updatedDirStruct

validateContentTypeList :: [ContentType] -> Int -> Either String [ContentType]
validateContentTypeList list rootLevel
  | levelCheck (== rootLevel) = Left "There can only be one root!"
  | levelCheck (< rootLevel)  = Left "Some content has higher level than the root!"
  | otherwise = Right list
  where
    contentLevels = map getContentLevel list
    levelCheck comp = (length $ filter comp contentLevels) == 0

dirStructFromString :: String -> Either String DirStruct
dirStructFromString contents =
  case parseDirLines contents of 
    [] -> Left "No directory tree is present in the configuration file!"
    (root:parsed) -> case root of
                      (File _ _) -> Left "File cannot be a root!"
                      (Dir name level) -> do
                        validated <- validateContentTypeList parsed level
                        let slashName = "./" ++ name
                        let (created, _) = createDirStruct validated level (emptyDirStruct slashName)
                        return created

dumpFiles :: [FilePath] -> Int -> String
dumpFiles files level = concatMap indentNewline files
  where
    indent = replicate level '\t'
    indentNewline x = indent ++ x ++ "\n"

dumpDirStruct :: DirStruct -> Int -> String
dumpDirStruct dirStruct level =
  let
    indent = replicate level '\t'
    currDirName = indent ++ dirName dirStruct ++ "\n"
    filesList = dumpFiles ((S.toList . allFiles) dirStruct) (level + 1)
    dirsList = concatMap (`dumpDirStruct` (level+1)) $ S.toList $ allDirs dirStruct
  in currDirName ++ filesList ++ dirsList



