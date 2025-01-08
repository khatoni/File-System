module FileSystem where

import HelperFunctions
import Data.Maybe ( fromMaybe )

data FileSystemElement
    = File String String | Directory String [FileSystemElement]
    deriving (Show, Eq)

getName :: FileSystemElement -> String
getName ( File name _ )      = name
getName ( Directory name _ ) = name

getFileContent :: FileSystemElement -> String
getFileContent ( File _ content ) = content
getFileContent ( Directory _ _ )  = ""

getDirectoryContent :: FileSystemElement -> [String]
getDirectoryContent ( File name content )       = []
getDirectoryContent ( Directory name children ) = map getName children

getChild :: String -> [FileSystemElement] -> Maybe FileSystemElement
getChild _ [] = Nothing
getChild name (child:children)
    | getName child == name  = Just child
    | otherwise              = getChild name children

traverseFileSystem:: FileSystemElement -> [String] -> Maybe FileSystemElement
traverseFileSystem fileSystemElement [] = Nothing
traverseFileSystem f@(File name content) (current:rest)
    | name == current && null rest = Just f
    | otherwise                    = Nothing

traverseFileSystem d@(Directory name children) (current:rest)
    | name == current && null rest      = Just d
    | otherwise                         = getChild (head rest) children >>= \child -> traverseFileSystem child rest


addFile :: FileSystemElement -> [String] -> FileSystemElement -> FileSystemElement
addFile f@(File name content) (current:rest) fileToCreate = f
addFile d@(Directory name children) (current:rest) fileToCreate
    | name == current && length rest == 1  = Directory name (fileToCreate : children)
    | otherwise                            = do
        let toUpdate = getChild (head rest) children
        case toUpdate of
            Nothing -> d
            Just fileElement -> Directory name (addFile fileElement rest fileToCreate : filter (/= fileElement) children)


mkdir :: FileSystemElement -> [String] -> String -> FileSystemElement
mkdir root currentDirectory folderName = addFile root path (Directory (last path) []) where
    path = parseFilePath currentDirectory folderName

touch:: FileSystemElement -> [String] -> String -> FileSystemElement
touch root currentDirectory fileName = addFile root path (File (last path) "") where
    path = parseFilePath currentDirectory fileName




pwd :: [String] -> String
pwd currentDirectory = let presentWorkingDirectory = concatMap (++ "/") (init currentDirectory) ++ last currentDirectory in
    if presentWorkingDirectory == "/" then "/" else tail presentWorkingDirectory

cd :: FileSystemElement -> [String] -> String -> [String]
cd root currentDirectory path = let parsedPath = parseFilePath currentDirectory path in 
    case traverseFileSystem root (parseFilePath currentDirectory path) of
    Nothing              -> currentDirectory
    Just (File _ _)      -> currentDirectory
    Just (Directory _ _) -> parsedPath

ls :: Maybe String -> FileSystemElement -> [String] -> [String]
ls Nothing root currentDirectory= fromMaybe [] (getDirectoryContent (traverseFileSystem root currentDirectory))
ls (Just path) root _ = fromMaybe [] (getDirectoryContent (traverseFileSystem root (parsePath (createQueryPath path) [])))