module FileSystem where

import HelperFunctions

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