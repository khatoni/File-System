
data FileSystemElement
    = File String String | Directory String [FileSystemElement]
    deriving (Show, Eq)


currentDirectory :: [String]
currentDirectory = ["root", "subdir1"]

pwd :: [String] -> String
pwd currentDirectory = concatMap (++ "/") (init currentDirectory) ++ last currentDirectory
-- >>> pwd currentDirectory
-- "root/first/second/third"


splitPathToTokens:: String -> [String]
splitPathToTokens [] = []
splitPathToTokens path = takeWhile (/= '/') path : splitPathToTokens ( dropWhile (== '/') (dropWhile (/= '/') path))

-- >>> splitPathToTokens "../test1/../test2"
-- ["..","test1","..","test2"]

isAbsolutePath :: String -> Bool
isAbsolutePath path = head path == '/'

createQueryPath :: String -> [String]
createQueryPath path
    | isAbsolutePath path = splitPathToTokens path
    | otherwise             = currentDirectory ++ splitPathToTokens path


parsePath :: [String] -> [String] -> [String]
parsePath [] tmp = tmp
parsePath path tmp
    | length tmp == 1 && head path == ".." = error "Invalid path"
    | head path == ".."                    = parsePath (tail path) (init tmp)
    | otherwise                           = parsePath (tail path)  (tmp ++ [head path])

-- >>> parsePath (createQueryPath "../hi") []
-- ["root","first","second","hi"]

-- >>> parsePath (createQueryPath "/misho/../pesho") []
-- ["","pesho"]

-- >>> parsePath (createQueryPath "test2/test3/../../../test4") []
-- ["root","first","second","test4"]

-- >>> parsePath (createQueryPath "/tmp1/../../tmp2") []
-- Invalid path


getName :: FileSystemElement -> String
getName (File name _) = name
getName (Directory name _) = name

getDirectoryContent :: Maybe FileSystemElement -> Maybe [String]
getDirectoryContent Nothing = Nothing
getDirectoryContent (Just (File name content)) = Nothing
getDirectoryContent ( Just (Directory name children)) =Just (map getName children)


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
    | name == current && null rest  = Just d
    | otherwise                     = getChild (head rest) children >>= \child -> traverseFileSystem child rest

root = Directory "root" 
    [ File "file1.txt" "Content of file 1"
    , Directory "subdir1" 
        [ File "file2.txt" "Content of file 2"
        , File "file3.txt" "Content of file 3"
        , Directory "subdir11" 
        [File "file111.txt" "Content of file 1111"]
        ]
    , Directory "subdir2" []

    ]

-- >>> traverseFileSystem root ["root", "file1.txt"]
-- Just (File "file1.txt" "Content of file 1")

-- >>> traverseFileSystem root ["root", "file1.txt", "file2.txt"]
-- Nothing

-- >>> traverseFileSystem root ["root", "subdir2"]
-- Just (Directory "subdir2" [])

-- >>> traverseFileSystem root ["root", "subdir1"]
-- Just (Directory "subdir1" [File "file2.txt" "Content of file 2",File "file3.txt" "Content of file 3"])

-- >>> traverseFileSystem root ["root", "subdir3"]
-- Nothing

-- >>> traverseFileSystem root ["root", "subdir2", "subdir3"]
-- Nothing

-- >>> traverseFileSystem root ["root", "subdir1", "file3.txtt"]
-- Nothing

-- >>> traverseFileSystem root ["root", "subdir1", "file3.txt"]
-- Just (File "file3.txt" "Content of file 3")

-- >>> traverseFileSystem root ["root", "subdir2", "file3.txt"]
-- Nothing
-- >>> traverseFileSystem root ["root", "subdir1", "file3.txt", "file4.txt"]
-- Nothing
-- >>> traverseFileSystem root ["root", "subdir1", "subdir11"]
-- Just (Directory "subdir11" [])


ls :: Maybe String -> Maybe [String]
ls Nothing = getDirectoryContent (traverseFileSystem root currentDirectory)
ls (Just path) = getDirectoryContent (traverseFileSystem root (parsePath (createQueryPath path) []))
-- >>> ls Nothing
-- Just ["file2.txt","file3.txt","subdir11"]
-- >>> ls (Just "..")
-- Just ["file1.txt","subdir1","subdir2"]

-- >>> ls (Just "/subdir1/subdir11")
-- Just ["file111.txt"]
