data FileSystemElement
    = File String String | Directory String [FileSystemElement]
    deriving (Show, Eq)


currentDirectory :: [String]
currentDirectory = ["root", "first", "second", "third"]

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
