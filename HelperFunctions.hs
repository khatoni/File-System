module HelperFunctions where

isAbsolutePath :: String -> Bool
isAbsolutePath path = not (null path) && head path == '/'

createQueryPath :: [String] -> String -> [String]
createQueryPath currentDirectory path
    | path == "/"           = ["/"]
    | isAbsolutePath path   = "/" : tail (splitPathToTokens path)
    | otherwise             = currentDirectory ++ splitPathToTokens path where 
        splitPathToTokens :: String -> [String]
        splitPathToTokens [] = []
        splitPathToTokens path = takeWhile (/= '/') path : splitPathToTokens ( dropWhile (== '/') (dropWhile (/= '/') path))

parseFilePath :: [String] -> String -> [String]
parseFilePath currentDirectory filePath = parsePath (createQueryPath currentDirectory filePath) [] where  
    parsePath :: [String] -> [String] -> [String]
    parsePath [] tmp = tmp
    parsePath path tmp
        | length tmp == 1 && head path == ".." = error "Invalid path"
        | head path == ".."                    = parsePath (tail path) (init tmp)
        | otherwise                            = parsePath (tail path)  (tmp ++ [head path])

splitCommandToTokens:: String -> [String]
splitCommandToTokens [] = []
splitCommandToTokens command = takeWhile (/= ' ') command : splitCommandToTokens ( dropWhile (== ' ') (dropWhile (/= ' ') command))


getFilesToRead :: [String] -> [String]
getFilesToRead commandParams= takeWhile (/= ">") (tail commandParams)

getFileToWrite:: [String] -> String
getFileToWrite commandParams = head (tail (dropWhile (/= ">") commandParams))

readFromConsole:: IO String
readFromConsole = consoleAccumulator "" where 
                consoleAccumulator result = do
                    line <- getLine
                    if line == "." then return result else consoleAccumulator (result ++ "\n" ++ line)
