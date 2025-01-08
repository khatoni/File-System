import System.IO (hFlush, stdout)
import Data.Maybe ( fromMaybe )
import FileSystem

currentDirectory :: [String]
currentDirectory = ["/", "pesho", "gosho"]

-- >>> pwd currentDirectory
-- "/pesho/gosho"

root :: FileSystemElement
root = Directory "/"
    [ File "file1.txt" "Content of file 1"
    , Directory "subdir1"
        [ File "file2.txt" "Content of file 2"
        , File "file3.txt" "Content of file 3"
        , Directory "subdir11"
        [File "file111.txt" "Content of file 1111"]
        ]
    , Directory "subdir2" []

    ]

cp :: FileSystemElement -> String -> String -> Maybe FileSystemElement
cp root filenameToCopy filenameToCreate =
    let filePath = parsePath (createQueryPath filenameToCopy) [] in
        traverseFileSystem root filePath >>= 
            (\file -> Just (addFile root filePath (File (getName file) (getFileContent file))))


rm :: FileSystemElement -> [String] -> FileSystemElement
rm f@(File name content) (current:rest) = f
rm file [] = file
rm d@(Directory name children) ["/"] = d
rm d@(Directory name children) (current:rest)
    | name == current && null rest              = d
    | name == current && length rest == 1       = do
        let child = getChild (head rest) children
        case child of
            Nothing -> d
            Just (File filename _)      -> Directory name (filter (\x -> getName x /= filename) children)
            Just (Directory dirName []) -> Directory name (filter (\x -> getName x /= dirName) children)
            Just (Directory _ _)        -> d
    | otherwise                             = do
        let toUpdate = getChild (head rest) children
        case toUpdate of
            Nothing -> d
            Just fileElement -> Directory name (map (\x-> if getName x == getName fileElement then rm x (tail rest) else x) children)

-- >>> rm root ["root","file1.txt"]
-- Directory "root" [Directory "subdir1" [File "file2.txt" "Content of file 2",File "file3.txt" "Content of file 3",Directory "subdir11" [File "file111.txt" "Content of file 1111"]],Directory "subdir2" []]





-- >>> mkdir root "subdir1/testing"
-- Directory "root" [Directory "subdir1" [Directory "testing" [],File "file2.txt" "Content of file 2",File "file3.txt" "Content of file 3",Directory "subdir11" [File "file111.txt" "Content of file 1111"]],File "file1.txt" "Content of file 1",Directory "subdir2" []]

ls :: Maybe String -> FileSystemElement -> [String] -> [String]
ls Nothing root currentDirectory= fromMaybe [] (getDirectoryContent (traverseFileSystem root currentDirectory))
ls (Just path) root _ = fromMaybe [] (getDirectoryContent (traverseFileSystem root (parsePath (createQueryPath path) [])))
-- >>> ls Nothing
-- ["file1.txt","subdir1","subdir2"]
-- >>> ls (Just "..")
-- Invalid path

readFileContent :: String -> FileSystemElement -> String
readFileContent filePath root = case traverseFileSystem root (parsePath (createQueryPath filePath) []) of
    Nothing              -> "No such file"
    Just f@(File _ _)    -> getFileContent f
    Just (Directory _ _) -> "Cannot cat dir"

cat :: [String] -> FileSystemElement -> IO ()
cat [] root = do
    line <- getLine
    putStrLn line
cat (path:paths) root
    | null paths = do
                putStrLn (readFileContent path root)
                return ()
    | otherwise  = do
                putStrLn (readFileContent path root)
                cat paths root


splitCommandToTokens:: String -> [String]
splitCommandToTokens [] = []
splitCommandToTokens command = takeWhile (/= ' ') command : splitCommandToTokens ( dropWhile (== ' ') (dropWhile (/= ' ') command))

-- >>> splitCommandToTokens "cat file1.txt /subdir1/subdir2/file2.txt file3.txt > file4.txt"
-- ["cat","file1.txt","/subdir1/subdir2/file2.txt","file3.txt",">","file4.txt"]

fileContentAccumulator :: FileSystemElement -> [String] -> String ->String
fileContentAccumulator root [] result = result
fileContentAccumulator root (path:paths) content = fileContentAccumulator root paths (content ++ "\n" ++ readFileContent path root)


catWithFile :: FileSystemElement -> [String] -> String -> FileSystemElement
catWithFile root filesToRead fileToWrite = do
    let filePath = parsePath (createQueryPath fileToWrite) []
    let file = traverseFileSystem root filePath
    let text = fileContentAccumulator root filesToRead ""
    case file of
        Nothing -> addFile root filePath (File fileToWrite text)
        Just (File _ content) -> addFile root filePath (File fileToWrite text)
        Just (Directory _ _) -> root

-- >>> catWithFile ["file1.txt"] "file2.txt"
-- Directory "root" [File "file2.txt" "Content of file 1",File "file1.txt" "Content of file 1",Directory "subdir1" [File "file2.txt" "Content of file 2",File "file3.txt" "Content of file 3",Directory "subdir11" [File "file111.txt" "Content of file 1111"]],Directory "subdir2" []]

executeLSCommand :: [String] -> FileSystemElement -> [String] -> [String]
executeLSCommand commandTokens root currentDirectory
    | length commandTokens == 1    = ls Nothing root currentDirectory
    | otherwise                    = ls (Just (head (tail commandTokens))) root currentDirectory

executeCDCommand :: [String] -> [String]
executeCDCommand commandTokens
    | length commandTokens == 2    = cd (head (tail commandTokens))
    | otherwise                    = currentDirectory

executeCPCommand :: [String] -> FileSystemElement -> FileSystemElement
executeCPCommand commandTokens root
    | length commandTokens /= 3   = root
    | otherwise                   = let cpResult = cp root (commandTokens !! 2) (commandTokens !! 3) in fromMaybe root cpResult

getReadFiles :: [String] -> [String]
getReadFiles commandParams= takeWhile (/= ">") (tail commandParams)

getFileToWrite:: [String] -> String
getFileToWrite commandParams = head (tail (dropWhile (/= ">") commandParams))

executeCatCommand commandTokens root
    | length commandTokens == 1     = cat [] root
    | ">" `notElem` commandTokens   = cat (tail commandTokens) root
-- >>> getFileToWrite ["cat", "/f1", "/f2", ">", "file4", "file5", "file6"]
-- "file4"

printFolderContent :: [String] -> IO ()
printFolderContent lsResult = do
                            putStrLn (concatMap (++ " ") lsResult)
                            hFlush stdout
-- >>> catWithFile (getReadFiles ["cat", "file1.txt", ">", "file2.txt"]) (getFileToWrite ["cat", "file1.txt", ">", "file2.txt"])
-- Directory "root" [File "file2.txt" "Content of file 1",File "file1.txt" "Content of file 1",Directory "subdir1" [File "file2.txt" "Content of file 2",File "file3.txt" "Content of file 3",Directory "subdir11" [File "file111.txt" "Content of file 1111"]],Directory "subdir2" []]

main :: IO ()
main = commandExecutor root currentDirectory where
    commandExecutor root currentDirectory = do
        putStr "$ "
        hFlush stdout
        command  <- getLine
        let commandTokens = splitCommandToTokens command
        case head commandTokens of
            "ls" -> do
                    let lsResult = executeLSCommand commandTokens root currentDirectory
                    printFolderContent lsResult
                    commandExecutor root currentDirectory
            "cd" -> do
                    let cdResult = executeCDCommand commandTokens
                    commandExecutor root cdResult
            "pwd" -> do
                    putStrLn (pwd currentDirectory)
                    commandExecutor root currentDirectory
            "cat" -> do
                    if ">" `elem` commandTokens then do
                        let updatedDirectory = catWithFile root (getReadFiles commandTokens) (getFileToWrite commandTokens)
                        commandExecutor updatedDirectory currentDirectory
                    else do
                        executeCatCommand commandTokens root
                        commandExecutor root currentDirectory
            "rm" -> do
                    let rmResult = rm root (parsePath (createQueryPath (last commandTokens)) [])
                    commandExecutor rmResult currentDirectory
            "mkdir" -> commandExecutor (mkdir root (last commandTokens)) currentDirectory
            "touch" -> commandExecutor  (touch root (last commandTokens)) currentDirectory
            "head" -> commandExecutor root currentDirectory
            "tail" -> commandExecutor root currentDirectory
            "cp"   -> do
                    let cpResult = executeCPCommand commandTokens root
                    commandExecutor cpResult currentDirectory
            "mv"   -> commandExecutor root currentDirectory
            _    -> do
                    putStrLn $ head commandTokens
                    commandExecutor root currentDirectory


