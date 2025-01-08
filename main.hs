import System.IO (hFlush, stdout)
import Data.Maybe ( fromMaybe )

data FileSystemElement
    = File String String | Directory String [FileSystemElement]
    deriving (Show, Eq)


currentDirectory :: [String]
currentDirectory = ["root"]

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
    | path == "/"           = ["/"]
    | isAbsolutePath path   = splitPathToTokens path
    | otherwise             = currentDirectory ++ splitPathToTokens path

-- FIX BUG
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

getFileContent :: FileSystemElement -> String
getFileContent (File _ content) = content
getFileContent (Directory _ _)  = "Directory"

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
    | name == "root" && current == "/"  = Just d
    | name == current && null rest      = Just d
    | otherwise                         = getChild (head rest) children >>= \child -> traverseFileSystem child rest


root :: FileSystemElement
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

-- >>> traverseFileSystem root ["/"]
-- Just (Directory "root" [File "file1.txt" "Content of file 1",Directory "subdir1" [File "file2.txt" "Content of file 2",File "file3.txt" "Content of file 3",Directory "subdir11" [File "file111.txt" "Content of file 1111"]],Directory "subdir2" []])

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

addFile :: FileSystemElement -> [String] -> FileSystemElement -> FileSystemElement
addFile f@(File name content) (current:rest) fileToCreate = f
addFile d@(Directory name children) (current:rest) fileToCreate
    | name == current && length rest == 1  = Directory name (fileToCreate : children)
    | otherwise                            = do
        let toUpdate = getChild (head rest) children
        case toUpdate of
            Nothing -> d
            Just fileElement -> Directory name (addFile fileElement rest fileToCreate : filter (/= fileElement) children)


mkdir :: FileSystemElement -> String -> FileSystemElement
mkdir root folderName = addFile root path (Directory (last path) []) where
    path = parsePath (createQueryPath folderName) []


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

ls :: Maybe String -> FileSystemElement -> [String]
ls Nothing root = fromMaybe [] (getDirectoryContent (traverseFileSystem root currentDirectory))
ls (Just path) root = fromMaybe [] (getDirectoryContent (traverseFileSystem root (parsePath (createQueryPath path) [])))
-- >>> ls Nothing
-- ["file1.txt","subdir1","subdir2"]
-- >>> ls (Just "..")
-- Invalid path

cd :: String -> [String]
cd path = case traverseFileSystem root (parsePath (createQueryPath path) []) of
    Nothing              -> currentDirectory
    Just (File _ _)      -> currentDirectory
    Just (Directory _ _) -> parsePath (createQueryPath path) []

-- >>> cd "/subdir1/subdir11"
-- ["","subdir1","subdir11"]

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

executeLSCommand :: [String] -> FileSystemElement -> [String]
executeLSCommand commandTokens root
    | length commandTokens == 1    = ls Nothing root
    | otherwise                    = ls (Just (head (tail commandTokens))) root

executeCDCommand :: [String] -> [String]
executeCDCommand commandTokens
    | length commandTokens == 2    = cd (head (tail commandTokens))
    | otherwise                    = currentDirectory


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
                    let lsResult = executeLSCommand commandTokens root
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
            _    -> do
                    putStrLn $ head commandTokens
                    commandExecutor root currentDirectory


