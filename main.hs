import System.IO (hFlush, stdout)
import Data.Maybe ( fromMaybe )
import FileSystem
import HelperFunctions

currentDirectory :: [String]
currentDirectory = ["/"]

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

executeLSCommand :: FileSystemElement -> [String] -> [String] -> [String]
executeLSCommand root currentDirectory commandTokens
    | length commandTokens == 1    = ls root currentDirectory ""
    | otherwise                    = ls root currentDirectory (head (tail commandTokens))

executeCDCommand :: FileSystemElement -> [String] -> [String] -> [String]
executeCDCommand root currentDirectory commandTokens
    | length commandTokens == 2    = cd root currentDirectory (head (tail commandTokens))
    | otherwise                    = currentDirectory

--executeCatCommand :: FileSystemElement -> [String] -> [String] -> IO ()
--executeCatCommand root currentDirectory commandTokens
--    | length commandTokens == 1     = cat root currentDirectory []
--    | ">" `notElem` commandTokens   = cat root currentDirectory (tail commandTokens)

executeCPCommand :: FileSystemElement -> [String] -> [String] -> FileSystemElement
executeCPCommand root currentDirectory commandTokens = cp root currentDirectory (commandTokens !! 1) (commandTokens !! 2)

printFolderContent :: [String] -> IO ()
printFolderContent lsResult = do
                            putStrLn (concatMap (++ " ") lsResult)
                            hFlush stdout

main :: IO ()
main = commandExecutor root currentDirectory where
    commandExecutor root currentDirectory = do
        putStr "$ "
        hFlush stdout
        command  <- getLine
        let commandTokens = splitCommandToTokens command
        case head commandTokens of
            "ls" -> do
                    let lsResult = executeLSCommand root currentDirectory commandTokens
                    printFolderContent lsResult
                    commandExecutor root currentDirectory
            "cd" -> do
                    let cdResult = executeCDCommand root currentDirectory commandTokens
                    commandExecutor root cdResult
            "pwd" -> do
                    putStrLn (pwd currentDirectory)
                    commandExecutor root currentDirectory
            "cat" -> do
                    if ">" `elem` commandTokens then do
                        let readFiles = getFilesToRead commandTokens
                        let writeFile = getFileToWrite commandTokens
                        if null readFiles then do
                            text <- readFromConsole
                            let updatedDirectory = catWithFile root currentDirectory text writeFile
                            commandExecutor updatedDirectory currentDirectory
                        else  do
                            let fileText = fileContentAccumulator root currentDirectory readFiles
                            let updatedDirectory = catWithFile root currentDirectory fileText writeFile
                            commandExecutor updatedDirectory currentDirectory
                    else do
                        text <-cat root currentDirectory (tail commandTokens)
                        putStrLn text
                        commandExecutor root currentDirectory
            "rm" -> do
                    let rmResult = rm root (parseFilePath currentDirectory (last commandTokens))
                    commandExecutor rmResult currentDirectory
            "mkdir" -> commandExecutor (mkdir root currentDirectory (last commandTokens)) currentDirectory
            "touch" -> commandExecutor  (touch root currentDirectory (last commandTokens)) currentDirectory
            "head" -> commandExecutor root currentDirectory
            "tail" -> commandExecutor root currentDirectory
            "cp"   -> do
                    let cpResult = executeCPCommand root currentDirectory commandTokens
                    commandExecutor cpResult currentDirectory
            "mv"   -> commandExecutor root currentDirectory
            _    -> do
                    putStrLn $ head commandTokens
                    commandExecutor root currentDirectory


