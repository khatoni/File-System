import System.IO (hFlush, stdout)
import Data.Maybe ( fromMaybe )
import FileSystem
import HelperFunctions

currentDirectory :: [String]
currentDirectory = ["/"]

root :: FileSystemElement
root = Directory "/" []

executeLSCommand :: FileSystemElement -> [String] -> [String] -> [String]
executeLSCommand root currentDirectory commandTokens
    | length commandTokens == 1    = ls root currentDirectory ""
    | otherwise                    = ls root currentDirectory (head (tail commandTokens))

executeCDCommand :: FileSystemElement -> [String] -> [String] -> [String]
executeCDCommand root currentDirectory commandTokens
    | length commandTokens == 2    = cd root currentDirectory (head (tail commandTokens))
    | otherwise                    = currentDirectory

executeCPCommand :: FileSystemElement -> [String] -> [String] -> FileSystemElement
executeCPCommand root currentDirectory commandTokens = cp root currentDirectory (commandTokens !! 1) (commandTokens !! 2)

executeCatFromConsole :: FileSystemElement -> [String] -> [String] -> IO FileSystemElement
executeCatFromConsole root currentDirectory commandTokens = 
    let readFiles = getFilesToRead commandTokens
        writeFile = getFileToWrite commandTokens in
        if null readFiles then do
            text <- readFromConsole
            return (catWithFile root currentDirectory text writeFile)
        else  do
            let fileText = fileContentAccumulator root currentDirectory readFiles
            return (catWithFile root currentDirectory (init fileText) writeFile)

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
            "pwd"-> do
                    putStrLn (pwd currentDirectory)
                    commandExecutor root currentDirectory
            "cat"-> do
                    if ">" `elem` commandTokens then do
                        updatedDirectory <- executeCatFromConsole root currentDirectory commandTokens
                        commandExecutor updatedDirectory currentDirectory
                    else do
                        text <-cat root currentDirectory (tail commandTokens)
                        putStrLn text
                        commandExecutor root currentDirectory
            "rm" -> do
                    let rmResult = rm root (parseFilePath currentDirectory (last commandTokens))
                    commandExecutor rmResult currentDirectory
            "mkdir"-> commandExecutor (mkdir root currentDirectory (last commandTokens)) currentDirectory
            "touch"-> commandExecutor  (touch root currentDirectory (last commandTokens)) currentDirectory
            "head" -> do
                    let text = headFS root currentDirectory (commandTokens !! 1) (read (commandTokens !! 2))
                    putStrLn text
                    commandExecutor root currentDirectory
            "tail" -> do
                    let text = tailFS root currentDirectory (commandTokens !! 1) (read (commandTokens !! 2))
                    putStrLn text
                    commandExecutor root currentDirectory
            "cp"   -> do
                    let cpResult = executeCPCommand root currentDirectory commandTokens
                    commandExecutor cpResult currentDirectory
            "mv"   -> commandExecutor root currentDirectory
            "quit" -> putStrLn "Exiting"
            _      -> do
                    putStrLn $ head commandTokens
                    commandExecutor root currentDirectory


