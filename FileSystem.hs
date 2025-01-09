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

getDirectoryContent :: Maybe FileSystemElement -> [String]
getDirectoryContent Nothing                          = []
getDirectoryContent (Just (File name content))       = []
getDirectoryContent (Just (Directory name children)) = map getName children

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
    | name == current && null rest         = Directory name (fileToCreate : filter (\child -> getName child /= getName fileToCreate) children)
    | name == current && length rest == 1  = Directory name (fileToCreate : filter (\child -> getName child /= getName fileToCreate) children)
    | otherwise                            = do
        let toUpdate = getChild (head rest) children
        case toUpdate of
            Nothing -> d
            Just fileElement -> Directory name (addFile fileElement rest fileToCreate : filter (\child -> getName child /= getName fileElement) children)

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
    case traverseFileSystem root parsedPath of
    Nothing              -> currentDirectory
    Just (File _ _)      -> currentDirectory
    Just (Directory _ _) -> parsedPath

cp :: FileSystemElement -> [String] -> String -> String -> FileSystemElement
cp root currentDirectory filenameToCopy filenameToCreate =
    let filePathToCopy   = parseFilePath currentDirectory filenameToCopy
        filePathToCreate = parseFilePath currentDirectory filenameToCreate ++ [last filePathToCopy]
        updatedRoot = traverseFileSystem root filePathToCopy >>=
            (Just . addFile root filePathToCreate) in
                fromMaybe root updatedRoot

mv :: FileSystemElement -> [String] -> String -> String -> FileSystemElement
mv root currentDirectory filenameToMove filenameToCreate =
    let copiedRoot = cp root currentDirectory filenameToMove filenameToCreate in
        if root /= copiedRoot then rm copiedRoot (parseFilePath currentDirectory filenameToMove) else root

ls :: FileSystemElement -> [String] -> String -> [String]
ls root currentDirectory filePath = let parsedPath = parseFilePath currentDirectory filePath in
    getDirectoryContent (traverseFileSystem root parsedPath)

cat :: FileSystemElement -> [String] -> [String] -> IO ()
cat root _ [] = do
    line <- readFromConsole
    putStrLn line
cat root currentDirectory (path:paths)
    | null paths = do
                putStrLn (readFileContent root (parseFilePath currentDirectory path))
                return ()
    | otherwise  = do
                putStrLn (readFileContent root (parseFilePath currentDirectory path))
                cat root paths currentDirectory

catWithFile :: FileSystemElement -> [String] -> [String] -> String -> FileSystemElement
catWithFile root currentDirectory filesToRead fileToWrite = do
    let filePath = parseFilePath currentDirectory fileToWrite
    let file = traverseFileSystem root filePath
    let text = fileContentAccumulator root currentDirectory filesToRead
    case file of
        Nothing -> addFile root filePath (File fileToWrite text)
        Just (File _ content) -> addFile root filePath (File fileToWrite text)
        Just (Directory _ _) -> root

fileContentAccumulator :: FileSystemElement -> [String] -> [String] -> String
fileContentAccumulator root currentDirectory = foldl (\content path -> content ++ readFileContent root (parseFilePath currentDirectory path) ++ "\n") ""

readFileContent :: FileSystemElement -> [String] -> String
readFileContent root filePath =
    case traverseFileSystem root filePath of
        Nothing              -> "No such file"
        Just f@(File _ _)    -> getFileContent f
        Just (Directory _ _) -> "Cannot cat dir"
