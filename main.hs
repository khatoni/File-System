data FileSystemElement
    = File String String | Directory String [FileSystemElement]
    deriving (Show, Eq)


currentDirectory :: [String]
currentDirectory = ["root", "first", "second", "third"]

pwd :: [String] -> String
pwd currentDirectory = concatMap (++ "/") (init currentDirectory) ++ last currentDirectory
-- >>> pwd currentDirectory
-- "root/first/second/third"
