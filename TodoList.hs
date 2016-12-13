import System.IO
import Control.Applicative
import System.Directory
import System.Environment
import Control.Monad
import Control.Monad.Writer
import Control.Exception
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Char as Char

type TodoList = [String]
type Command = [String] -> TodoList -> TodoList
type CommandMap = Map.Map String (Int, Command)

localFileName :: String
localFileName = ".todoList"
todoFileName :: IO String
todoFileName = do
    cond <- doesFileExist localFileName
    if (cond) 
      then return localFileName
      else getAppUserDataDirectory $ tail localFileName

printArgError :: String -> Int -> IO ()
printArgError cmd numArgs = 
    putStrLn $ "The " ++ cmd ++ " command takes " ++ 
      (show numArgs) ++ " argument(s)."

fileKey :: Int
fileKey = 20

encode :: Int -> String -> String
encode key = map (Char.chr . (+key) . Char.ord)

decode :: Int -> String -> String
decode key = encode (-key)

push :: Command
push [task] todoList = todoList ++ [task]

pop :: Command
pop [] = init

remove :: Command
remove [taskNum] todoList = 
    let number = (read taskNum :: Int) - 1 in
      if number >= 0 && number < length todoList then
        List.delete (todoList !! number) todoList
      else
        todoList

shift :: Command
shift _ = tail 

unshift :: Command
unshift [task] todoList = task:todoList

replace :: Command
replace [indexStr, newTask] todoList = 
    let index = read indexStr in
      if index >= 0 && index < length todoList then
        let (beginning, (_:end)) = List.splitAt (index - 1) todoList in
          beginning ++ (newTask:end)
      else
        todoList

clear :: Command
clear _ _ = []

insert :: Command
insert [indexStr, task] todoList =
    let index = read indexStr - 1
        (beg, end) = splitAt index todoList in
      beg ++ [task] ++ end

swap :: Command
swap [numStr1, numStr2] todoList =
    let num1 = read numStr1 - 1
        num2 = read numStr2 - 1 in
      swap' (todoList !! num1) (todoList !! num2) todoList
  where swap' _ _ [] = []
        swap' item1 item2 (x:xs)
          | x == item1 = item2:(swap' item1 item2 xs)
          | x == item2 = item1:(swap' item1 item2 xs)
          | otherwise = x:(swap' item1 item2 xs)

create :: Command
create [] = id

commands :: CommandMap
commands = Map.fromList $
              [ ("push",      (1, push))
              , ("pop",       (0, pop))
              , ("remove",    (1, remove))
              , ("shift",     (0, shift))
              , ("unshift",   (1, unshift))
              , ("replace",   (2, replace))
              , ("clear",     (0, clear))
              , ("insert",    (2, insert))
              , ("swap",      (2, swap))
              , ("create",    (0, create))
              ]

view :: TodoList -> [String]
view todoList = 
    "TODO:" : (zipWith (\n item -> show n ++ " - " ++ item) [1..] todoList)

processArgs :: CommandMap -> TodoList -> [String] -> Writer [String] TodoList
processArgs _ todoList [] = return todoList
processArgs cmdMap todoList ("view":args) = 
    (tell $ view todoList) >> processArgs cmdMap todoList args
processArgs cmdMap todoList (arg:args) =
    case Map.lookup arg cmdMap of
        Nothing -> do
            tell ["Command '" ++ arg ++ "' does not exist."] 
            processArgs cmdMap todoList args
        Just (n, fct) -> do
            processArgs cmdMap (fct (take n args) todoList) $ drop n args

writeNewTodoList :: TodoList -> IO ()
writeNewTodoList newList = do
    fileName <- todoFileName
    bracketOnError (openTempFile "." "temp")
      (\(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName)
      (\(tempName, tempHandle) -> do
        hPutStr tempHandle $ unlines newList
        hClose tempHandle
        removeFile fileName
        renameFile tempName fileName)

main :: IO ()
main = do
    args <- getArgs
    when (not (null args) && head args == "create") $ do
      writeFile localFileName ""
      putStrLn $ "Created " ++ localFileName
    fileName <- todoFileName
    oldList <- fmap (\text -> lines text) $ readFile fileName 
    let (todoList, logs) = runWriter $ processArgs commands oldList args in
        mapM_ putStrLn logs >> writeNewTodoList todoList 

