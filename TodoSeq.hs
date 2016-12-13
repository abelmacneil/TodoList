import System.IO
import Control.Applicative
import System.Directory
import System.Environment
import Control.Monad
import Control.Monad.Writer
import Control.Exception
import Data.Sequence ((<|), (|>), (><))
import qualified Data.Sequence as Seq
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Char as Char
import qualified Data.Foldable as F

type TodoList = Seq.Seq String
type Command = [String] -> TodoList -> TodoList
type CommandMap = Map.Map String (Int, Command)

todoFileName :: IO String
todoFileName = getAppUserDataDirectory "todoList"

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

add :: Command
add [task] todoList = todoList |> task

--remove :: Command
--remove [taskNum] todoList = 
--    let number = (read taskNum :: Int) - 1 in
--      if number >= 0 && number < length todoList then
--        List.delete (todoList !! number) todoList
--      else
--        todoList
--
--pop :: Command
--pop _ = tail 
--
--push :: Command
--push [task] todoList = task:todoList
--
--replace :: Command
--replace [indexStr, newTask] todoList = 
--    let index = read indexStr in
--      if index >= 0 && index < length todoList then
--        let (beginning, (_:end)) = List.splitAt (index - 1) todoList in
--          beginning ++ (newTask:end)
--      else
--        todoList
--
--clear :: Command
--clear _ _ = []
--
--insert :: Command
--insert [indexStr, task] todoList =
--    let index = read indexStr - 1
--        (beg, end) = splitAt index todoList in
--      beg ++ [task] ++ end
--
--swap :: Command
--swap [numStr1, numStr2] todoList =
--    let num1 = read numStr1 - 1
--        num2 = read numStr2 - 1 in
--      swapHelper (todoList !! num1) (todoList !! num2) todoList
--
--swapHelper :: (Eq a) => a -> a -> [a] -> [a]
--swapHelper _ _ [] = []
--swapHelper item1 item2 (x:xs)
--  | x == item1 = item2:(swapHelper item1 item2 xs)
--  | x == item2 = item1:(swapHelper item1 item2 xs)
--  | otherwise = x:(swapHelper item1 item2 xs)
--
commands :: CommandMap
commands = Map.fromList $
              [ ("add",     (1, add))
              , ("remove",  (1, remove))
--              , ("pop",     (0, pop))
--              , ("push",    (1, push))
--              , ("replace", (2, replace))
--              , ("clear",   (0, clear))
--              , ("insert",  (2, insert))
--              , ("swap",    (2, swap))
              ]

view :: TodoList -> [String]
view todoList = 
    "TODO:" : (F.toList (Seq.zipWith (\n item -> show n ++ " - " ++ item) (Seq.fromList [1..(Seq.length todoList)]) todoList))

processArgs :: CommandMap -> TodoList -> [String] -> Writer [String] TodoList
processArgs cmdMap todoList [] = return todoList
processArgs cmdMap todoList ("view":args) = 
    (tell $ view todoList) >> processArgs cmdMap todoList args
processArgs cmdMap todoList (arg:args) = do
    let (numArgs, fct) = case Map.lookup arg cmdMap of
                        Nothing -> (-1, \_ _-> return [])
                        Just (n, fct) -> (n,fct)
    if numArgs < 0 then 
      tell ["Command '" ++ arg ++ "' does not exist."] >>
      processArgs cmdMap todoList args
    else
      processArgs cmdMap (fct (take numArgs args) todoList) $ drop numArgs args

writeNewTodoList :: TodoList -> IO ()
writeNewTodoList newList = do
    fileName <- todoFileName
    bracketOnError (openTempFile "." "temp")
      (\(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName)
      (\(tempName, tempHandle) -> do
        hPutStr tempHandle $ encode fileKey $ unlines $ F.toList newList
        hClose tempHandle
        removeFile fileName
        renameFile tempName fileName)

main :: IO ()
main = do
    fileName <- todoFileName
    fileExists <- doesFileExist fileName
    when (not fileExists) $ do
      writeFile fileName ""
      putStrLn $ "Created " ++ fileName
    
    oldList <- fmap (\text -> Seq.fromList $ lines $ decode fileKey text) $ readFile fileName 
    args <- getArgs
    let (todoList, logs) = runWriter $ processArgs commands oldList args in
        mapM putStrLn logs >> writeNewTodoList  todoList 

