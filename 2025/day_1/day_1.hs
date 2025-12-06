module Main where
import Distribution.Compat.Prelude (readMaybe)
import Data.IntSet (split)
import Distribution.Compat.CharParsing (CharParsing(string))

main :: IO ()
main = do
  putStrLn "Provide file path: "
  name <- getLine
  contents <- readFile name
  let commands = lines contents

  res <- countPosition commands 50 0

  putStrLn ("Key = " ++ show res ++ " !")


parse :: Read a => String -> Maybe a
parse = readMaybe


countPosition :: [String] -> Int -> Int -> IO Int
countPosition [] _ count = return count
countPosition (cmd:rest) state count = 
  case cmd of 
    ('R':amount) ->
      case parse amount :: Maybe Int of 
        Nothing -> return count
        Just number -> do
          let n = state+number
          let new = recrMinus n
          putStrLn ("R>>\tCurrent State: " ++ show state ++ " add: " ++ show new)
          if new == 0
            then countPosition rest new (count+1)
            else countPosition rest new count

    ('L':amount) ->
      case parse amount :: Maybe Int of 
        Nothing -> return count
        Just number -> do
          let n = state-number
          let new = recrAdd n
          putStrLn ("L>>\tCurrent State: " ++ show state ++ " minus: " ++ show new)
          if new == 0
            then countPosition rest new (count+1)
            else countPosition rest new count

    _ -> countPosition rest state count


recrMinus :: Int -> Int
recrMinus value = 
  if value <= 99 && value >= 0
  then value 
  else recrMinus (value-100)


recrAdd :: Int -> Int
recrAdd value = do
  if value >= 0 && value <= 99
  then value
  else recrAdd (value+100)

