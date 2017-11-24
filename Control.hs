import Module_Deep
import Module_Shallow

import System.Environment

list :: Int -> [Int]
list n = map (*1000) [1..n]

compute :: (Int -> IO ()) -> Int -> IO ()
compute f n = do (sequence_ (map f (list n)))

main :: IO ()
main = do
  args <- getArgs
  let n = (read (unwords (args))) :: Int
  writeFile "Shallow.txt" ""
  writeFile "Deep.txt" ""
  compute deep n
  compute shallow n
