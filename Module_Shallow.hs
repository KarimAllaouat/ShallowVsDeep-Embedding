module Module_Shallow where

import Data.Bits
import System.Environment
import Timer

switch :: Int -> Int
switch i = i

logicUnit :: Int -> Int -> Int -> Int
logicUnit c i1 i2
    | mod c 4 == 0 = (15 - (mod i1 16))
    | mod c 4 == 1 = (i1 .&. i2)
    | mod c 4 == 2 = (i1 .|. i2)
    | mod c 4 == 3 = (xor i1 i2)

arithmeticUnit :: Int -> Int -> Int -> Int
arithmeticUnit c i1 i2
    | mod c 4 == 0 = i1 + i2
    | mod c 4 == 1 = i1
    | mod c 4 == 2 = i1 - i2
    | mod c 4 == 3 = i1 - i2

register :: Int -> Int -> Int -> Int
register c i mem
    | c == 0    = mod mem 16
    | c == 1    = i
    | otherwise = 0

multiplexer :: Int -> Int -> Int -> Int -> Int -> Int
multiplexer c i1 i2 i3 i4
    | mod c 4 == 0 = i1
    | mod c 4 == 1 = i2
    | mod c 4 == 2 = i3
    | mod c 4 == 3 = i4

clock :: Int -> Int
clock t
    | mod t 2 == 0 = 0
    | otherwise    = 1

trafficLights :: Int -> Int
trafficLights n = do
            let i1 = switch 1
                i2 = switch 3
                i3 = switch 4
                i4 = switch 2
                i5 = switch 1
                i6 = switch 0
                r1 = register (clock 0) au n
                r2 = register (clock 1) r1 n
                au = arithmeticUnit i5 r2 i6
                mux = multiplexer r1 i1 i2 i3 i4
                in mux

evaluateAfter :: (Int -> Int) -> Int -> [Int]
evaluateAfter f 0 = [f 0]
evaluateAfter f n = ((evaluateAfter f (n-1)) ++ [f n])

shallow :: Int -> IO ()
shallow n = do
      appendFile "Shallow.txt" ((show n) ++ " ")
      (time (last (evaluateAfter trafficLights n)) "Shallow.txt")
