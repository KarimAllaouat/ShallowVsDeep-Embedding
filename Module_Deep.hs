module Module_Deep where

import Data.Bits
import System.Environment
import Timer

data Module = Switch Int
             | LogicUnit Module Module Module
             | ArithmeticUnit Module Module Module
             | Register Module Module Int
             | Multiplexer Module Module Module Module Module
             | Clock Int
             deriving Show

-- Evaluate a module after n steps.
evaluate :: Module -> Int -> Int
evaluate (Switch i) n = mod i 16
evaluate (LogicUnit c i1 i2) n
                    | mod (evaluate c n) 4 == 0 = (15 - (mod (evaluate i1 n) 16))
                    | mod (evaluate c n) 4 == 1 = ((evaluate i1 n) .&. (evaluate i2 n))
                    | mod (evaluate c n) 4 == 2 = ((evaluate i1 n) .|. (evaluate i2 n))
                    | mod (evaluate c n) 4 == 3 = (xor (evaluate i1 n) (evaluate i2 n))
evaluate (ArithmeticUnit c i1 i2) n
                    | mod (evaluate c n) 4 == 0 = ((evaluate i1 n) + (evaluate i2 n))
                    | mod (evaluate c n) 4 == 1 = (evaluate i1 n)
                    | mod (evaluate c n) 4 == 2 = ((evaluate i1 n) - (evaluate i2 n))
                    | mod (evaluate c n) 4 == 3 = ((evaluate i1 n) - (evaluate i2 n))
evaluate (Register c i mem) n
                    | (evaluate c n) == 0 = mod mem 16
                    | (evaluate c n) == 1 = (evaluate i n)
                    | otherwise           = 0
evaluate (Multiplexer c i1 i2 i3 i4) n
                    | mod (evaluate c n) 4 == 0 = (evaluate i1 n)
                    | mod (evaluate c n) 4 == 1 = (evaluate i2 n)
                    | mod (evaluate c n) 4 == 2 = (evaluate i3 n)
                    | mod (evaluate c n) 4 == 3 = (evaluate i4 n)
evaluate (Clock t) n
                    | mod t 2 == 0 = mod n 2
                    | otherwise    = 1 - (mod n 2)

evaluateAfter :: (Int -> Module) -> Int -> [Int]
evaluateAfter f 0 = [evaluate (f 0) 0]
evaluateAfter f n = (evaluateAfter f (n-1) ++ [evaluate (f n) n])

trafficLights :: Int -> Module
trafficLights n = do
            let i1 = Switch 1
                i2 = Switch 3
                i3 = Switch 4
                i4 = Switch 2
                i5 = Switch 1
                i6 = Switch 0
                r1 = Register (Clock 0) au n
                r2 = Register (Clock 1) r1 n
                au = ArithmeticUnit i5 r2 i6
                mux = Multiplexer r1 i1 i2 i3 i4
                in mux

deep :: Int -> IO ()
deep n = do
      appendFile "Deep.txt" ((show n) ++ " ")
      (time (last (evaluateAfter trafficLights n)) "Deep.txt")
