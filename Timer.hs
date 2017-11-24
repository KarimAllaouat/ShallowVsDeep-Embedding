module Timer where
-- Code from https://wiki.haskell.org/Timing_computations
import Text.Printf
import Control.Exception
import System.CPUTime
import Control.Parallel.Strategies
import Control.Monad
import Control.DeepSeq
import System.Environment

lim :: Int
lim = 10^6

time :: (Num a, NFData a) => a -> String -> IO ()
time y f = do
    start <- getCPUTime
    replicateM_ lim $ do
        x <- evaluate (1 + y)
        rnf x `seq` return ()
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    appendFile f (show (diff :: Double) ++ "\n")
    return ()
