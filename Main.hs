module Main where
{- |
   Conway's game of life written in Haskell. From random 1's and 0's
-}
import Life
import qualified Data.Vector as V
import System.Environment(getArgs)
import System.Random

main :: IO()
main = do
  g <- newStdGen
  grid <- getVector g
  putStrLn $ show grid


getVector:: StdGen -> IO (V.Vector (V.Vector Life))
getVector gen = return $ V.generate 10  (\g -> V.fromList . take 10 $ (randoms gen :: [Life]))

