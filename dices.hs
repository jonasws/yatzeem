import System.Random
import Data.List

main :: IO ()
main = do
  gen <- getStdGen
  print $ throwDices (dices gen) 1

dices :: (RandomGen a) => a -> [Int]
dices = randomRs (1, 6)

isYatzee :: [Int] -> Bool
isYatzee = (== 1) . length . group . sort

throwDices :: [Int] -> Int -> Int
throwDices ds n
  | isYatzee six = n
  | otherwise = throwDices rest n + 1
  where (six, rest) = splitAt 6 ds
