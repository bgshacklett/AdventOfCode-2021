import Debug.Trace (trace)
import Data.Maybe (fromMaybe)

winLen = 3
memLen = winLen -1

aggDepths :: [Int] -> ([[Int]], [Int])
aggDepths = foldl step ([[]], [])
  where
    step :: ([[Int]], [Int]) -> Int -> ([[Int]], [Int])
    step (windows, aggregatedDepths) n
      | length(head windows) < memLen = (map (\x -> x ++ [n]) windows ++ [[]], aggregatedDepths)
      | length(head windows) == memLen = (map (\x -> x ++ [n]) windows, aggregatedDepths)
      | otherwise  = (map (\x -> x ++ [n]) (tail windows) ++ [[n]], aggregatedDepths ++ [sum (head windows)])


stackDepths :: [Int] -> (Maybe Int, Int)
stackDepths = foldl step (Nothing, 0)
  where
    step :: (Maybe Int, Int) -> Int -> (Maybe Int, Int)
    step (last, count) n
      | last == Nothing = (Just n, count + 1)
      | otherwise = (Just n, if n > fromMaybe 0 last then count + 1 else count)


main :: IO ()
main = do
  x <- readFile "input.txt"

  let depths = map (\n -> read n :: Int) (words x)
  let aggregatedDepths = snd (aggDepths depths)
  print . snd $ stackDepths aggregatedDepths
