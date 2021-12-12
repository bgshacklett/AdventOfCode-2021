
stackDepths :: [Integer] -> (Integer, Integer)
stackDepths xs = foldl step (9999, 0) xs
  where
    step (last, count) n = (n, (if n > last then count + 1 else count))

main :: IO ()
main = do
  x <- readFile "input.txt"

  let depths = map (\n -> read n) (words x)
  let resp = snd (stackDepths depths)
  putStrLn $ show resp
