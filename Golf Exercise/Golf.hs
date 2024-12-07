module Golf where

skips :: [a] -> [[a]]
skips xs = [nthElements n xs | n <- [1..length xs]]
  where
    nthElements n ys = [y | (y, i) <- zip ys [1..], i `mod` n == 0]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | y > x && y > z = y : localMaxima (y:z:xs)
  | otherwise      = localMaxima (y:z:xs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram [] = []
histogram xs = unlines (reverse rows ++ [footer, indices])
  where
    counts = map (\n -> length (filter (== n) xs)) [0..9]
    maxHeight = maximum counts
    rows = [[if count >= level then '*' else ' ' | count <- counts] | level <- [1..maxHeight]]
    footer = replicate 10 '='
    indices = "0123456789"

-- " * * \n==========\n0123456789\n"