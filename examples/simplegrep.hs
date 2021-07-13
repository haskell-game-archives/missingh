import MissingH.List

main :: IO ()
main = do
  c <- getContents
  putStr $ unlines $ filter (contains "Haskell") $ lines c
