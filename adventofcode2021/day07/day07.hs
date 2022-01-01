import Data.List.Split(splitOn)
import Test.Hspec

cost1 :: [Int] -> Int -> Int
cost1 cs to = sum $ map (abs . (to-)) cs

minCost :: ([Int] -> Int -> Int) -> [Int] -> Int
minCost cost cs = minimum $ map (cost cs) [minimum(cs)..maximum(cs)]

cost2 :: [Int] -> Int -> Int
cost2 cs to = sum $ map (cost to) cs
  where cost to from = diff * (diff + 1) `div` 2
          where diff = abs $ to - from

part1 :: String -> Int
part1 = minCost cost1 . map read . splitOn ","

part2 :: String -> Int
part2 = minCost cost2 . map read . splitOn ","

run :: (String -> Int) -> FilePath -> IO Int
run f fname = do
  content <- readFile fname
  return $ f content

main :: IO ()
main = hspec $ do
  describe "day7" $ do
    it "part1 sample" $ do
      run part1 "sample.txt" `shouldReturn` 37
    it "part1 input" $ do
      run part1 "input.txt" `shouldReturn` 355521
    it "part2 sample" $ do
      run part2 "sample.txt" `shouldReturn` 168
    it "part2 input" $ do
      run part2 "input.txt" `shouldReturn` 100148777
