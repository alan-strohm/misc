import Data.Function(on)
import Data.List(groupBy, sort)
import Data.List.Split(splitOn)
import Debug.Trace(traceShowId)
import Test.Hspec

data Gen = Gen { remDays, size :: Int } deriving (Show, Eq, Ord)

sumGens :: [Gen] -> Gen
sumGens = foldl1 sumGen
  where sumGen (Gen r x) (Gen _ y) = Gen r $ x+y

compact :: [Gen] -> [Gen]
compact = map sumGens . groupBy ((==) `on` remDays) . sort

next :: [Gen] -> [Gen]
next = compact . foldl nextGen []
  where nextGen acc (Gen 0 s) = (Gen 8 s) : (Gen 6 s) : acc
        nextGen acc (Gen r s) = (Gen (r-1) s) : acc

parse :: String -> [Gen]
parse = compact . map (((flip Gen) 1) . read) . splitOn ","

numAfter :: Int -> [Gen] -> Int
numAfter n gs = size $ sumGens $ foldl (const . next) gs [1..n]

part1 :: String -> Int
part1 = (numAfter 80) . parse

part2 :: String -> Int
part2 = (numAfter 256) . parse

run :: (String -> Int) -> FilePath -> IO Int
run f fname = do
  content <- readFile fname
  return $ f content

main :: IO ()
main = hspec $ do
  describe "day6" $ do
    it "part1 sample" $ do
      run part1 "sample.txt" `shouldReturn` 5934
    it "part1 input" $ do
      run part1 "input.txt" `shouldReturn` 388419
    it "part2 sample" $ do
      run part2 "sample.txt" `shouldReturn` 26984457539
    it "part2 input" $ do
      run part2 "input.txt" `shouldReturn` 1740449478328
