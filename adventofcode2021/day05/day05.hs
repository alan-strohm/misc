import Data.List(group, sort)
import Data.List.Split(splitOn)
import Debug.Trace(traceShowId)
import Test.Hspec

data Line = Line { first, last :: (Int, Int) }

numOverlap :: [Line] -> Int
numOverlap = length . filter (not . (==1) . length) .  group . sort . concat . map points

points :: Line -> [(Int, Int)]
points l@(Line (x1, y1) (x2, y2)) = zip xs ys
  where len = 1 + (max (abs $ x1 - x2) (abs $ y1 - y2))
        range a b = take len $ iterate (+inc) a
          where inc = signum $ b - a
        xs = range x1 x2
        ys = range y1 y2

diag :: Line -> Bool
diag (Line (x1, y1) (x2, y2)) = (abs $ x1 - x2) == (abs $ y1 - y2)

parseLine :: String -> Line
parseLine = lineFrom . map (map read . splitOn ",") . splitOn " -> "
  where lineFrom [[x1, y1], [x2, y2]] = Line (x1, y1) (x2, y2)

parse :: String -> [Line]
parse = map parseLine . lines

part1 :: String -> Int
part1 = numOverlap . filter (not . diag) . parse

part2 :: String -> Int
part2 = numOverlap . parse

run :: (String -> Int) -> FilePath -> IO Int
run f fname = do
  content <- readFile fname
  return $ f content

main :: IO ()
main = hspec $ do
  describe "day5" $ do
    it "part1 sample" $ do
      run part1 "sample.txt" `shouldReturn` 5
    it "part1 input" $ do
      run part1 "input.txt" `shouldReturn` 6564
    it "part2 sample" $ do
      run part2 "sample.txt" `shouldReturn` 12
    it "part2 input" $ do
      run part2 "input.txt" `shouldReturn` 19172
