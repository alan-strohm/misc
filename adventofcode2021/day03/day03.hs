import Data.Bits(xor)
import Data.Char(digitToInt)
import Data.List(transpose)
import Debug.Trace
import Test.Hspec

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

cntOnes :: Int -> Int -> Int
cntOnes x 1 = x+1
cntOnes x 0 = x-1

toDec :: [Int] -> Int
toDec = foldl ((+) . (2*)) 0

part1 :: String -> Int
part1 s = gamma * epsilon
  where cnts = map (foldl cntOnes 0) $ transpose $ parse s
        gamma = toDec $ map (fromEnum . (>=0)) cnts
        epsilon = toDec $ map (fromEnum . (<0)) cnts

findRating :: (Int -> Bool) -> [[Int]] -> [Int]
findRating pred [x] = x
findRating pred is = want : (findRating pred $ map tail $ filter ((want==) . head) is)
  where want = fromEnum $ pred $ foldl cntOnes 0 $ head $ transpose is

part2 :: String -> Int
part2 s = oo * co2
  where is = parse s
        oo = toDec $ findRating (>=0) is
        co2 = toDec $ findRating (<0) is

run :: (String -> Int) -> FilePath -> IO Int
run f fname = do
  content <- readFile fname
  return $ f content

main :: IO ()
main = hspec $ do
  describe "day3" $ do
    it "part1 sample" $ do
      run part1 "sample.txt" `shouldReturn` 198
    it "part1 input" $ do
      run part1 "input.txt" `shouldReturn` 3901196
    it "part2 sample" $ do
      run part2 "sample.txt" `shouldReturn` 230
    it "part2 input" $ do
      run part2 "input.txt" `shouldReturn` 4412188
