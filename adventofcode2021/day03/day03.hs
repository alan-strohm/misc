import Data.Bits(xor)
import Data.Char(digitToInt)
import Debug.Trace
import Test.Hspec

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

cntOnes :: Int -> Int -> Int
cntOnes x 1 = x+1
cntOnes x 0 = x-1

accOnes :: [Int] -> [Int] -> [Int]
accOnes [] [] = []
accOnes (x:restx) (y:resty) = cntOnes x y : ( accOnes restx resty )

toDec :: [Int] -> Int
toDec = foldl (\acc x -> 2*acc+x) 0

part1 :: String -> Int
part1 s =
  let is = parse s
      len = length $ head is
      cnts = foldl accOnes (replicate len 0) is
      bin = map (fromEnum . (>0)) cnts
  in (toDec bin) * (toDec $ map (xor 1) bin)

findRating :: (Int -> Bool) -> [[Int]] -> [Int]
findRating pred [x] = x
findRating pred is =
  let want = fromEnum $ pred $ foldl cntOnes 0 $ map head is
   in want : (findRating pred $ map tail $ filter ((want==) . head) is)

part2 :: String -> Int
part2 s =
  let is = parse s
      oo = toDec $ findRating (>=0) is
      co2 = toDec $ findRating (<0) is
  in oo * co2

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
