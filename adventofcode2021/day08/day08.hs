import Data.List.Split(splitOn)
import Data.List(elemIndex, intercalate, permutations, sort, sortOn)
import Data.Maybe(fromMaybe)
import Debug.Trace(traceShowId)
import Test.Hspec

segs = "abcdefg"

digits = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

makeKey :: [String] -> String
makeKey = intercalate " " . sort . map sort

buildMappings :: [[(Char, Char)]] -> [(String, [(Char, Char)])]
buildMappings ms = sortOn fst $ zip (map getKey ms) $ map (sortOn fst . map flipPair) ms
  where getKey m = makeKey $ map (apply m) digits
        flipPair (x,y) = (y,x)

mappings = buildMappings $ map (zip segs) $ permutations segs

merge :: (Ord a, Show a) => [(a, b)] -> [(a, b -> c)] -> [c]
merge [] _ = []
merge _ [] = []
merge axs@((xa, b):xs) ays@((ya, f):ys) 
  | xa == ya = f b : merge axs ys -- there could be duplicate ys
  | xa < ya = merge xs ays 
  | xa > ya = merge axs ys

apply :: [(Char, Char)] -> String -> String
apply m s = sort $ merge m $ zip (sort s) $ repeat id

parse :: String -> [[Maybe Int]]
parse = merge mappings . sortOn fst . map parseOne . map (splitOn " | ") . lines
  where parseOne [l, r] = (makeKey $ words l, mkConvert r)
        lookup s = elemIndex (sort s) digits
        mkConvert s = \m -> map (lookup . apply m) $ words s

part1 :: String -> Int
part1 = foldl1 (+) . map (length . filter ((flip elem) $ map Just [1, 4, 7, 8])) . parse

part2 :: String -> Int
part2 = fromMaybe 0 . (sum <$>) . sequence . map ((fromDigits <$>) . sequence) . parse
  where fromDigits = foldl1 ((+) . (10*))

run :: (String -> Int) -> FilePath -> IO Int
run f fname = do
  content <- readFile fname
  return $ f content

main :: IO ()
main = hspec $ do
  describe "day8" $ do
    it "part1 sample" $ do
      run part1 "sample.txt" `shouldReturn` 26
    it "part1 input" $ do
      run part1 "input.txt" `shouldReturn` 525
    it "part2 sample" $ do
      run part2 "sample.txt" `shouldReturn` 61229
    it "part2 input" $ do
      run part2 "input.txt" `shouldReturn` 1083859
