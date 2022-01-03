import Data.List.Split(splitOn)
import Data.List(elemIndex, intercalate, permutations, sort, sortOn)
import Data.Maybe(fromMaybe)
import Debug.Trace(traceShowId)
import Test.Hspec

segs = "abcdefg"

digits = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

-- Merge two lists of pairs which are both sorted on fst.  The second list contains a function which is called on the snd of the first list to produce the resulting list.
merge :: (Ord a, Show a) => [(a, b)] -> [(a, b -> c)] -> [c]
merge [] _ = []
merge _ [] = []
merge axs@((xa, b):xs) ays@((ya, f):ys) 
  | xa == ya = f b : merge axs ys -- the next ya could be a duplicate.
  | xa < ya = merge xs ays 
  | xa > ya = merge axs ys


-- Apply a character mapping to string.  Assumes the mapping is sorted by fst.
-- Sorts the input and the output.
apply :: [(Char, Char)] -> String -> String
apply m s = sort $ merge m $ zip (sort s) $ repeat id

makeKey :: [String] -> String
makeKey = intercalate " " . sort . map sort

buildMappings :: [[(Char, Char)]] -> [(String, [(Char, Char)])]
buildMappings ms = sortOn fst $ zip (map getKey ms) $ map (sortOn fst . map flipPair) ms
  where getKey m = makeKey $ map (apply m) digits
        flipPair (x,y) = (y,x)

-- One mapping for each permutation.
-- fst = the mapping applied to all 10 digits with the results sorted (each
--       digit is sorted, the list of digits is sorted and the total set of
--       mappings is sorted by fst).  The format is produced by makeKey.
-- snd = A reverse mapping to convert strings with this permutation back to the
--       canonical form.
mappings = buildMappings $ map (zip segs) $ permutations segs

parse :: String -> [[Int]]
parse = fromMaybe [] . sequence . merge mappings . prepMerge
  where prepMerge = sortOn fst . map parseOne . map (splitOn " | ") . lines
        parseOne [l, r] = (makeKey $ words l, mkConvert r)
        lookup s = elemIndex (sort s) digits
        mkConvert s = \m -> sequence $ map (lookup . apply m) $ words s

part1 :: String -> Int
part1 = foldl1 (+) . map (length . filter ((flip elem) [1, 4, 7, 8])) . parse

part2 :: String -> Int
part2 = sum . map (foldl1 ((+) . (10*))) . parse

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
