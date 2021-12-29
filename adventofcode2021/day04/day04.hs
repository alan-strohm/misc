import Data.List(transpose, find)
import Data.List.Split(splitOn, wordsBy)
import Debug.Trace
import Test.Hspec

data Board = Board { rows :: [[Int]] } deriving (Show)
data Input = Input { nums :: [Int], boards :: [Board] } deriving (Show)

parse :: String -> Input
parse s = (Input nums boards)
  where ls = lines s
        nums = map read $ splitOn "," $ head ls
        boards = map Board $ wordsBy ([]==) $ map (map read) $ map words $ tail ls

mark :: Int -> Board -> Board
mark n = Board . (map (map (repl n))) . rows
  where repl x y = if x == y then -1 else y

won :: Board -> Bool
won b = (any (all (-1==)) $ rows b) || (any (all (-1==)) $ transpose $ rows b)

score :: Board -> Int
score = sum . map (sum . filter (/= -1)) . rows

callNum :: Input -> Input
callNum i = Input{ nums = tail $ nums i, boards = map (mark $ head $ nums i) $ boards i } 

play :: (Input -> Maybe Board) -> Input -> Int
play done i = try $ done next
  where num = head $ nums i
        next = callNum i
        try (Just winner) = num * (score winner)
        try Nothing = play done next{ boards = filter (not . won) $ boards next}

part1 :: String -> Int
part1 = play (find won . boards) . parse

lastWin :: Input -> Maybe Board
lastWin (Input _ [b])
  | won b = Just b
  | otherwise = Nothing
lastWin _ = Nothing

part2 :: String -> Int
part2 = play lastWin . parse

run :: (String -> Int) -> FilePath -> IO Int
run f fname = do
  content <- readFile fname
  return $ f content

main :: IO ()
main = hspec $ do
  describe "day4" $ do
    it "part1 sample" $ do
      run part1 "sample.txt" `shouldReturn` 4512
    it "part1 input" $ do
      run part1 "input.txt" `shouldReturn` 22680
    it "part2 sample" $ do
      run part2 "sample.txt" `shouldReturn` 1924
    it "part2 input" $ do
      run part2 "input.txt" `shouldReturn` 16168
