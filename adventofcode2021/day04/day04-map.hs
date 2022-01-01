import qualified Data.Map as Map
import Data.List(find)
import Data.List.Split(splitOn, wordsBy)
import Debug.Trace
import Test.Hspec


data Cell = Cell { marked :: Bool, row, col :: Int } deriving (Show)

data Board = Board { idx :: Map.Map Int Cell, rowsMarked, colsMarked :: Map.Map Int Int, won :: Bool } deriving (Show)

data Input = Input { nums :: [Int], boards :: [Board] } deriving (Show)

fromRows :: [[Int]] -> Board
fromRows rs = (Board idx' Map.empty Map.empty False)
  where idx' = fst $ foldl addRow (Map.empty, 0) rs
        addRow (b, ri) cs = (fst $ foldl (insert' ri) (b, 0) cs, ri+1)
        insert' ri (b, ci) num = (Map.insert num (Cell False ri ci) b, ci+1)

parse :: String -> Input
parse s = (Input nums boards)
  where ls = lines s
        nums = map read $ splitOn "," $ head $ ls
        boards = map fromRows $ wordsBy ([]==) $ map (map read) $ map words $ tail ls

mark :: Int -> Board -> Board
mark n b = try c
  where (c, idx') = Map.updateLookupWithKey (\_ c -> Just c { marked=True }) n $ idx b
        incMark m n = Map.insertLookupWithKey (\_ _ o -> o+1) n 1 m
        try (Just c) = (Board idx' rowNext colNext $ won rowPrev colPrev)
          where (rowPrev, rowNext) = incMark (rowsMarked b) (row c)
                (colPrev, colNext) = incMark (colsMarked b) (col c)
                won (Just 4) _ = True
                won _ (Just 4) = True
                won _ _ = False
        try Nothing = b

score :: Board -> Int
score b = sum [ k | (k, c) <- Map.toList $ idx b, not $ marked c ]

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
