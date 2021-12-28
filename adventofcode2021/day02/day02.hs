import Test.Hspec

data Sub = Sub { p, d, a :: Int } deriving (Show)

go :: Int -> Sub -> (String, Int) -> Sub
go 1 s ("forward", x) = s { p = (p s) + x }
go 2 s ("forward", x) = s { p = (p s) + x, d = (d s) + x * (a s) }
go 1 s ("up", x) = s { d = (d s) - x }
go 2 s ("up", x) = s { a = (a s) - x }
go 1 s ("down", x) = s { d = (d s) + x }
go 2 s ("down", x) = s { a = (a s) + x }

parse :: String -> [(String, Int)]
parse s = [ (x, read y::Int) | [x, y] <-  map words (lines s) ]

part :: Int -> String -> Int
part n = res . foldl (go n) (Sub 0 0 0) . parse
  where res s = (p s) * (d s)

run :: (String -> Int) -> FilePath -> IO Int
run f fname = do
  content <- readFile fname
  return $ f content

main :: IO ()
main = hspec $ do
  describe "day1" $ do
    it "part1 sample" $ do
      run (part 1) "sample.txt" `shouldReturn` 150
    it "(part 1) input" $ do
      run (part 1) "input.txt" `shouldReturn` 1524750
    it "(part 2) sample" $ do
      run (part 2) "sample.txt" `shouldReturn` 900
    it "(part 2) input" $ do
      run (part 2) "input.txt" `shouldReturn` 1592426537
