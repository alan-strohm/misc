(import judge)
(use ../util)

(def test-input `
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
`)

(def real-input (string/trim (slurp "./day12/input.txt")))

(def peg '{:main (split "\n" (group :line))
           :line (* ':S+ " " (group :nums))
           :nums (split "," (number :d+))})

(judge/test (peg/match peg "???.### 1,1,3") @[@["???.###" @[1 1 3]]])
(judge/test (peg/match peg test-input)
  @[@["???.###" @[1 1 3]]
    @[".??..??...?##." @[1 1 3]]
    @["?#?#?#?#?#?#?#?" @[1 3 1 6]]
    @["????.#...#..." @[4 1 1]]
    @["????.######..#####." @[1 6 5]]
    @["?###????????" @[3 2 1]]])

(defn to-linked-list [iterable]
  (def arrtup (cond
                (string? iterable) (map string/from-bytes iterable)
                iterable))
  (match arrtup
    [first & rest] [first (to-linked-list rest)]
    [] []))

(judge/test (to-linked-list [1 2 3]) [1 [2 [3 []]]])
(judge/test (to-linked-list "abc") ["a" ["b" ["c" []]]])

(defn consume-group [str len]
  (match [str len]
    [["#" _] 0] nil
    [[first rest] 0] rest
    [["." rest] _] nil
    [[first rest] len] (consume-group rest (dec len))
    [[] 0] []
    [[] _] nil))

(judge/test (consume-group (to-linked-list "#") 1) [])
(judge/test (consume-group (to-linked-list ".") 1) nil)
(judge/test (consume-group (to-linked-list "##") 1) nil)
(judge/test (consume-group (to-linked-list "#.") 1) [])
(judge/test (consume-group (to-linked-list "##") 3) nil)
(judge/test (consume-group (to-linked-list "###?") 3) [])
(judge/test (consume-group (to-linked-list "###?.") 3) ["." []])

(defn options* [str lens memo]
  (when-let [result (in memo [str lens])]
    (break result))
  (def result
    (match str
      ["." rest] (options* rest lens memo)
      ["#" rest] (if-let [len (first lens)
                          remaining (consume-group str len)]
                   (options* remaining (last lens) memo)
                   0)
      ["?" rest] (+ (options* ["." rest] lens memo)
                    (options* ["#" rest] lens memo))
      [] (if (empty? lens) 1 0)
      (assert false)
      ))
  (put memo [str lens] result)
  result)

(defn options [str lens] (options* (to-linked-list str) (to-linked-list lens) @{}))

(judge/test (options "#" [1]) 1)
(judge/test (options "?" [1]) 1)
(judge/test (options "???.###" [1 1 3]) 1)
(judge/test (options ".??..??...?##." [1 1 3]) 4)
(judge/test (options "?###????????" [3 2 1]) 10)
(judge/test (options "?#?#?#?#?#?#?#?" [1 3 1 6]) 1)
(judge/test (options "????.######..#####." [1 6 5]) 4)

(defn part1 [str]
  (sum-loop [line :in (peg/match peg str)]
            (options ;line)))

(judge/test (part1 test-input) 21)
(judge/test (part1 real-input) 7792)

(defn expand [str lens]
  [(string/join (seq [:repeat 5] str) "?")
   (flatten (seq [:repeat 5] lens))])

(judge/test (expand ".#" [1]) [".#?.#?.#?.#?.#" @[1 1 1 1 1]])

(judge/test (options ;(expand "????.######..#####." [1 6 5])) 2500)
(judge/test (options ;(expand "?###????????" [3 2 1])) 506250)

(defn part2 [str]
  (sum-loop [line :in (peg/match peg str)]
            (options ;(expand ;line))))

(judge/test (part2 test-input) 525152)
(judge/test (part2 real-input) 13012052341533)
