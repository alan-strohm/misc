(import judge)

(use ./util)

(def real-input (slurp "./input/03.txt"))
(def test-input1 `
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
`)

(def mul '{:main (* "mul(" :num "," :num ")")
           :num (number (between 1 3 :d))})

(def part1 ~(any (+ (group ,mul) 1)))

(judge/test (peg/match part1 test-input1) @[@[2 4] @[5 5] @[11 8] @[8 5]])
    
(defn solve [peg str]
  (sum-loop [mul :in (peg/match peg str)]
            (* ;mul)))

(judge/test (solve part1 test-input1) 161)
(judge/test (solve part1 real-input) 196826776)

(def part2 ~{:main (any (+ :dont (group ,mul) 1))
             :dont (* "don't()" (thru (+ -1 "do()")))})

(def test-input2 `
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
`)

(judge/test (peg/match part2 test-input2) @[@[2 4] @[8 5]])

(judge/test (solve part2 test-input2) 48)
(judge/test (solve part2 real-input) 106780429)
