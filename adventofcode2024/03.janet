(import judge)

(use ./util)

(def real-input (slurp "./input/03.txt"))
(def test-input `
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
`)

(defn read [str]
  (def peg ~{:main (any (+ (group :mul) 1))
             :mul (* "mul(" :num "," :num ")")
             :num (number (between 1 3 :d))})
  (peg/match peg str))

(judge/test (read test-input) @[@[2 4] @[5 5] @[11 8] @[8 5]])
    
(defn part1 [str]
  (sum-loop [mul :in (read str)]
            (* ;mul)))

(judge/test (part1 test-input) 161)
(judge/test (part1 real-input) 196826776)
