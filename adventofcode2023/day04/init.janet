(import judge)

(use ../util)

(def test-input `
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
`)

(def real-input (slurp "./day04/input.txt"))

(def peg ~{:main (some (group :line))
           :line (* :card :number-list "|" :number-list)
           :card (* "Card" :s+ :d+ ":")
           :number-list (group (some (+ :s+ (number :d+))))
           })

(judge/test (peg/match peg test-input)
  @[@[@[41 48 83 86 17]
      @[83 86 6 31 17 9 48 53]]
    @[@[13 32 20 16 61]
      @[61 30 68 82 17 32 24 19]]
    @[@[1 21 53 59 44]
      @[69 82 63 72 16 21 14 1]]
    @[@[41 92 73 84 69]
      @[59 84 76 51 58 5 54 83]]
    @[@[87 83 26 28 32]
      @[88 30 70 12 93 22 82 36]]
    @[@[31 18 13 56 72]
      @[74 77 10 23 35 67 36 11]]])

(defn make-set [a]
  (reduce |(put $0 $1 true) @{} a))

(judge/test (make-set [1 2 3 4]) @{1 true 2 true 3 true 4 true})

(defn num-winning [winning have]
  (length (filter (make-set winning) have)))

(judge/test (num-winning [83 86 6 31 17 9 48 53] [41 48 83 86 17]) 4)

(defn points [num-winning]
  (if
    (zero? num-winning) 0
    (math/exp2 (dec num-winning))))

(judge/test (points 0) 0)
(judge/test (points 1) 1)
(judge/test (points 2) 2)
(judge/test (points 3) 4)

(defn part1 [str]
  (sum-loop [[winning have] :in (peg/match peg str)]
            (points (num-winning winning have))))

(judge/test (part1 test-input) 13)
(judge/test (part1 real-input) 28538)

(defn part2 [str]
  (def cards (peg/match peg str))
  (def copies (seq [_ :in cards] 1))
  (sum-loop [[i [winning have]] :pairs cards
             :let [cnt (in copies i)]]
            (loop [offset :range-to [1 (num-winning winning have)]]
              (update copies (+ i offset) + cnt))
            cnt))

(judge/test (part2 test-input) 30)
(judge/test (part2 real-input) 9425061)
