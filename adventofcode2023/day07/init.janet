(import judge)

(def peg
  (peg/compile '(some (group (* ':w+ :s+ (number :d+) (? :s+))))))

(def example `
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
`)

(judge/test (peg/match peg example)
  @[@["32T3K" 765]
    @["T55J5" 684]
    @["KK677" 28]
    @["KTJJT" 220]
    @["QQQJA" 483]])

(def order "23456789TJQKA")
(def order-map (zipcoll order (range 0 (length order))))

(defn hand-type [hand]
  (match (sort (values (frequencies hand)))
    [5] 6
    [1 4] 5
    [2 3] 4
    [1 1 3] 3
    [1 2 2] 2
    [1 1 1 2] 1
    [1 1 1 1 1] 0
    (errorf "unexpected hand %s" hand)
    ))

(judge/test (hand-type "AAAAA") 6)
(judge/test (hand-type "TJQKA") 0)
(judge/test (hand-type "TJQK9") 0)

(defn score-hand [hand]
  (tuple (hand-type hand) ;(map order-map hand)))

(judge/test (score-hand "AAAAA") [6 12 12 12 12 12])

(defn rank-hands [hands]
  (sort-by |(score-hand (get $ 0)) hands))

(judge/test (rank-hands (peg/match peg example))
  @[@["32T3K" 765]
    @["KTJJT" 220]
    @["KK677" 28]
    @["T55J5" 684]
    @["QQQJA" 483]])

(defn part1 [input]
  (let [ranked (rank-hands (peg/match peg input))
        winning (fn [entry rank]  (* (+ rank 1) (get entry 1)))
        winnings (map winning ranked (range 0 (length ranked)))]
    (sum winnings)))

(judge/test (part1 example) 6440)
(judge/test (part1 (slurp "./day07/input.txt")) 251106089)


