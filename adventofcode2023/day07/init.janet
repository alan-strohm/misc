(import judge)
(use ../util)

(def test-input `
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
`)

(def real-input (slurp "./day07/input.txt"))

(def peg '(some (group (* ':w+ :s+ (number :d+) :s*))))

(judge/test (peg/match peg test-input)
  @[@["32T3K" 765]
    @["T55J5" 684]
    @["KK677" 28]
    @["KTJJT" 220]
    @["QQQJA" 483]])

(def card-strength (tabseq [[c i] :pairs "23456789TJQKA"] i c))

(defn hand-type [hand]
  (match (-> hand frequencies values sort)
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

(defn rank-plays [plays &opt type-transform]
  (default type-transform identity)
  (defn score-hand [[hand _]]
    (def hand-type (hand-type (type-transform hand)))
    (tuple hand-type  ;(map card-strength hand)))
  (sort-by score-hand plays))

(judge/test (rank-plays (peg/match peg test-input))
  @[@["32T3K" 765]
    @["KTJJT" 220]
    @["KK677" 28]
    @["T55J5" 684]
    @["QQQJA" 483]])

(defn score [plays] (sum-loop [[rank [_ bid]] :pairs plays]
                              (* (inc rank) bid)))

(defn part1 [input] (-> (peg/match peg input) rank-plays score))

(judge/test (part1 test-input) 6440)
(judge/test (part1 real-input) 251106089)

(put card-strength (chr "J") -1)

(defn resolve-jokers [hand]
  (if-let [without-jokers (filter |(not= $ (chr "J")) hand)
           mcc (last (sort-by (frequencies without-jokers) without-jokers))
           new-hand (string/replace-all "J" (string/from-bytes mcc) hand)]
    new-hand
    hand)) # mcc-byte is null because the hand is all jokers.

(judge/test (resolve-jokers "KTJJT") "KTTTT")
(judge/test (resolve-jokers "JJJJJ") "JJJJJ")

(defn part2 [input] (-> (peg/match peg input) (rank-plays resolve-jokers) score))

(judge/test (part2 test-input) 5905)
(judge/test (part2 real-input) 249620106)
