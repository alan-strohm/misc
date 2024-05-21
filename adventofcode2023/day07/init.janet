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

(def order-map (tabseq [[c i] :pairs "23456789TJQKA"] i c))

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

(defn rank-hands [hands &opt type-transform]
  (default type-transform identity)
  (defn score-hand [[hand _]]
    (tuple (hand-type (type-transform hand)) ;(map order-map hand)))
  (sort-by score-hand hands))

(judge/test (rank-hands (peg/match peg test-input))
  @[@["32T3K" 765]
    @["KTJJT" 220]
    @["KK677" 28]
    @["T55J5" 684]
    @["QQQJA" 483]])

(defn score [hands] (sum-loop [[rank [_ bid]] :pairs hands]
                              (* (inc rank) bid)))

(defn part1 [input] (-> (peg/match peg input) (rank-hands) (score)))

(judge/test (part1 test-input) 6440)
(judge/test (part1 real-input) 251106089)

(put order-map (chr "J") -1)

(defn resolve-jokers [hand]
  (if-let [no-jokers (string/replace-all "J" "" hand)
           mcc-byte (last (sort-by (frequencies no-jokers) (buffer no-jokers)))
           new-hand (string/replace-all "J" (string/from-bytes mcc-byte) hand)]
    new-hand
    hand)) # mcc-byte is null because the hand is all jokers.

(judge/test (resolve-jokers "KTJJT") "KTTTT")
(judge/test (resolve-jokers "JJJJJ") "JJJJJ")

(defn part2 [input] (-> (peg/match peg input) (rank-hands resolve-jokers) (score)))

(judge/test (part2 test-input) 5905)
(judge/test (part2 real-input) 249620106)
