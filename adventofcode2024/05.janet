(import judge)

(use ./util)

(def test-input `
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47`)

(def real-input (slurp "./input/05.txt"))

(defn read [str]
  (def peg ~{:main (* (group (some :pair)) "\n" (group :updates))
             :pair (group (* (number :d+) "|" (number :d+) "\n"))
             :updates (split "\n" (+ -1 :update))
             :update (group (split "," (number :d+)))})
  (peg/match peg str))

(judge/test (read test-input)
  @[@[@[47 53]
      @[97 13]
      @[97 61]
      @[97 47]
      @[75 29]
      @[61 13]
      @[75 53]
      @[29 13]
      @[97 29]
      @[53 29]
      @[61 53]
      @[97 53]
      @[61 29]
      @[47 13]
      @[75 47]
      @[97 75]
      @[47 61]
      @[75 61]
      @[47 29]
      @[75 13]
      @[53 13]]
    @[@[75 47 61 53 29]
      @[97 61 53 29 13]
      @[75 29 13]
      @[75 97 47 61 53]
      @[61 13 29]
      @[97 13 75 29 47]]])

(defn make-cmp [orders]
  (def is-before? @{})
  (loop [[before after] :in orders]
    (put-in is-before? [before after] true))
  (fn [x y]
    (assert (or (get-in is-before? [x y])
                (get-in is-before? [y x])
                (= x y))
            (errorf "%n" [x y]))
    (get-in is-before? [x y])))

(defn part1 [str]
  (def [orders updates] (read str))
  (def cmp-fn (make-cmp orders))
  (sum-loop [u :in updates
             :let [u-sorted (sorted u cmp-fn)]
             :when (deep= u u-sorted)]
            (in u (div (length u) 2))))

(judge/test (part1 test-input) 143)
(judge/test (part1 real-input) 5747)
