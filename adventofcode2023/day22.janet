(import judge)

(use ./util)

(def test-input `
1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9`)

(def real-input (slurp "./input/22.txt"))

(def peg
  ~{:main (split "\n" (+ -1 :line))
    :line (/ (split "~" :point) ,|{:lo $0 :hi $1})
    :point (/ (split "," (number :d+)) ,tuple)})

(judge/test (peg/match peg test-input)
  @[{:hi [1 2 1] :lo [1 0 1]}
    {:hi [2 0 2] :lo [0 0 2]}
    {:hi [2 2 3] :lo [0 2 3]}
    {:hi [0 2 4] :lo [0 0 4]}
    {:hi [2 2 5] :lo [2 0 5]}
    {:hi [2 1 6] :lo [0 1 6]}
    {:hi [1 1 9] :lo [1 1 8]}])

(defn overlaps? [{:lo lo1 :hi hi1} {:lo lo2 :hi hi2}]
  (defn overlaps-1d? [min1 max1 min2 max2] (and (>= max1 min2) (>= max2 min1)))
  (all true? (map overlaps-1d? lo1 hi1 lo2 hi2)))

(judge/test (overlaps? {:hi @[1 2 1] :lo @[1 0 1]} {:hi @[2 0 1] :lo @[0 0 1]}) true)
(judge/test (overlaps? {:hi @[1 2 1] :lo @[1 0 1]} {:hi @[2 2 1] :lo @[0 2 1]}) true)

(defn lower [{:lo [x1 y1 z1] :hi [x2 y2 z2]}]
  {:lo [x1 y1 (dec z1)] :hi [x2 y2 (dec z2)]})

(defn on-ground? [{:lo [_ _ z]}] (= z 1))

(defmacro tab-arr-seq [dsl keybody & value-body]
  (defn append [tab [k v]] (update tab k |(array/push (or $0 @[]) v)))
  ~(reduce ,append @{} (seq ,dsl [,keybody (do ,;value-body)])))

(judge/test (tab-arr-seq [:repeat 2] 1 2) @{1 @[2 2]})

(defn find-supports [bricks]
  # Index from dimension (:hi or :lo) -> z -> the set of bricks with that z
  # coordinate of that dimension.
  (def by-z @{})
  # Put v into the by-z index for brick i based on its current dimensions.
  (defn set-index [i v]
    (loop [[dim [_ _ z]] :pairs (in bricks i)]
      (put-in by-z [dim z i] v)))

  (map |(set-index $ true)  (keys bricks))

  (def max-lo-z (max-of (keys (by-z :lo))))

  (var supports "A list of brick IDs keyed by the brick ID which supports them." @{})
  (var falling "A list of brick IDs which are currently still falling." @{})
  (var fallen "A list of brick IDs which have fallen" @{})

  # Going from the bottom up, mark each z level as "falling" and then lower
  # each falling brick until it is supported.
  (loop [z :range-to [2 max-lo-z]
         :before (do
                   (set falling (get-in by-z [:lo z] @{}))
                   (put-in by-z [:lo z] nil))
         i :iterate (first (keys falling))
         :before (set-index i nil)
         :let [this (lower (in bricks i))]
         :after (do
                  (when (falling i) (put bricks i this) (put fallen i true))
                  (when (on-ground? this) (put falling i nil))
                  (when (not (falling i)) (set-index i true)))
         j :keys (get-in by-z [:hi (get-in this [:lo 2])] @{})
         :when (overlaps? this (in bricks j))]
    (update supports j |(array/push (or $ @[]) i))
    (put falling i nil))
  [supports (length fallen)])

(judge/test (find-supports (->> test-input (peg/match peg)))
  [@{0 @[1 2]
     1 @[3 4]
     2 @[3 4]
     3 @[5]
     4 @[5]
     5 @[6]}
   5])

(defn invert-keys [iter]
  (tab-arr-seq [[k vs] :pairs iter
                v :in vs]
               v k))

(judge/test (invert-keys (->> test-input (peg/match peg) find-supports first))
  @{1 @[0]
    2 @[0]
    3 @[1 2]
    4 @[1 2]
    5 @[4 3]
    6 @[5]})

(defn part1 [str]
  (def bricks (peg/match peg str))
  (def supported-by (-> bricks find-supports first invert-keys))
  (def dangerous (tabseq [[k vs] :pairs supported-by
                          :when (= 1 (length vs))]
                         (first vs) true))
  (sum-loop [i :keys bricks
             :when (not (dangerous i))]
            1))

(judge/test (part1 test-input) 5)
(judge/test (part1 real-input) 477)

(defn part2 [str]
  (def bricks (peg/match peg str))
  (find-supports bricks)
  (sum-loop [i :keys bricks]
            (last (find-supports (seq [[j b] :pairs bricks
                                       :when (not= j i)]
                                   b)))))

(judge/test (part2 test-input) 7)
# 40s
# (judge/test (part2 real-input) 61555)
