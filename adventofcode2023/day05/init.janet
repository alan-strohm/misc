(import judge)
(use ../util)

(def test-input (slurp "./day05/example.txt"))
(def real-input (slurp "./day05/input.txt"))

(def peg '{:main (* (group :seeds) (some (group :map)))
           :seeds (* "seeds: " (some (+ :s+ (number :d+))))
           :map (* :w+ "-to-" :w+ " map:\n" (some (group :range)))
           :range (repeat 3 (* (number :d+) :s+))
           })

(judge/test (peg/match peg test-input)
  @[@[79 14 55 13]
    @[@[50 98 2] @[52 50 48]]
    @[@[0 15 37] @[37 52 2] @[39 0 15]]
    @[@[49 53 8]
      @[0 11 42]
      @[42 0 7]
      @[57 7 4]]
    @[@[88 18 7] @[18 25 70]]
    @[@[45 77 23] @[81 45 19] @[68 64 13]]
    @[@[0 69 1] @[1 0 69]]
    @[@[60 56 37] @[56 93 4]]])

(defn split 
  "Return the portion of left before, intersecting and after right"
  [[left-begin left-end] [right-begin right-end]]
  [(if (< left-begin right-begin)
     [left-begin (min left-end right-begin)]
     nil)
   (range/intersect [left-begin left-end] [right-begin right-end])
   (if (> left-end right-end)
     [(max left-begin right-end) left-end]
     nil)
   ])

(judge/test (split [0 4] [1 2]) [[0 1] [1 2] [2 4]])
(judge/test (split [0 2] [1 2]) [[0 1] [1 2] nil])
(judge/test (split [0 2] [2 3]) [[0 2] nil nil])
(judge/test (split [2 3] [0 2]) [nil nil [2 3]])
(judge/test (split [1 3] [0 2]) [nil [1 2] [2 3]])

(defn convert
  "Convert src-ranges to corresponding destination ranges using rules"
  [src-ranges rules]
  (var current src-ranges)
  (def dst-ranges @[])
  (loop [[dst src len] :in rules
         :let [next @[]
               offset (- dst src)]
         :after (set current next)
         src-range :in current]
    (def [before inter after] (split src-range [src (+ src len)]))
    (match inter
      [b e] (array/push dst-ranges [(+ b offset) (+ e offset)]))
    (array/push next ;(filter truthy? [before after])))
  [;dst-ranges ;current])

(judge/test (convert [[79 80] [14 15] [55 56] [13 14]] [[50 98 2] [52 50 48]]) [[81 82] [57 58] [14 15] [13 14]])
(judge/test (convert [[79 93] [55 68]] [[50 98 2] [52 50 48]]) [[81 95] [57 70]])
(judge/test (convert [[97 99]] [[50 98 2] [52 50 48]]) [[50 51] [99 100]])

(defn solve
  "Find the lowest destination corresponding to one of init."
  [src maps]
  (->> (reduce convert src maps)
       (map first)
       min-of))

(defn part1 [str]
  (def [seeds & mappings] (peg/match peg str))
  (solve (seq [s :in seeds] [s (inc s)]) mappings))

(judge/test (part1 test-input) 35)
(judge/test (part1 real-input) 462648396)

(defn part2 [str]
  (def [seeds & mappings] (peg/match peg str))
  (def ranges (seq [[b l] :in (partition 2 seeds)] [b (+ b l)]))
  (solve ranges mappings))

(judge/test (part2 test-input) 46)
(judge/test (part2 real-input) 2520479)
