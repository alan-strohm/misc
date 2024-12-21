(use judge)
(use ./util)

(def real-input `
593A
283A
670A
459A
279A
`)

(def test-input `
029A
980A
179A
456A
379A
`)

(defn make-pad [str]
  (def grid (grid/parse str))
  (def pad
    (tabseq [[p v] :in (grid/pairs grid)] v p))
  [grid pad])

(def numpad (make-pad `
789
456
123
*0A`))

(def dirpad (make-pad `
*^A
<v>`))

(defn get-dir [dir-1d dim]
  (case [dir-1d dim]
    [-1 0] dir-W
    [1 0] dir-E
    [-1 1] dir-N
    [1 1] dir-S))

(defn path [dim-order cur dest]
  (seq [dim :in dim-order
        :let [dist (- (in dest dim) (in cur dim))
              dir-1d (/ dist (math/abs dist))]
        _ :range [0 (math/abs dist)]]
    (get-dir dir-1d dim)))

(test (path [0 1] [2 2] [0 0])
  @[[-1 0] [-1 0] [0 -1] [0 -1]])
(test (path [1 0] [2 2] [0 0])
  @[[0 -1] [0 -1] [-1 0] [-1 0]])

(defn options [cur dest]
  (seq [order :in [[0 1] [1 0]]]
    (path order cur dest)))

(test (options [2 2] [0 0])
  @[@[[-1 0] [-1 0] [0 -1] [0 -1]]
    @[[0 -1] [0 -1] [-1 0] [-1 0]]])

(defn crosses-star [grid start path]
  (label result
    (loop [:before (var cur start)
           d :in path
           :before (p2+= cur d)
           :when (= "*" (grid/get grid cur))]
      (return result true))))

(test (crosses-star (first numpad) [2 3] [dir-W dir-W]) true)
(test (crosses-star (first numpad) [2 2] [dir-W dir-W]) nil)

(defn to-presses [dirs]
  (array/push (map format-dir dirs) "A"))

(defmemo find-presses [pads wants]
  (when (empty? pads)
    (break wants))
  (def [pad & rest] pads)
  (def [grid pad] pad)
  (string/join
    (catseq [:before (var cur (pad "A"))
             want :in (map string/from-bytes wants)
             :let [dest (in pad want)]
             #:before (printf "dest: %n" dest)
             :after (set cur dest)]
            (->> (options cur dest)
                 (filter |(not (crosses-star grid cur $)))
                 (map to-presses)
                 (map |(find-presses rest (string/join $)))
                 (sort-by length)
                 first))))

(defn press [pad presses]
  (def [grid pad] pad)
  (var cur (pad "A"))
  (string/join
    (catseq [press :in (map string/from-bytes presses)]
            (when (not= "A" press)
              (set cur (p2+ cur (parse-dir press))))
            (if (= "A" press)
              (grid/get grid cur)
              []))))

(def- test-find-presses (partial find-presses [numpad dirpad dirpad]))
(test (test-find-presses "379A")
  "v<<A>>^AvA^Av<A<AA>>^AAvA^<A>AAvA^Av<A^>AA<A>Av<A<A>>^AAA<Av>A^A")
(test (press dirpad (test-find-presses "379A"))
  "<A>Av<<AA>^AA>AvAA^Av<AAA^>A")
(test (test-find-presses "456A")
  "v<<A>>^AAv<A<A>>^AAvAA^<A>Av<A^>A<A>Av<A^>A<A>Av<A<A>>^AA<Av>A^A")

(defmemo find-presses [pads wants]
  (when (empty? pads)
    (break (length wants)))
  (def [pad & rest] pads)
  (def [grid pad] pad)
  (sum-loop [:before (var cur (pad "A"))
             want :in (map string/from-bytes wants)
             :let [dest (in pad want)]
             #:before (printf "dest: %n" dest)
             :after (set cur dest)]
            (->> (options cur dest)
                 (filter |(not (crosses-star grid cur $)))
                 (map to-presses)
                 (map |(find-presses rest (string/join $)))
                 min-of)))

(defn read [str]
  (->> str string/trim (string/split "\n")))

(defn numeric [code]
  (->> code (map string/from-bytes) (filter |(not= "A" $)) string/join scan-number))

(test (numeric "029A") 29)

(defn part1 [str]
  (sum-loop [code :in (read str)]
            (def need (find-presses [numpad dirpad dirpad] code))
            (* need (numeric code))))

(test (part1 test-input) 126384)
(test (part1 real-input) 162740)

(defn part2 [str]
  (def pads (array/concat @[numpad] (seq [:repeat 25] dirpad)))
  (sum-loop [code :in (read str)]
            (def need (find-presses (tuple/slice pads) code))
            (* need (numeric code))))

(test (part2 real-input) 203640915832208)
