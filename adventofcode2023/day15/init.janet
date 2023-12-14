(import judge)

(def test-input "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")
(def real-input (slurp "./day15/input.txt"))

(defn hash [str]
  (reduce |(-> $0 (+ $1) (* 17) (% 256)) 0 str))

(judge/test (hash "HASH") 52)
(judge/test (hash "cm") 0)

(defn part1 [str]
  (->> str (string/trim) (string/split ",") (map hash) (sum)))

(judge/test (part1 test-input) 1320)
(judge/test (part1 real-input) 516070)

(defn sep [pat delim] ~(* (any (* ,pat ,delim)) ,pat))

(def line
  (peg/compile
    ~{:step (* ':a+ (+ (* (/ '"=" :set) (number :d+)) (/ '"-" :rm)))
      :main ,(sep '(group :step) ",")}))


(defn box/new [] @{:focals @{} :max 0})

(defn box/set [box label focal]
  (if-let [cur (get-in box [:focals label])]
    (put cur :focal focal)
    (do
      (put-in box [:focals label] @{:focal focal :pos (in box :max)})
      (update box :max inc)))
  box)

(judge/test (-> (box/new) (box/set "rn" 1) (box/set "rn" 2))
  @{:focals @{"rn" @{:focal 2 :pos 0}}
    :max 1})

(defn rm-label [box lbl]
  (put-in box [:focals lbl] nil))

(judge/test (-> (box/new) (box/set "rn" 1) (rm-label "rn")) @{:focals @{} :max 1})

(defn part2 [str]
  (def boxes @{})
  (defn do-step [step]
    (def box-id (hash (in step 0)))
    (var b (in boxes box-id))
    (if (nil? b) (do
                   (set b (box/new))
                   (put boxes box-id b)))
    (match step
      [lbl :set foc] (box/set b lbl foc)
      [lbl :rm] (rm-label b lbl)))
  (->> str (peg/match line) (map do-step))
  (def powers
    (seq [[box-id box] :pairs boxes
          :let [focals (->> (in box :focals) (values) (sort-by |(in $ :pos)))]
          [idx {:focal focal}] :pairs focals]
      (* (+ box-id 1) (+ idx 1) focal)))
  (sum powers))

(judge/test (part2 test-input) 145)
(judge/test (part2 real-input) 244981)
