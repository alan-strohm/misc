(import judge)
(use ../util)

(def test-input "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")
(def real-input (slurp "./day15/input.txt"))

(defn hash [str]
  (reduce |(-> $0 (+ $1) (* 17) (% 256)) 0 str))

(judge/test (hash "HASH") 52)
(judge/test (hash "cm") 0)

(defn part1 [str]
  (->> str string/trim (string/split ",") (map hash) sum))

(judge/test (part1 test-input) 1320)
(judge/test (part1 real-input) 516070)

(def peg ~{:main (split "," (group :step))
           :step (* ':w+ (+ (* (/ "=" :set) (number :d+))
                            (/ "-" :rm)))})

(judge/test (peg/match peg test-input)
  @[@["rn" :set 1]
    @["cm" :rm]
    @["qp" :set 3]
    @["cm" :set 2]
    @["qp" :rm]
    @["pc" :set 4]
    @["ot" :set 9]
    @["ab" :set 5]
    @["pc" :rm]
    @["pc" :set 6]
    @["ot" :set 7]])

(defn box/new [] @{:focals @{} :max 0})

(defn box/set [box label focal]
  (update-in box [:focals label]
             |(if $
                (put $ :focal focal)
                (let [{:max max} box]
                  (update box :max inc)
                  @{:focal focal :pos max}))))

(judge/test (-> (box/new) (box/set "rn" 1) (box/set "rn" 2))
  @{:focals @{"rn" @{:focal 2 :pos 0}}
    :max 1})

(defn box/rm [box label] (put-in box [:focals label] nil))

(judge/test (-> (box/new) (box/set "rn" 1) (box/rm "rn")) @{:focals @{} :max 1})

(defn part2 [str]
  (def boxes @{})
  (defn do-step [[label & rest]]
    (def box-id (hash label))
    (def box (or (boxes box-id)
                 (set (boxes box-id) (box/new))))
    (match rest
      [:set f] (box/set box label f)
      [:rm] (box/rm box label)))
  (->> str (peg/match peg) (map do-step))
  (sum-loop [[box-id box] :pairs boxes
             :let [focals (->> (in box :focals) values (sort-by |(in $ :pos)))]
             [idx {:focal focal}] :pairs focals]
      (* (+ box-id 1) (inc idx) focal)))

(judge/test (part2 test-input) 145)
(judge/test (part2 real-input) 244981)
