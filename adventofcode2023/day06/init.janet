(import judge)

(def example ``
Time:      7  15   30
Distance:  9  40  200
  ``)

(def input ``
Time:        35     69     68     87
Distance:   213   1168   1086   1248
  ``)

(def peg
  (peg/compile '{:numbers (some (+ :s+ (number :d+)))
                 :time (* "Time:" :numbers)
                 :distance (* "Distance:" :numbers)
                 :main (* (group :time) (group :distance))}))

(judge/test (peg/match peg example) @[@[7 15 30] @[9 40 200]])

(defn get-dist [hold total]
  (let [left (- total hold)
        velocity hold]
    (* velocity left)))

(judge/test (get-dist 1 7) 6)
(judge/test (get-dist 4 7) 12)

(defn num-winning [time record]
  (var result 0)
  (loop [hold :range [1 time]]
    (let [dist (get-dist hold time)]
      (if (> dist record) (++ result))))
  result)

(judge/test (num-winning 7 9) 4)

(defn day1 [input]
  (product (map num-winning ;(peg/match peg input))))

(judge/test (day1 example) 288)
(judge/test (day1 input) 170000)

(judge/test (num-winning 71530 940200) 71503)
(judge/test (num-winning 35696887 213116810861248) 20537782)
