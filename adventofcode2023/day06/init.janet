(import judge)

(defn get-dist [velocity total]
  (let [left (- total velocity)]
    (* velocity left)))

(judge/test (get-dist 1 7) 6)
(judge/test (get-dist 4 7) 12)

(defn num-winning [time record]
  (var result 0)
  (loop [hold :range [1 time]
         :let [dist (get-dist hold time)]
         :when (> dist record)]
    (++ result))
  result)

(judge/test (num-winning 7 9) 4)

(defn part1 [input] (product (map num-winning ;input)))

(judge/test (part1 [[7 15 30] [9 40 200]]) 288)
(judge/test (part1 [[35 69 68 87] [213 1168 1086 1248]]) 170000)

(def part2 num-winning)

(judge/test (part2 71530 940200) 71503)
(judge/test (part2 35696887 213116810861248) 20537782)
