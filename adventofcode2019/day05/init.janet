(import judge)

(defn load [file]
  (as-> (file/open file) _
        (file/read _ :line)
        (string/trim _)
        (string/split "," _)
        (map scan-number _)))

(defn get-arg [mem pc arg-i modes]
  (def arg (get mem (+ pc arg-i 1)))
  (def mode (get modes arg-i))
  (case mode
    "0" (get mem arg)
    "1" arg
    (errorf "invalid mode %d" mode)))

(defn do-mem-io [mem pc f num-inputs modes]
  (def out-i (get mem (+ pc num-inputs 1)))
  (as-> num-inputs _
        (range 0 _)
        (map |(get-arg mem pc $ modes) _)
        (f ;_)
        (put mem out-i _)))

(def op-info {"01" {:num-args 3}
              "02" {:num-args 3}
              "03" {:num-args 1}
              "04" {:num-args 1}})

(defn scan-op [op]
  (def op-str (string/format "%05d" op))
  [(string/slice op-str 3)
   [(string/slice op-str 2 3)
    (string/slice op-str 1 2)
    (string/slice op-str 0 1)]])

(judge/test (scan-op 3) ["03" ["0" "0" "0"]])
(judge/test (scan-op 1101) ["01" ["1" "1" "0"]])

(defn save-input [mem pc input-idx input]
  (def out-i (get mem (+ pc 1)))
  (put mem out-i input))

(defn run [mem input]
  (var pc 0)
  (var input-idx 0)
  (var output @[])
  (var output-idx 0)
  (defn exec [f num-inputs modes]
    (def out-i (get mem (+ pc num-inputs 1)))
    (def result (f ;(map |(get-arg mem pc $ modes) (range 0 num-inputs))))
    (put mem out-i result)
    (+= pc (+ num-inputs 2)))
  (defn jump-if [pred? modes]
    (def arg (get-arg mem pc 0 modes))
    (def dest (get-arg mem pc 1 modes))
    (if (pred? arg) (set pc dest) (+= pc 3)))
          
  (while (not= 99 (get mem pc))
    (def [op modes] (scan-op (get mem pc)))
    (case op
      "01" (exec + 2 modes)
      "02" (exec * 2 modes)
      "03" (do
             (def out-i (get mem (+ pc 1)))
             (put mem out-i (get input input-idx))
             (++ input-idx)
             (+= pc 2))
      "04" (do
             (def arg (get-arg mem pc 0 modes))
             (put output output-idx arg)
             (++ output-idx)
             (+= pc 2))
      "05" (jump-if |(not= $ 0) modes)
      "06" (jump-if |(= $ 0) modes)
      "07" (exec |(if (< $0 $1) 1 0) 2 modes)
      "08" (exec |(if (= $0 $1) 1 0) 2 modes)
      (errorf "invalid op %s" op)
      ))
  output)

(defn part1 [file]
  (run (load file) [1]))

(judge/test (part1 "./day05/input.txt") @[0 0 0 0 0 0 0 0 0 5074395])

(defn part2 [file]
  (run (load file) [5]))

(judge/test (part2 "./day05/input.txt") @[8346937])
