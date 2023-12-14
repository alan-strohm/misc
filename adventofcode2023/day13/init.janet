(import judge)

(defn mirror [len m x]
  (assert (< x m))
  (let [r (+ m (- m x 1))]
    (if (>= r len) nil r)))

(judge/test (mirror 2 1 0) 1)
(judge/test (mirror 3 2 0) nil)
(judge/test (mirror 4 2 0) 3)

(defn rotate [lines]
  (var result @[])
  (loop [x :range [0 (length (get lines 0))]
         :before (var line @"")
         :after (array/push result (string line))
         y :range [0 (length lines)]
         :let [char (x (y lines))]]
    (buffer/push-byte line char))
  result)

(judge/test (rotate ["abc" "def"]) @["ad" "be" "cf"])
(judge/test (rotate ["ad" "be" "cf"]) @["abc" "def"])

(defn find-mirror-1d [lines]
  (def valid-mirrors (array/new-filled (length (0 lines)) true))
  (loop [line :in lines
         x :range [0 (length line)]
         :let [char (x line)]
         m :range [0 (length valid-mirrors)] :when (mirror valid-mirrors)
         :let [mirror-x (mirror (length line) m x)]]
    (if (and mirror-x (not= char (mirror-x line)))
      (put valid-mirrors m false))))
    

