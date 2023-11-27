#!/usr/bin/env janet

(import judge)

(defn lines [file]
  (as-> file $
        (file/open $)
        (file/read $ :all)
        (string/split "\n" $)))

(defn fuel [mass]
  (as-> mass $
        (/ $ 3)
        (math/floor $)
        (- $ 2)))

(judge/test (fuel 12) 2)
(judge/test (fuel 14) 2)
(judge/test (fuel 1969) 654)
(judge/test (fuel 100756) 33583)

(defn run [file fun]
  (var total 0)
  (each line (lines file) (+= total (if (= line "") 0 (fun (scan-number line)))))
  total)

(defn day01 [file] (run file fuel))

(judge/test (day01 "./day01/input.txt") 3233481)

(defn total-fuel [mass]
  (def f (fuel mass))
  (if (< f 0)
    0
    (+ f (total-fuel f))))

(judge/test (total-fuel 12) 2)
(judge/test (total-fuel 1969) 966)
(judge/test (total-fuel 100756) 50346)

(defn day02 [file] (run file total-fuel))

(judge/test (day02 "./day01/input.txt") 4847351)
