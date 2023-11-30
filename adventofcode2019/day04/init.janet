(import judge)

(defn meets-criteria-bytes [password-bytes p1]
  (def has-repeated
    (as-> password-bytes _
          (frequencies _)
          (find |(if p1 (and (>= $ 2) (<= $ 6)) (= 2 $)) _)
          (nil? _)
          (not _)))
  (and has-repeated (<= ;password-bytes)))

(defn meets-criteria [password p1]
  (as-> (string password) $
        (string/bytes $)
        (meets-criteria-bytes $ p1)))

(judge/test (meets-criteria 111111 true) true)
(judge/test (meets-criteria 223450 true) false)
(judge/test (meets-criteria 123789 true) false)

(judge/test (meets-criteria 111111 false) false)
(judge/test (meets-criteria 112233 false) true)
(judge/test (meets-criteria 123444 false) false)
(judge/test (meets-criteria 111122 false) true)

(defn run [p1]
  (var num-meets-criteria 0)
  (for i 193652 649729
    (if (meets-criteria i p1) (++ num-meets-criteria)))
  num-meets-criteria)

(judge/test (run true) 1605)
(judge/test (run false) 1102)
