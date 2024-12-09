(import judge)
(use ./util)

(def real-input (slurp "./input/09.txt"))

(def test-input `
2333133121414131402
`)

(defn file-id [map-idx] (if (odd? map-idx)
                          (errorf "can't get file-id for odd map idx: %n" map-idx)
                          (/ map-idx 2)))

(defn partial-checksum [disk-idx file-id len]
  (* file-id
     (sum (range disk-idx (+ disk-idx len)))))

(judge/test (partial-checksum 2 18 2) 90)

(defn read [str]
  (seq [[i v] :pairs (peg/match ~(any (number :d)) str)]
    (if (even? i)
      @{:type :file
        :id (file-id i)
        :len v}
      @{:type :space
        :len v})))

(defn part1 [str]
  (var entries (read str))
  (assert (odd? (length entries)))
  (var to-compact (array/pop entries))
  (sum-loop [:before (var disk-i 0)
             #:after (print)
             entry :iterate (or (first entries) to-compact)
             :after (if (= entry to-compact)
                      (set to-compact nil)
                      (when (not (empty? entries))
                        (set entries (array/slice entries 1))))
             _ :range [0 (entry :len)]
             :after (++ disk-i)]
            (if (= :file (entry :type))
              (do
                #(prin (entry :id))
                (* (entry :id) disk-i))
              (do
                (when (zero? (to-compact :len))
                  (array/pop entries)  # Pop space
                  (set to-compact (array/pop entries))
                  (if (not to-compact)
                    (break)))
                #(prin (to-compact :id))
                (update to-compact :len dec)
                (* (to-compact :id) disk-i)))))
  

(judge/test (part1 test-input) 1928)
(judge/test (part1 real-input) 6384282079460)
