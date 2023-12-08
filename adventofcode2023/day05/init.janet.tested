(import judge)

(def peg
  (peg/compile
    '{:seeds (* "seeds: " (some (+ :s+ (number :d+))))
      :range (repeat 3 (* (number :d+) :s+))
      :map (* ':w+ "-to-" ':w+ " map:\n" (some (group :range)))
      :main (* (group :seeds) (some (group :map)))}))

(defn map-id [id ranges]
  (label result
    (each [dst src len] ranges
      (let [delta (- id src)]
        (if (and (>= delta 0) (< delta len)) (return result (+ dst delta)))))
    (return result id)))

(defn load [file]
  (def parsed (peg/match peg (slurp file)))
  (defn acc-deps [deps [from to & mappings]]
    (put deps from
         @{:to to
           :mappings mappings}))
  @{:seeds (get parsed 0)
    :deps (reduce acc-deps @{} (array/slice parsed 1))})

(defn get-dst [deps src src-type dst-type]
  (if (= src-type dst-type) src
    (let [next-type (get-in deps [src-type :to])
          next-v (map-id src (get-in deps [src-type :mappings]))]
      (get-dst deps next-v next-type dst-type))))

(defn day1 [file]
  (def almanac (load file))
  (min ;(map |(get-dst (almanac :deps) $ "seed" "location") (almanac :seeds))))

(judge/test (day1 "./day05/example.txt") 35)
(judge/test (day1 "./day05/input.txt") 462648396)

(defn map-intersection [[start end] [dst src len]]
  (let [start-delta (- start src)
        end-delta (min (- end src) len)
        new-start (if (< start src) dst (+ dst start-delta))]
    (if (or (>= start-delta len) (<= end-delta 0))
      []
      [new-start (+ dst end-delta)])))

(judge/test (map-intersection [2 4] [2 5 1]) [])
(judge/test (map-intersection [2 4] [12 1 1]) [])
(judge/test (map-intersection [2 4] [12 4 1]) [])
(judge/test (map-intersection [2 4] [12 3 1]) [12 13])
(judge/test (map-intersection [2 5] [12 3 1]) [12 13])
(judge/test (map-intersection [2 4] [12 1 2]) [13 14])
(judge/test (map-intersection [2 4] [12 1 3]) [13 15])
(judge/test (map-intersection [2 4] [12 2 1]) [12 13])

(defn subtract [[start end] [_ src len]]
  (def result @[])
  (let [src-end (+ src len)]
    (if (< start src) (array/push result [start (min end src)]))
    (if (> end src-end) (array/push result [(max start src-end) end])))
  result)

(judge/test (subtract [2 4] [0 5 1]) @[[2 4]])
(judge/test (subtract [2 4] [0 1 1]) @[[2 4]])
(judge/test (subtract [2 4] [0 1 2]) @[[3 4]])
(judge/test (subtract [2 4] [0 3 1]) @[[2 3]])
(judge/test (subtract [2 5] [0 3 1]) @[[2 3] [4 5]])
(judge/test (subtract [2 4] [0 1 3]) @[])
(judge/test (subtract [2 4] [0 2 2]) @[])
(judge/test (subtract [2 4] [0 2 3]) @[])

(defn get-dst-ranges [deps src-ranges src-type dst-type]
  (if (= src-type dst-type) src-ranges
    (let [next-type (get-in deps [src-type :to])
          mappings (get-in deps [src-type :mappings])]
      (print (string/format "finding mappings for %d ranges from %s to %s" (length src-ranges) src-type next-type))
      (def dst-ranges @[])
      (loop [:before (var found-mapping false)
             src-range :in src-ranges
             :before (set found-mapping false)
             :after (if-not found-mapping (array/push dst-ranges src-range))
             mapping :in mappings]
        (def dst-range (map-intersection src-range mapping))
        #(print (string/format " got dst-range %n from src-range %n and mapping %n" dst-range src-range mapping))
        (if (not= [] dst-range)
          (do
            (set found-mapping true)
            (array/push dst-ranges dst-range)
            (each new-src (subtract src-range mapping)
              #(print (string/format " got remaining src-range %n from src-range %n and mapping %n" new-src src-range mapping))
              (array/push src-ranges new-src)))))
      (get-dst-ranges deps dst-ranges next-type dst-type))))

(defn make-ranges [list]
  (def result @[])
  (loop [x :range [0 (length list)] :when (even? x)]
    (let [start (get list x)
          end (+ start (get list (+ x 1)))]
      (array/push result [start end])))
  result)

(judge/test (make-ranges [1 2 3 4]) @[[1 3] [3 7]])

(defn day2 [file]
  (let [almanac (load file)
        seed-ranges (make-ranges (almanac :seeds))
        deps (almanac :deps)]
    (min ;(map first (get-dst-ranges deps seed-ranges "seed" "location")))))

(judge/test (day2 "./day05/example.txt") 46)
(judge/test (day2 "./day05/input.txt") 2520479)
