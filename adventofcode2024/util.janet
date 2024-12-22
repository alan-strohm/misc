(import judge)

(defmacro fold-loop [init f dsl & body]
  (with-syms [$result $init $f]
    ~(let [,$init ,init ,$f ,f]
       (var ,$result ,$init)
       (loop ,dsl
         (set ,$result (,$f ,$result (do ,;body))))
       ,$result)))

(defmacro sum-loop [dsl & body]
  ~(as-macro ,fold-loop 0 + ,dsl ,;body))

(defmacro defmemo [name args & body]
  (with-syms [$f $args $memo $result]
    ~(def ,name
       (do
         (var ,name nil)
         (defn ,$f ,args ,;body)
         (def ,$memo @{})
         (set ,name (fn [& ,$args]
                     (when-let [,$result (,$memo ,$args)]
                       (break ,$result))
                     (def ,$result (apply ,$f ,$args))
                     (put ,$memo ,$args ,$result)
                     ,$result))))))

(defn grid/parse [str]
  (def content (->> str
                    string/trim
                    (string/split "\n")
                    (map |(map string/from-bytes $))))
  @{:dims [(length (first content)) (length content)]
    :content content})

(defn grid/get [{:content content} [x y]]
  (assert content "invalid grid: nil content")
  (get-in content [y x]))

(defn grid/contains [{:dims [max-x max-y]} [x y]]
  (and
    (< x max-x)
    (>= x 0)
    (< y max-y)
    (>= y 0)))

(defn- grid/check-oob [grid p]
  (assert (grid/contains grid p)
          (string/format "oob point: %n dims: %n" p (grid :dims))))

(defn grid/set [grid p v]
  (compwhen (dyn *debug*)
    (grid/check-oob grid p)
    (assert (string? v) (string/format "invalid value: %n" v)))
  (let [{:content content} grid
        [x y] p]
    (put-in content [y x] v)))

(defn grid/pairs [{:content content}]
  (assert content "invalid grid: nil content")
  (generate [[y line] :pairs content
             [x val] :pairs line]
    [[x y] val]))

(defn grid/map [{:content content :dims dims} f]
  {:content (map |(map f $) content) :dims dims})

(defn grid/format [{:content content}]
  (assert content "invalid grid: nil content")
  (string/join (map |(string/join $ "") content) "\n"))

(defn grid/find [grid f]
  (some (fn [[p v]] (if (f v) p))
        (grid/pairs grid)))

(defn p2+ [[x1 y1 & rest1] [x2 y2 & rest2]]
  (compwhen (dyn *debug*)
    (assert (empty? rest1) "first argument has %d elements" (+ 2 (length rest1)))
    (assert (empty? rest2) "second argument has %d elements" (+ 2 (length rest1))))
  [(+ x1 x2) (+ y1 y2)])

(defn p2- [[x1 y1 & rest1] [x2 y2 & rest2]]
  (compwhen (dyn *debug*)
    (assert (empty? rest1) "first argument has %d elements" (+ 2 (length rest1)))
    (assert (empty? rest2) "second argument has %d elements" (+ 2 (length rest1))))
  [(- x1 x2) (- y1 y2)])

(defmacro p2+= [v1 v2] ~(set ,v1 (,p2+ ,v1 ,v2)))

(defn vec*n [vec n] (tuple/slice (map |(* n $) vec)))
(judge/test (vec*n [1 2] -1) [-1 -2])

(def dirs8 [[0 -1] [1 -1] [1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1]])
(def [dir-N dir-NE dir-E dir-SE dir-S dir-SW dir-W dir-NW] dirs8)
(def dirs4 [dir-N dir-E dir-S dir-W])
(def format-dir {dir-N "^" dir-E ">" dir-S "v" dir-W "<"})
(def parse-dir (tabseq [[d s] :pairs format-dir] s d))

(defn cons [head rest]
  [head rest])

(defn make-list [arrtup]
  (match arrtup
    nil nil
    [head & rest] (cons head (make-list rest))
    []))

(judge/test (make-list [1 2]) [1 [2 []]])

(defn det2d "Compute the determinate of a 2x2 matrix"
  [[[a c] [b d]]] (- (* a d) (* b c)))

(judge/test (det2d [[3 1] [7 -4]]) -19)

# https://en.wikipedia.org/wiki/Cramer%27s_rule
(defn solve2d
  "Return x such that Ax = y"
  [A y]
  (let [detA (det2d A)
        x0 (/ (det2d [y (in A 1)]) detA)
        x1 (/ (det2d [(in A 0) y]) detA)]
    (if (= 0 detA)
      nil
      [x0 x1])))

(judge/test (solve2d [[-2 1] [1 1]] [-1 6]) [2.3333333333333335 3.6666666666666665])

# Janet's xor only works up to 32 bit integers.
(defn xor [a b]
  (def mid 0x10000000)
  (def lower (bxor (mod a mid) (mod b mid)))
  (def upper (bxor (div a mid) (div b mid)))
  (+ (* upper mid) lower))

(def- hex (partial string/format "%x"))
(judge/test (xor (math/exp2 (* 3 15)) (math/exp2 (* 3 15))) 0)
(judge/test (hex (xor 0x1020304050 0x8000000000)) "9020304050")

