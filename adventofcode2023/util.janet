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

(judge/test (fold-loop 0 + [v :in [1 2 3]] v) 6)
(judge/test (sum-loop [v :in [1 2 3]] v) 6)

(defn make-set [a] (reduce |(put $0 $1 true) @{} a))

(judge/test (make-set [1 2 3 4]) @{1 true 2 true 3 true 4 true})

(defn grid/parse [str]
  (def content (->> str
                    string/trim
                    (string/split "\n")
                    (map |(map string/from-bytes $))))
  @{:dims [(length (first content)) (length content)]
    :content content})

(defn grid/map [{:content content :dims dims} f]
  {:content (map |(map f $) content) :dims dims})

(defn grid/contains [{:dims [max-x max-y]} [x y]]
  (and
    (< x max-x)
    (>= x 0)
    (< y max-y)
    (>= y 0)))

(defn grid/height [{:dims [_ h]}] h)
(defn grid/width [{:dims [w _]}] w)

(defn grid/get [{:content content} [x y]]
  (assert content "invalid grid: nil content")
  (get-in content [y x]))

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

(judge/test-error (grid/set (grid/parse "1\n2") [1 1] "3") "oob point: (1 1) dims: (1 2)")
(judge/test-error (grid/set (grid/parse "1") [0 0] 1) "invalid value: 1")

(defn grid/pairs [{:content content}]
  (assert content "invalid grid: nil content")
  (generate [[y line] :pairs content
             [x val] :pairs line]
    [[x y] val]))

(defn grid/format [{:content content}]
  (assert content "invalid grid: nil content")
  (string/join (map |(string/join $ "") content) "\n"))

(defn p2+ [[x1 y1 & rest1] [x2 y2 & rest2]]
  (compwhen (dyn *debug*)
    (assert (empty? rest1) "first argument has %d elements" (+ 2 (length rest1)))
    (assert (empty? rest2) "second argument has %d elements" (+ 2 (length rest1))))
  [(+ x1 x2) (+ y1 y2)])

(defn vec+ [first & rest]
  (assert (all |(= (length $) (length first)) rest) "all args must be the same length")
  (tuple/slice (map + first ;rest)))
(judge/test (vec+ [1 2] [3 4]) [4 6])
(judge/test-error (vec+ [1] [1 2 3]) "all args must be the same length")

(defn vec*n [vec n] (tuple/slice (map |(* n $) vec)))
(judge/test (vec*n [1 2] -1) [-1 -2])

(defn vec- [first & rest]
  (vec+ first ;(map |(vec*n $ -1) rest)))
(judge/test (vec- [2 3] [1 1]) [1 2])

(defmacro vec+= [v1 v2] ~(set ,v1 (,vec+ ,v1 ,v2)))

(defn sep [pat delim]
  ~(* ,pat (any (* ,delim ,pat)) (any ,delim)))

(judge/test (peg/match (sep '':a+ ",") "a,b,c") @["a" "b" "c"])
(judge/test (peg/match (sep '':a+ ",") "a") @["a"])
(judge/test (peg/match ~{:main ,(sep '':a+ ",")} "a") @["a"])

(def dirs4 [[0 -1] [1 0] [0 1] [-1 0]])
(def [dir-N dir-E dir-S dir-W] dirs4)
