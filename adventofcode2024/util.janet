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

(defn grid/pairs [{:content content}]
  (assert content "invalid grid: nil content")
  (generate [[y line] :pairs content
             [x val] :pairs line]
    [[x y] val]))

(defn p2+ [[x1 y1 & rest1] [x2 y2 & rest2]]
  (compwhen (dyn *debug*)
    (assert (empty? rest1) "first argument has %d elements" (+ 2 (length rest1)))
    (assert (empty? rest2) "second argument has %d elements" (+ 2 (length rest1))))
  [(+ x1 x2) (+ y1 y2)])

(defmacro p2+= [v1 v2] ~(set ,v1 (,p2+ ,v1 ,v2)))

(def dirs8 [[0 -1] [1 -1] [1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1]])
(def [dir-N dir-NE dir-E dir-SE dir-S dir-SW dir-W dir-NW] dirs8)

(defn cons [head rest]
  [head rest])

(defn make-list [arrtup]
  (match arrtup
    nil nil
    [head & rest] (cons head (make-list rest))
    []))

(judge/test (make-list [1 2]) [1 [2 []]])
