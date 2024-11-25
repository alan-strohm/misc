(import judge)

(use ./util)

(def test-input1 `
broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a`)

(def test-input2 `
broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output`)

(def real-input (slurp "./input/20.txt"))

(def peg
  ~{:main (split "\n" (+ -1 :line))
    :line (/ (* :module " -> " :outputs) ,|[$1 @{:type $0 :outputs $2}])
    :module (+ :broadcaster :typed)
    :broadcaster (* "broadcaster" (constant :bcast) (constant :bcast))
    :typed (* (/ '(set "%&") ,keyword) (/ ':a+ ,keyword))
    :outputs (group (split ", " (/ ':a+ ,keyword)))
    })

(judge/test (peg/match peg test-input2)
  @[[:bcast @{:outputs @[:a] :type :bcast}]
    [:a @{:outputs @[:inv :con] :type :%}]
    [:inv @{:outputs @[:b] :type :&}]
    [:b @{:outputs @[:con] :type :%}]
    [:con @{:outputs @[:output] :type :&}]])

(def component-builders
  "For each each component type, a constructor function which takes the input
  IDs for the component and returns a function which maps a signal from a
  single input to an output signal or nil if the component outputs nothing"
  {:bcast (fn [inputs] (fn [from sig] sig))
   :% (fn [inputs]
        (var state false)
        (fn [from sig]
          (when (= sig :low)
            (set state (not state))
            (if state :high :low))))
   :& (fn [inputs]
        (def state (tabseq [id :in inputs] id :low))
        (fn [from sig]
          (put state from sig)
          (if (all |(= $ :high) state)
            :low :high)))})

(defn mac/new [str]
  (def mac (from-pairs (peg/match peg str)))
  (loop [[id {:outputs outputs}] :pairs mac
         output :in outputs
         :when (mac output)]
    (put-in mac [output :inputs id] true))
  (tabseq [[id {:type t :inputs inputs :outputs outputs}] :pairs mac
           :let [builder (assert (in component-builders t) (string/format "no builder for id: %q, type: %q" id t))
                 inputs (if inputs (keys inputs) @[])]]
    id {:next-sig-fn (builder inputs)
        :outputs outputs
        :inputs inputs}))

(judge/test (mac/new test-input1)
  @{:a {:inputs @[:bcast :inv]
        :next-sig-fn "<function 0x3>"
        :outputs @[:b]}
    :b {:inputs @[:bcast :a]
        :next-sig-fn "<function 0x4>"
        :outputs @[:c]}
    :bcast {:inputs @[]
            :next-sig-fn "<function 0x1>"
            :outputs @[:a :b :c]}
    :c {:inputs @[:bcast :b]
        :next-sig-fn "<function 0x5>"
        :outputs @[:inv]}
    :inv {:inputs @[:c]
          :next-sig-fn "<function 0x2>"
          :outputs @[:a]}})

(defn mac/push [mac observe]
  (var in-flight @[[:but :low :bcast]])
  (observe ;(first in-flight))

  (while (not (empty? in-flight))
    (def to-handle in-flight)
    (set in-flight @[])
    (loop [[from sig to] :in to-handle
           :let [{:outputs outputs :next-sig-fn next-sig-fn} (mac to)
                 sig (next-sig-fn from sig)]
           :when sig
           output :in outputs]
      (def entry [to sig output])
      (observe ;entry)
      (when (mac output) (array/push in-flight entry)))
    ))

(defn first-signals [str]
  (def arr @[])
  (def mac (mac/new str))
  (mac/push mac |(array/push arr $&))
  arr)

(judge/test (first-signals test-input1)
  @[[:but :low :bcast]
    [:bcast :low :a]
    [:bcast :low :b]
    [:bcast :low :c]
    [:a :high :b]
    [:b :high :c]
    [:c :high :inv]
    [:inv :low :a]
    [:a :low :b]
    [:b :low :c]
    [:c :low :inv]
    [:inv :high :a]])

(judge/test (first-signals test-input2)
  @[[:but :low :bcast]
    [:bcast :low :a]
    [:a :high :inv]
    [:a :high :con]
    [:inv :low :b]
    [:con :high :output]
    [:b :high :con]
    [:con :low :output]])

(defn part1 [str]
  (def mac (mac/new str))
  (def cnts @{:high 0 :low 0})
  (loop [:repeat 1000]
    (mac/push mac (fn [_ sig _] (update cnts sig inc))))
  (product cnts))

(judge/test (part1 test-input1) 32000000)
(judge/test (part1 test-input2) 11687500)
(judge/test (part1 real-input) 806332748)

(defn part2 [str]
  (def mac (mac/new str))
  (def goal :xn)
  (def seen (tabseq [input :in (get-in mac [goal :inputs])]
                    input false))
  (var pushes 0)

  (defn update-seen [from sig to]
    (when (and (= to goal) (= sig :high))
      (put seen from pushes)))

  (while (some false? seen)
    (++ pushes)
    (mac/push mac update-seen))
  (reduce2 |(math/lcm $0 $1) seen))

(judge/test (part2 real-input) 228060006554227)
