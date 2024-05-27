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
  (peg/compile
    ~{:broadcaster (* "broadcaster" (constant nil) (constant :bcast))
      :typed (* (/ '(set "%&") ,keyword) (/ ':a+ ,keyword))
      :module (+ :broadcaster :typed)
      :outs (group ,(sep ~(/ ':a+ ,keyword) ", "))
      :line (/ (* :module " -> " :outs) ,(fn [t n os] [n @{:type t :outs os}]))
      :lines (/ (group ,(sep ':line "\n")) ,from-pairs)
      :main (* :lines (any "\n") -1)
      #:main (* (any (* :line "\n")) :line (any 1))
      }))

(defn mac/new [str] 
  (def mac (in (peg/match peg str) 0))
  (eachp [id {:outs outs}] mac
    (each out outs
      (if (= :& (get-in mac [out :type]))
        (put-in mac [out :ins id] :low))))
  mac)

(judge/test (mac/new test-input1)
  @{:a @{:outs @[:b] :type :%}
    :b @{:outs @[:c] :type :%}
    :bcast @{:outs @[:a :b :c]}
    :c @{:outs @[:inv] :type :%}
    :inv @{:ins @{:c :low} :outs @[:a] :type :&}})


(defn handle-signal [in-id in-mod in-sig]
  (defn output [sig] (seq [out :in (in-mod :outs)] [out sig]))
  (cond
    (nil? (in-mod :type)) (output in-sig)
    (and
      (= (in-mod :type) :%)
      (= in-sig :low))
    (let [on (in-mod :state)]
      (put in-mod :state (not on))
      (if on (output :low) (output :high)))
    (= (in-mod :type) :&)
    (do
      (put-in in-mod [:ins in-id] in-sig)
      (if (all |(= $ :high) (in in-mod :ins))
        (output :low)
        (output :high)))
    []))

(judge/test (handle-signal :bcast @{:outs [:a :b]} :low) @[[:a :low] [:b :low]])
(judge/test (handle-signal :a @{:outs [:b] :ins {:a :low}} :high) @[[:b :high]])

(defn mac/push [mac]
  (var q @[[:but :low :bcast]])
  (def sent @{:but @[:low]})

  (while (not (empty? q))
    (def to-handle q)
    (set q @[])
    (each [from in-sig to] to-handle
      (each [out-mod out-sig] (handle-signal from (mac to) in-sig)
        (if (nil? (in sent out-mod)) (put sent out-mod @[]))
        (array/push (in sent out-mod) out-sig)
        (if (mac out-mod)
          (array/push q [to out-sig out-mod])))))

  sent)

(judge/test (mac/push (mac/new test-input1))
  @{:a @[:low :low :high]
    :b @[:low :high :low]
    :but @[:low]
    :c @[:low :high :low]
    :inv @[:high :low]})

(judge/test (mac/push (mac/new test-input2))
  @{:a @[:low]
    :b @[:low]
    :but @[:low]
    :con @[:high :high]
    :inv @[:high]
    :output @[:high :low]})

(defn part1 [str]
  (def mac (mac/new str))
  (def cnts @{:high 0 :low 0})
  (repeat 1000
    (let [sigs (mac/push mac)]
      (each sig (mapcat identity sigs)
        (update cnts sig inc))))
  (product cnts))

(judge/test (part1 test-input1) 32000000) 
(judge/test (part1 real-input) 806332748) 

#(def freq/n [[num denom] n] [num (* n denom)])
#(def freq+ [[n1 d1] [n2 d2]]
#  (let [d (math/lcm d1 d2)]
#    [(+ (* n1 (/ d1 d)) (* n2 (/ d2 d))) d]))
#
#(defn handle-freq [from mod freq]
#  (defn output [freq] (seq [out :in (mod :outs)] [out freq]))
#  (cond
#    (nil? (mod :type)) (output freq)
#    (= (in-mod :type) :%) (output
#                            @{:low (freq/n (freq :low) 2)
#                              :high (freq/n (freq :low) 2)
#                              :none (freq+ (freq :high) (freq :none)))
#    (= (in-mod :type) :&)
#    (do
#      (put-in in-mod [:in-freq from] freq)
#      (if (all identity (in in-mod :in-freqs))
#        (output @{:none 0
#                  :low (product (in in-mod :in-freqs)))
#        ))
#    []))
#
#(defn part2 [str]
#  (def mac (mac/new str))
#  (def q @[[:but {:low [1 1] :high [0 0] :none [0 0]} :bcast]])
#  (label
#    result
#    (while (not (empty? q))
#      (def [from in-freq to] (array/pop q))
#      (each [out out-freq] (handle-freq from (mac to) in-freq)
#        (if (not (mac out))
#          (return result out-freq))
#        (array/push q [to out-freq out]))
#      )))

(defn print-ins [ins]
  (each s (values ins)
    (case s :low (prin "0") (prin "1")))
  (print))

(defn part2 [str]
  (def mac (mac/new str))
  (for i 0 100
    (mac/push mac)
    (print-ins ((mac :fb) :ins))
  )
  )

#(judge/test (part2 real-input) nil)
