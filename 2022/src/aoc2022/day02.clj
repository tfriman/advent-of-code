(ns aoc2022.day02)

(def input "resources/day02.txt")

(def rps [:rock :scissors :paper :rock :scissors])

(defn- win-with
  "Find winner for given r/p/s"
  [x]
  (-> (drop-while #(not= x %) rps)
      rest
      first))

(defn- lose-with
  "Find loser for given r/p/s"
  [x]
  (-> (drop-while #(not= x %) rps)
      rest
      rest
      first))

(def p2mapping {\X win-with
                \Y identity
                \Z lose-with
                })

(def firstmap {\A :rock
               \B :paper
               \C :scissors
               \X :rock
               \Y :paper
               \Z :scissors})

(defn- parse-line [l]
  (let [[a _ b] l
        am (get firstmap a)
        bm (get firstmap b)]
    [am bm]))

(defn- parse-line-p2 [l]
  (let [[a _ b] l
        am (get firstmap a)
        bm (get p2mapping b)]
    [am (bm am)]))

(def resultmap {[:rock :rock]         (+ 1 3)
                [:rock :paper]        (+ 2 6)
                [:rock :scissors]     (+ 3 0)
                [:paper :rock]        (+ 1 0)
                [:paper :paper]       (+ 2 3)
                [:paper :scissors]    (+ 3 6)
                [:scissors :rock]     (+ 1 6)
                [:scissors :paper]    (+ 2 0)
                [:scissors :scissors] (+ 3 3)})

(defn- calc-single
  [line]
  (let [[f s] (parse-line line)]
    (get resultmap [f s])))

(defn- calc-single-p2
  [line]
  (let [[f s] (parse-line-p2 line)]
    (get resultmap [f s])))

(defn- calc-all
  "Calculate winner"
  [lines]
  (->> (map calc-single lines)
       (apply +)))

(defn- calc-p2
  "Calculate winner"
  [lines]
  (->> (map calc-single-p2 lines)
       (apply +)))

(defn p1 []
  (let [lines (clojure.string/split-lines (slurp input))]
    (calc-all lines)))

(defn p2 []
  (let [lines (clojure.string/split-lines (slurp input))]
    (calc-p2 lines)))
