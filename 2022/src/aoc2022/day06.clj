(ns aoc2022.day06)

(def path-example "resources/day06-example.txt")
(def path "resources/day06.txt")
;;(def input path-example)
(def input path)

(defn- countfilter
  "Pred for checking if input has 4 different chars"
  [c s]
  (not= c (-> (set s) count)))

(defn solve [c]
  (let [inputseq (slurp input)
        ps (partition c 1 inputseq)
        ff (partial countfilter c)
        x (->> (take-while ff ps)
               count
               (+ c))]
  x))

(defn p1 []
  (solve 4))

(defn p2 []
  (solve 14))
