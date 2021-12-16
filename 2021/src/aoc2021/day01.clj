(ns aoc2021.day01)

(def example "resources/day01-example.txt")
(def path "resources/day01.txt")
(def input path)

(defn diffs [in]
  (loop [fi (first in)
         sec (second in)
         re (rest in)
         res []]
    (if (not sec)
      res
      (recur (first re) (second re) (rest re) (conj res (- (Integer/parseInt sec) (Integer/parseInt fi))))
      )))

(defn larger-count [in]
  (count (filter #(> % 0) in)))

(defn p1 []
  (let [in (clojure.string/split-lines (slurp input))
        ds (diffs in)
        lc (larger-count ds)]
    (println "result:" lc)))

(defn diffs3 [in]
  (loop [fi (first in)
         fourth (first (next (next (next in))))
         re (rest in)
         res []]
    (if (not fourth)
      res
      (recur (first re) (first (next (next (next re)))) (rest re) (conj res (- (Integer/parseInt fourth) (Integer/parseInt fi))))
      )))

(defn p2 []
  (let [in (clojure.string/split-lines (slurp input))
        ds (diffs3 in)
        lc (larger-count ds)]
    (println "result:" lc)))
