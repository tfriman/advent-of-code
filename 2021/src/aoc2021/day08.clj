(ns aoc2021.day08
  (:require [clojure.set :as set]))

(def example "resources/day08-example.txt")
(def path "resources/day08.txt")
(def input example)
(def input path)

(def unique-counts #{2 3 4 7})

(defn p1 []
  (let [raw (clojure.string/split-lines (slurp input))
        nums (map #(second (re-matches #".*\| (.*)" %)) raw)
        words (flatten (map #(clojure.string/split % #" ") nums))
        lens (map #(count %) words)
        result (count (filter unique-counts lens))
        ]
    (println "result: " result)
    result))

(defn p1-threaded
  "Same as p1 but no intermediate stuff"
  []
  (->> (slurp input)
       clojure.string/split-lines
       (map #(second (re-matches #".*\| (.*)" %)))
       (map #(clojure.string/split % #" "))
       flatten
       (map count)
       (filter #{2 3 4 7})
       count))

(defn solve [[ten four]]
  (let [tens (group-by count (map (fn [x] (set x)) (clojure.string/split ten #" ")))
        fours (map #(set %) (clojure.string/split four #" "))
        k1 (first (get tens 2))
        k4 (first (get tens 4))
        k7 (first (get tens 3))
        k8 (first (get tens 7))
        f2 (set/union k4 k7)
        c2 (get tens 5)
        k2 (first (filter #(= 2 (count (set/difference % f2))) c2))
        c3 (filter #(not= k2 %) c2)
        k3 (first (filter #(set/superset? % k1) c3))
        k5 (first (filter #(not= k3 %) c3))
        c6 (get tens 6)
        k6 (first (filter #(= 7 (count (set/union % k1))) c6))
        c0 (filter #(not= k6 %) c6)
        k0 (first (filter #(= 7 (count (set/union % k4))) c0))
        k9 (first (filter #(not= k0 %) c0))
        mapping  {k0 0
                  k1 1
                  k2 2
                  k3 3
                  k4 4
                  k5 5
                  k6 6
                  k7 7
                  k8 8
                  k9 9
                  }
        nums (map #(get mapping %) fours)
        result (->> nums
                    (map * [1000 100 10 1])
                    (apply +))]
    result))

(defn p2 []
  (let [raw (clojure.string/split-lines (slurp input))
        pairs (map #(rest (re-matches #"(.*) \| (.*)" %)) raw)
        result (->> pairs
                    (map solve)
                    (apply +))]
    (println "result:" result)))
