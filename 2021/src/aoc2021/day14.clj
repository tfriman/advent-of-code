(ns aoc2021.day14
  (:require [criterium.core :as cc]))

(def example "resources/day14-example.txt")
(def path "resources/day14.txt")
(def input example)
#_(def input path)

(defn- ->rule
  "Returns new pairs and extra char"
  [s]
  (let [[_ a b r] (re-matches #"(\w)(\w) -> (\w)" s)
        ca        (.charAt a 0)
        cb        (.charAt b 0)
        cr        (.charAt r 0)]
    {[ca cb] {:new       [[ca cr]
                       [cr cb]]
              :duplicate cr}}))

(defn- cget [rulemap pair cnt]
  (let [{:keys [new duplicate]} (get rulemap pair)]
    {:pairs  {(first new)  cnt
              (second new) cnt}
     :duplicates {duplicate cnt}}))

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level.
  (deep-merge-with + {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
                     {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
  -> {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}"
  [f & maps]
  (apply
   (fn m [& maps]
     (if (every? map? maps)
       (apply merge-with m maps)
       (apply f maps)))
   maps))

(defn- ->init [basemap input]
  (let [parts   (frequencies (partition 2 1 input))
        reduced (frequencies (map second (drop-last parts)))
        new {:pairs parts
             :duplicates reduced}]
    (deep-merge-with + basemap new)))

(defn- solve-one
  "Returns new map"
  [current rulemap]
  (let [pairs (filter #(pos? (second %)) (:pairs current))
        reduced (:reduced current)
        news (for [[pair cnt] pairs] (cget rulemap pair cnt))
        result (reduce (fn [a b] (deep-merge-with + a b)) {:duplicates (:duplicates current)}  news)]
    result))

(defn- loop-until [start rulemap cnt]
  (loop [i 0
         inp start]
    (if (= i cnt)
      inp
      (recur (inc i) (solve-one inp rulemap)))))

(defn- gen-base [rulemap]
  (let [allkeys  (keys rulemap)
        allchars (distinct (map first allkeys))
        keym     (into {} (map (fn [x] {x 0}) allkeys))
        charm    (into {} (map (fn [x] {x 0}) allchars))]
    {:pairs keym
     :duplicates charm }))

(defn solve [cnt]
  (let [in       (clojure.string/split-lines (slurp input))
        template (first in)
        _        (def tem template)
        rules    (->> (drop 2 in)
                      (map ->rule)
                      (apply merge))
        _        (def rm rules)
        newbase  (gen-base rules)
        _        (def newbasex newbase)
        newstart (->init (gen-base rules) template)
        _        (def newstartx newstart)
        rmap     (loop-until newstart rules cnt)
        dups     (reduce (partial merge-with +)  (flatten (map (fn [[[a b] c]] [{a c} {b c}]) (seq (:pairs rmap)))))
        res      (merge-with - dups (:duplicates rmap))
        minv     (apply min (vals res))
        maxv     (apply max (vals res))
        result   (- maxv minv)
        ]
    result))

(defn p1 []
  (let [result (solve 10)]
    (println "p1 result:" result)
    result))

;; 3318837563121 too low
;; 2192039569600
(defn p2 []
  (let [result (solve 40)]
    (println "p2 result:" result)
    result))
