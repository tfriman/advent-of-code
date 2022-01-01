(ns aoc2021.day13
  (:require [clojure.set :as set]))

(def example "resources/day13-example.txt")
(def path "resources/day13.txt")
(def input example)
(def input path)
#_(def in (clojure.string/split-lines (slurp input)))

(defn ->coords
  "Gets x y pairs as longs"
  [s]
  (->> s
   (take-while #(not-empty %))
   (map (fn [e] (let [[_ x y] (re-matches #"(\d+),(\d+)" e)]
                  [(Long/parseLong x) (Long/parseLong y)])))))

(defn fold-left
  [x-axis m]
  (let [{:keys [to-fold keepers]} (group-by (fn [[x y]] (if (> x x-axis) :to-fold :keepers )) m)
        rel                       (* 2 x-axis)
        new                       (map (fn [[x y]] [(- rel x) y]) to-fold)
        result                    (set/union (set new) (set keepers))]
    result))

(defn fold-up
  [y-axis m]
  (let [{:keys [to-fold keepers]} (group-by (fn [[x y]] (if (> y y-axis) :to-fold :keepers )) m)
        rel                       (* 2 y-axis)
        new                       (map (fn [[x y]] [x (- rel y)]) to-fold)
        result                    (set/union (set new) (set keepers))]
    result))

(defn ->foldfn [s]
  (let [[_ axis v] (re-matches #"fold along ([xy])=(\d+)" s)
        vl         (Long/parseLong v)]
    (if (= "y" axis)
      (partial fold-up vl)
      (partial fold-left vl))))

(defn ->foldfns [s]
  (->> s
       (drop-while #(not-empty %))
       rest
       (map ->foldfn)))

(defn p1 []
  (let [in     (clojure.string/split-lines (slurp input))
        coords (->coords in)
        folds  (->foldfns in)
        result (-> coords
                   ((first folds))
                   count)]
    (println "p1 result " result)
    result))

(defn- to-row-str [len s]
  #_(println "to-row-str " s)
  (let [os (char-array (apply str (repeat (inc len) " ")))
        is (map first s)]
    #_(println "is " is)
    (doseq [x is] (aset os x \#))
        #_(dorun (map (fn [x] (aset os x "#")) is))
    (apply str os)))

(defn p2 []
  (let [in      (clojure.string/split-lines (slurp input))
        coords  (->coords in)
        #_ (def coo coords)
        folds   (->foldfns in)
        final   (reduce (fn [x f] (f x)) coords folds)
        xmax    (apply max (map first final))
        #_ (println "xmax" xmax)
        #_ (def fi final)
        by-row  (into (sorted-map) (group-by (fn [[x y]] y) final))
        result (map (fn [[k v]] (to-row-str xmax v)) by-row)]
    (println "p2 result:")
    (doseq [x result] (println x))))
