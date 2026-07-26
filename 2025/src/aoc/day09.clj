(ns aoc.day09
  (:require [clojure.math :as math]
            [clojure.test]))

(def input-example "resources/day09-example.txt")
(def input-real "resources/day09-gitignore.txt")

(defn get-input [in]
  (-> in
      slurp
      clojure.string/split-lines))

(defn- ->long [s]
  {:pre [(clojure.test/is (not (nil? s)))]}
  (if (char? s)
    (Long/parseLong (str s))
    (if (string? s)
      (Long/parseLong s)
      (if (int? s)
        s
        (throw (RuntimeException. (str "Nopes!" s " type was:" (type s))))))))

(defn- parse-coords
  "Parse x,y to map"
  [r]
  (let [[_ xs ys] (re-find #"(\d+),(\d+)" r)]
    {
     :x (->long xs)
     :y (->long ys)
     }
    ))

(defn- calc-area
  [{x1 :x y1 :y} {x2 :x y2 :y}]
  (-> (* (inc (abs (- x1 x2))) (inc (abs (- y1 y2))))))

(comment
  (do
    (assert (= {:x 11, :y 23} (parse-coords "11,23")))
    (assert (= 50 (calc-area (parse-coords "2,5") (parse-coords "11,1"))))
    (assert (= 50 (calc-area (parse-coords "11,1") (parse-coords "2,5") )))
    (assert (= 6 (calc-area (parse-coords "7,3") (parse-coords "2,3"))))
    (assert (= 35 (calc-area (parse-coords "7,1") (parse-coords "11,7"))))
    )
  )

(defn- calc-areas
  "Calc all coord pair areas"
  [coords]
  (loop [reference (first coords)
         r (rest coords)
         area 0]
    (if (empty? r)
      area
      (let [all-areas (map (partial calc-area reference) r)
            found-max-area (apply max all-areas)
            new-area (max found-max-area area)
            ]
        (recur (first r) (rest r) new-area)
        ))))

(defn p1 [in]
  (let [coords (->> in get-input (map parse-coords))]
    (calc-areas coords)
    ))

(comment
  (p1 input-example)
  (p1 input-real)
  )
