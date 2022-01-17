(ns aoc2021.day17
  (:require [clojure.set :as set]))

;; target area: x=20..30, y=-10..-5
(def example {:x1 20 :x2 30 :y1 -5 :y2 -10})
;; target area: x=34..67, y=-215..-186
(def actual {:x1 34 :x2 67 :y1 -186 :y2 -215})
(def example-results "resources/day17-example-results.txt")
#_(def input example)
(def input actual)

(defn calc-possible-yrange
  "Assumes negative y values"
  [ymax ymin]
  (let [maxy (* -1 ymin) ;; max steps up
        result (into #{} (range ymin maxy))]
    result))

(defn calc-max-steps [ymin] (* -2 ymin))

(defn- generate-x-steps
  "Starting with n, return x values."
  [maxsteps n upper]
  (take maxsteps (take-while (fn [x] (<= x upper)) (reductions + (iterate (fn [x] (if (pos? x) (dec x) 0)) n)))))

(defn- generate-y-steps
  "Starting with n, return y values."
  [maxsteps lower n]
  (take maxsteps (take-while (fn [x] (<= lower x)) (reductions + (iterate dec n)))))

(defn- find-matching-y
  "Returns seq of pairs steplevel, step# where steplevel [l1, l2]"
  [l1 l2 steps]
  (let [mi     (map-indexed (fn [idx e] [idx e]) steps)
        before (drop-while (fn [[i e]] (< l1 e)) mi)
        result (take-while (fn [[i e]] (<= l2 e)) before)]
    result))

(defn- find-matching-x
  "Returns seq of pairs steplevel, step# where steplevel [l1, l2]"
  [l1 l2 steps]
  (let [mi     (map-indexed (fn [idx e] [idx e]) steps)
        before (drop-while (fn [[i e]] (< e l1)) mi)
        result (take-while (fn [[i e]] (<= e l2)) before)]
    result))

(defn calc-y
  "pairs ystart, step which hit the area"
  [maxstepcount y1 y2]
  (let [yr           (calc-possible-yrange y1 y2)
        steps        (map (fn [x] [x (generate-y-steps maxstepcount y2 x)]) yr)
        reducedsteps (->> steps
                          (map (fn [[x stps]] [x (find-matching-y y1 y2 stps)]))
                          (filter (fn [[x st]] ((complement empty?) st))))
        result       (->> reducedsteps
                          (map (fn [[speed ypairs]] (mapcat (fn [[step yloc]] [speed step]) ypairs)))
                          flatten
                          (partition 2))]
    result))

(defn calc-x
  "Calculate all x start values which result in hitting target x range.
   Result contains seq of pairs: speed, step which hit the area"
  [maxstepcount x1 x2]
  (let [xr           (range 1 (inc x2)) ;; start could be higher
        steps        (map (fn [x] [x (generate-x-steps maxstepcount x x2)]) xr)
        reducedsteps (->> steps
                          (map (fn [[x stps]] [x (find-matching-x x1 x2 stps)]))
                          (filter (fn [[x st]] ((complement empty?) st))))
        result       (->> reducedsteps
                          (map (fn [[speed sxpairs]] (mapcat (fn [[step xloc]] [speed step]) sxpairs)))
                          flatten
                          (partition 2))]
    result))

(defn- pairs-to-map
  "(speed, step) pairs are mapped to {step listofspeeds}"
  [pairs]
  (let [x (group-by second pairs)
        ks (keys x)
        result (into {} (map (fn [k] [k (map first (get x k))]) ks))]
    result))

(defn calc-result [{:keys [x1 x2 y1 y2]}]
  (let [mxstp (calc-max-steps y2)
        xs (calc-x mxstp x1 x2)
        ys (calc-y mxstp y1 y2)
        xm (pairs-to-map xs)
        ym (pairs-to-map ys)
        ks (set/intersection (set (keys xm)) (set (keys ym)))
        joined (for [k ks] [(get xm k) (get ym k)])
        pairs (apply concat (map (fn [[xs ys]] (for [x xs y ys] [x y])) joined))]
    (count (distinct pairs))))

(defn p1 []
  ;; easy math, n*(n+1)/2 is the sum of steps
  (let [miny (* -1 (:y2 input))
        result (/ (* (dec miny) miny) 2)]
    (doto result (#(println "p1 result:" %)))))

(defn p2 []
  (let [result (calc-result input)]
    (doto result (#(println "p2 result:" %)))))

(comment
  (calc-x 9 20 30)
  (defn ->int [s] (Integer/parseInt s))

  (defn ex-read []
    (let [in     (clojure.string/split-lines (slurp example-results))
          coords (map (fn [xin] (let [[_ x y] (re-matches #"(.*),(.*)" xin)] [(->int x) (->int y)])) in)]
      (sort (fn [[x1 y1] [x2 y2]] (let [c (compare x1 x2)]
                                    (if (not= c 0) c
                                        (compare y1 y2))))coords))))
