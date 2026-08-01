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
            new-area (max found-max-area area)]
        (recur (first r) (rest r) new-area)))))

(defn p1 [in]
  (let [coords (->> in get-input (map parse-coords))]
    (calc-areas coords)))

(comment
  (p1 input-example)
  (p1 input-real))

(defn- calc-corners
  "Calc two missing corner coords for rectangle"
  [c1 c2]
  (for [x (map :x [c1 c2])
        y (map :y [c1 c2])]
    {:x x :y y}))

(comment
  (do
    (assert (= #{{:x 1 :y 1} {:x 2 :y 2} {:x 1 :y 2} {:x 2 :y 1}} (into #{} (calc-corners {:x 1 :y 2} {:x 2 :y 1}))))
    (assert (= #{{:x 1 :y 1} {:x 2 :y 2} {:x 1 :y 2} {:x 2 :y 1}} (into #{} (calc-corners {:x 1 :y 1} {:x 2 :y 2}))))
    (assert (= #{{:x 1 :y 1} {:x 2 :y 2} {:x 1 :y 2} {:x 2 :y 1}} (into #{} (calc-corners {:x 2 :y 1} {:x 1 :y 2}))))
    ))

(defn- create-vectors
  "Assume coords in correct order meaning c1-c2, c2-c3 etc"
  [coords]
  (let [cf (first coords)
        cl (last coords)
        ;; cl cf is the same direction!
        pairs (conj (partition 2 1 coords) [cl cf])
        ]
    (map (fn [[{x1 :x y1 :y :as c1} {x2 :x y2 :y :as c2}]]
           (let [horizontal (= y1 y2)]
             {:start c1
              :end c2
              :horizontal? horizontal
              :length (inc (abs (+ (- x1 x2) (- y1 y2))))
              :min (if horizontal (min x1 x2) (min y1 y2))
              :max (if horizontal (max x1 x2) (max y1 y2))
              :axis (if horizontal y1 x1)
              }))
         pairs)))

(defn- bounding-box-limits
  "Calculates horizontal and vertical range for the diagonals"
  [{x1 :x y1 :y :as c1} {x2 :x y2 :y :as c2}]
  {
   :horizontal-min (min x1 x2)
   :horizontal-max (max x1 x2)
   :vertical-min (min y1 y2)
   :vertical-max (max y1 y2)
   }
  )

(defn- find-within?
  "Can be used to filter within bounding box."
  [axis-min axis-max non-axis-min non-axis-max {min-h :min max-h :max axis :axis}]
  (not
   (and
    () ;; check that there is something on top of this axis
    (< axis-min axis axis-max) ;; exclude "borders"!
    (or
     (and ;; lower bound, does exclude exact hit from lower
      (> non-axis-max max-h)
      (> max-h non-axis-min))
     (>= non-axis-max max-h min-h non-axis-min) ;; middle bound, "within"
     (and ;; upper bound, does exclude exact hit from upper
      (> min-h non-axis-min)
      (> non-axis-min min-h))
     (and ;; overlapping, "super set"
      (>= non-axis-min min-h)
      (>= max-h non-axis-max))))))

(defn- has-outside?
  "Checks that the area is filled. Assumes axis lines and accordingly vectors.
  There must be one within the min or lower and one within the max or higher.
  Simple logic: must have something in the borders."
  [within-range axis-min axis-max]
  ;; TODO find axes and min and max and check those. it is enough?
  (let [below (filter (fn [c] (>= axis-min (:axis c))) within-range)
        above (filter (fn [c] (>= (:axis c) axis-max)) within-range)
        _ (def above above)
        _ (def below below)
        ]
    (and (not-empty below) (not-empty above))))

(defn- filter-by-edge
  "Filter by edge val, has to have something in the range."
  [edge-val vectors]
  (filter (fn [{min-h :min max-h :max}]
            (>= max-h edge-val min-h)
            )
          vectors))

(defn- has-crosses?
  "Search for possible vertical or horizontal endings within the box"
  [vectors {:keys [horizontal-min horizontal-max vertical-min vertical-max]} ]
  (let [groups (group-by (fn [x] (if (:horizontal? x) :horizontal :vertical)) vectors)
        horizontal-vectors (:horizontal groups)
        vertical-vectors (:vertical groups)
        horizontal-matches (remove
                            (partial find-within? vertical-min vertical-max horizontal-min horizontal-max)
                            horizontal-vectors)
        vertical-matches (remove
                          (partial find-within? horizontal-min horizontal-max vertical-min vertical-max)
                          vertical-vectors)
        ]
    (not
     (and (empty? horizontal-matches)
          (empty? vertical-matches)
          (has-outside? (filter-by-edge vertical-min vertical-vectors) horizontal-min horizontal-max)
          (has-outside? (filter-by-edge vertical-max vertical-vectors) horizontal-min horizontal-max)
          (has-outside? (filter-by-edge horizontal-min horizontal-vectors) vertical-min vertical-max)
          (has-outside? (filter-by-edge horizontal-max horizontal-vectors) vertical-min vertical-max)
          ))))

(comment
  (do
    (def inx (->> input-example get-input (map parse-coords)))
    (def vecs (create-vectors inx))
    (def bbox (bounding-box-limits {:x 7 :y 1} {:x 11 :y 7}))
    (assert (true? (has-crosses? vecs (bounding-box-limits {:x 7 :y 1} {:x 11 :y 7}))))
    (def bbox (bounding-box-limits {:x 9 :y 5} {:x 2 :y 3}))
    (assert (false? (has-crosses? vecs (bounding-box-limits {:x 9 :y 5} {:x 2 :y 3}))))
    (def bbox (bounding-box-limits {:x 9, :y 7} {:x 2, :y 5}))
    (assert (true? (has-crosses? vecs (bounding-box-limits {:x 9, :y 7} {:x 2, :y 5}))) "-, corner failed"))
  )

(defn- filter-coords-by-overlapping-box
  [all-vectors reference coords]
  (remove (fn [c] (has-crosses? all-vectors (bounding-box-limits reference c))) coords))

(defn- calc-area-if-in
  "Assume at least 3 in the borders to be in"
  [coords]
  (let [all-vectors (create-vectors coords)]
    (loop [reference (first coords)
           r (rest coords)
           area 0
           corners []
           ]
      (if (empty? r)
        [area corners]
        (let [filtered-coords (filter-coords-by-overlapping-box all-vectors reference r)
              all-areas (map (partial calc-area reference) filtered-coords)
              found-max-area (if (empty? all-areas) 0 (apply max all-areas))
              new-area (max found-max-area area)
              new-corners (if (> new-area area) [reference]
                              (if (= new-area area)
                                (conj corners reference)
                                corners))]
          (recur (first r) (rest r) new-area new-corners))))))

(defn p2 [in]
  (let [coords (->> in get-input (map parse-coords))]
    (calc-area-if-in coords)))


(defn p2-run [] (first (p2 input-real)))
