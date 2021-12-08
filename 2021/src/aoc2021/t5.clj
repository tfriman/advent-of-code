(ns aoc2021.t5)

(def example "resources/t5-example.txt")
(def path "resources/t5.txt")
(def input example)
(def input path)

(defn parse-row
  "row: x1,y1 -> x2, y2
  result: {x1: x1, ..}"
  [row]
  (let [strings (rest (re-find #"(\d+),(\d+) -> (\d+),(\d+)" row))
        [x1 y1 x2 y2] (map #(Integer/parseInt %) strings)]
    {:x1 x1 :y1 y1 :x2 x2 :y2 y2}))

(defn whole-range [a1 a2]
  (if (> a1 a2)
    (range a2 (inc a1))
    (range a1 (inc a2))))

(defn vertical-coords [{:keys [x1 y1 x2 y2]}]
  (when (= x1 x2)
    (for [y (whole-range y1 y2)]
      [x1 y])))

(defn horizontal-coords [{:keys [x1 y1 x2 y2]}]
  (when (= y1 y2)
    (for [x (whole-range x1 x2)]
      [x y1])))

(defn diagonal-coords [{:keys [x1 y1 x2 y2]}]
  (let [xd (- x1 x2)
        yd (- y1 y2)]
    (if (= xd yd)
      ;; down
      (map (fn [a b] [a b]) (whole-range x1 x2) (whole-range y1 y2))
      (when (= xd (* -1 yd))
        (map (fn [a b] [a b]) (whole-range x1 x2) (reverse (whole-range y1 y2)))))))

(defn all-coords-p1 [m]
  (concat (vertical-coords m) (horizontal-coords m)))

(defn p1 []
  (let [in (clojure.string/split-lines (slurp input))
        lines (map parse-row in)
        coords (mapcat all-coords-p1 lines)
        #_ (println "coords:" coords)
        freqs (frequencies coords)
        vfreq (dissoc (frequencies (vals freqs)) 1)
        result (reduce + (vals vfreq))]
    (println "result:" result)))

(defn all-coords-p2 [m]
  (concat (vertical-coords m) (horizontal-coords m) (diagonal-coords m)))

(defn p2 []
  (let [in (clojure.string/split-lines (slurp input))
        lines (map parse-row in)
        coords (mapcat all-coords-p2 lines)
        freqs (frequencies coords)
        vfreq (dissoc (frequencies (vals freqs)) 1)
        result (reduce + (vals vfreq))]
    (println "result:" result)))
