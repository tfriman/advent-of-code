(ns aoc.day04)

(def input-ex "resources/day04-example.txt")
(def input-ex-pt2 "resources/day04-example-pt2.txt")
(def input "resources/day04.txt")

(defn- get-diag [lines line-length start-pos direction]
  (if (= :right direction)
    (let [r (range start-pos line-length)]
      (->> (map (fn [line p] (nth line p)) lines r)
           (remove nil?)
           (apply str)))
    (let [r (reverse (range 0 start-pos))]
      (->> (map (fn [line p] (nth line p)) lines r)
           (remove nil?)
           (apply str)))))

(defn- columns [lines]
  (->> (apply map vector lines)
       (map #(apply str %))))

(defn- part1
  [lines]
  (let [line-len (count (first lines))
        all (->> (concat lines
                         (columns lines)
                         (map (fn [pos] (get-diag lines line-len pos :right)) (range 0 (inc line-len)))
                         (map (fn [pos] (get-diag (drop pos lines) line-len 0 :right)) (range 1 line-len))
                         (map (fn [pos] (get-diag lines line-len pos :left)) (range 1 (inc line-len)))
                         (map (fn [pos] (get-diag (drop pos lines) line-len line-len :left)) (range 1 (inc line-len))))
                 (mapcat (juxt identity (fn [line] (apply str (reverse line)))))
                 flatten)]
    (->> (map (fn [line] (count (re-seq #"XMAS" line))) all)
         (apply +))))

(defn p1 []
  (let [lines (clojure.string/split-lines (slurp input))]
    (part1 lines)))

(prn "p1" (p1))

(defn coords [len]
  ;; 0 2
  ;;  1
  ;; 0 2
  (for [x (range 0 (- len 2))]
    [[x (inc x) (+ 2 x)] [(+ 2 x) (inc x) x]]))

;; p2 3x3 where either MAS or SAM is diagonals.
(defn get-cross [lines]
  (let [lines3 (partition 3 1 lines)
        len (count (first lines))
        coord-list (coords len)]
    (map (fn [[u m l]]
           (map (fn [[[c1u c1m c1l] [c2u c2m c2l]]]
                  #_(prn u b)
                  (let [w1 (str
                             (nth u c1u)
                             (nth m c1m)
                             (nth l c1l)
                             )
                        w2 (str
                             (nth u c2u)
                             (nth m c2m)
                             (nth l c2l)
                             )]
                    (into #{} [w1 w2])
                    )
                  ) coord-list)
           ) lines3)))

(prn "p2" (->> (get-cross (clojure.string/split-lines (slurp input)))
               flatten
               (filter (fn [x] (clojure.set/superset? #{"MAS" "SAM"} x)))
               count
               ))
