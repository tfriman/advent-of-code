(ns aoc.day04)

(def input "resources/day04-example.txt")
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

(p1)