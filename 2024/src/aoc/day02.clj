(ns aoc.day02)

(def input "resources/day02.txt")

(def safe1 "7 6 4 2 1")
(def safe2 "1 3 6 7 9")
(def unsafe1 "1 2 7 8 9")
(def unsafe2 "9 7 6 2 1")
(def unsafe3 "1 3 2 4 5")
(def unsafe4 "8 6 4 4 1")

(defn- parse-int [line]
  (->>
    (clojure.string/split line #" ")
    (map (fn [x] (Integer/parseInt x)))))

(defn- diffs [line]
  (->> line
       (partition 2 1)
       (map (fn [[a b]] (- a b)))))

(defn safe? [line]
  (let [in (diffs line)
        mi (apply min in)
        ma (apply max in)
        mm (apply max (map (fn [x] (Math/abs x)) in))]
    (if (> mm 3)
      false
      (or (and (neg? mi) (neg? ma)) (and (pos? mi) (pos? ma))))))

(defn- drop-nth
  "Expects n > 0"
  [n coll]
  (let [bef (take (dec n) coll)
        after (drop n coll)]
    (flatten (conj after bef))))

(comment
  (drop-nth 1 [1 2 3 4])
  (drop-nth 2 [1 2 3 4])
  (drop-nth 3 [1 2 3 4])
  (drop-nth 4 [1 2 3 4])
  )

(defn- tolerate?
  "Brute force, drop one by one and stop if ok."
  [line]
  (loop [c 1
         result false]
    (if result
      true
      (if (> c (count line))
        result
        (recur (inc c) (safe? (drop-nth c line)))))))

(defn- safe2? [line]
  (if (safe? line)
    true
    (tolerate? line)))

(comment
  (do
    (def safes (map parse-int [safe1 safe2 unsafe3 unsafe4]))
    (def unsafes (map parse-int [unsafe1 unsafe2]))
    (tolerate? (parse-int unsafe3))
    (assert (every? true? (map safe2? safes)))
    (assert (every? false? (map safe2? unsafes)))
    (count (first safes))))

(defn- part1 [lines]
  (count (filter safe? lines)))

(defn- part2 [lines]
  (count (filter safe2? lines)))

(defn p1 []
  (let [lines (map parse-int (clojure.string/split-lines (slurp input)))]
    (part1 lines)))

(defn p2 []
  (let [lines (map parse-int (clojure.string/split-lines (slurp input)))]
    (part2 lines)))