(ns aoc2022.day03)

(def path-example "resources/day03-example.txt")
(def path "resources/day03.txt")
;;(def input path-example)
(def input path)

(defn- parse
  "parse raw input line to two seqs"
  [line]
  (let [len (/ (count line) 2) ;; even length always
        a (take len line)
        b (drop len line)
        ]
    [a b]))

(defn- common
  "find common element in two seqs"
  [[as bs]]
  (let [s1 (into #{} as)
        s2 (into #{} bs)]
    (-> (clojure.set/intersection s1 s2)
        first)))


(def priorities (merge (into {} (map (fn [i] [(char (+ i 64)) (+ 26 i)]) (range 1 27)))
                       (into {} (map (fn [i] [(char (+ i 96)) i]) (range 1 27)))))

(defn- parse-p2
  "Get group of 3 elves rucksacks"
  [[l1 l2 l3]]
  (let [s1 (into #{} l1)
        s2 (into #{} l2)
        s3 (into #{} l3)]
    (-> (clojure.set/intersection s1 s2 s3)
        first)))

(defn p1 []
  (let [lines   (clojure.string/split-lines (slurp input))
        parsed  (map parse lines)
        commons (map common parsed)]
    (apply + (map (fn [x] (get priorities x)) commons))))

(defn p2 []
  (let [lines   (partition 3 (clojure.string/split-lines (slurp input)))
        parsed  (map parse-p2 lines)]
        (apply + (map (fn [x] (get priorities x)) parsed))))
