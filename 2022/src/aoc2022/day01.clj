(ns aoc2022.day01)

(def input "resources/day01.txt")

(defn- parse
  "Get collection of lines, containing empty ones. Return each elves' lines as a string collection"
  [c]
  (->> (partition-by (fn [e] (empty? e)) c)
       (remove (fn [e] (= "" (first e))))))

(defn- calories
  "get col of cols containing calories in string format"
  [c]
  (map (fn [x] (->> (map (fn [y] (Long/parseLong y)) x)
                    (apply +)
                    )) c))

(defn p1 []
  (let [lines (clojure.string/split-lines (slurp input))]
    (apply max (calories (parse lines)))))

(defn p2 []
  (let [lines (clojure.string/split-lines (slurp input))]
    (->> (sort (calories (parse lines)))
         reverse
         (take 3)
         (apply +))))

(comment
  (def t1 ["1" "2" "" "3" "4"])
  (def p1 (parse t1))
  (assert (= ["1" "2"] (first p1)))
  (assert (= ["3" "4"] (last p1))))
