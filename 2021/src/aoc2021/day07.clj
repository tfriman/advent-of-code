(ns aoc2021.day07)

(def example "resources/day07-example.txt")
(def path "resources/day07.txt")
(def input example)
(def input path)

(defn find-lowest [c-f in t low]
  (let [newres (reduce
                (fn [a i] (if (> low a)
                            (+ a (c-f (Math/abs (- i t))))
                            low)) 0 in)]
    (min low newres)))

(defn find-l [in c-f]
  (let [xl (apply min in)
        xmax (apply max in)
        xran (range xl (inc xmax))
        f-l (partial find-lowest c-f in)]
    (loop [[x & re] xran
           result Integer/MAX_VALUE]
      (if (empty? re)
        result
        (recur re (f-l x result))))))

(defn p1 []
  (let [in (slurp input)
        x (clojure.string/split in  #",")
        ints (map #(Integer/parseInt %) x)
        result (find-l ints identity)
        _ (println "result:" result)]))

(defn cost-f-sum [y]
  (/ (* y (inc y)) 2))

(defn p2 []
  (let [in (slurp input)
        x (clojure.string/split in  #",")
        ints (map #(Integer/parseInt %) x)
        result (find-l ints (memoize cost-f-sum))
        _ (println "result:" result)]))
