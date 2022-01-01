(ns aoc2021.day14)

(def example "resources/day14-example.txt")
(def path "resources/day14.txt")
(def input example)
(def input path)

(defn- ->rule
  [s]
  (println "->rule" s)
  (let [[_ a b r] (re-matches #"(\w)(\w) -> (\w)" s)]
    {[(.charAt a 0) (.charAt b 0)]  (.charAt r 0)}))

(defn- cget [rm]
  (memoize (fn [a b] [(get rm [a b]) b])))

(defn- generate [in rulemapfn]
  (let [chars in
        cnt   (alength chars)
        xr    (range 1 (dec cnt))
        fa    (aget chars 0)
        fb    (aget chars 1)]
    (println (java.util.Date.) "length" cnt)
    (char-array (into (vec (flatten [fa (rulemapfn fa fb)]))
                      (flatten
                       (for [x xr]
                         (let [a (aget chars x)
                               b (aget chars (inc x))]
                           (rulemapfn a b))))))))

(defn- loop-to [input rulemap c]
  (let [rulemapfn (cget rulemap)]
    (loop [i 0
           in (char-array input)]
      (println (java.util.Date.) "round" i)
      (if (= i c)
        (apply str in)
        (recur (inc i) (generate in rulemapfn))))))

(defn solve [cnt]
  (let [in            (clojure.string/split-lines (slurp input))
        template      (first in)
        _ (def tem template)
        rules         (->> (drop 2 in)
                           (map ->rule)
                           (apply merge))
        _ (def rm rules)
        resultstr     (loop-to template rules cnt)
        distributions (vals (frequencies (seq resultstr)))
        minv          (apply min distributions)
        maxv          (apply max distributions)
        result        (- maxv minv)]
    result))

(defn p1 []
  (let [result (solve 10)]
    (println "p1 result:" result)
    result))

(defn p2 []
  (let [result (solve 40)]
    (println "p2 result:" result)
    result))
