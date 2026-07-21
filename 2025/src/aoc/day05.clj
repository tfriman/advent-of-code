(ns aoc.day05)

(def input-example "resources/day05-example.txt")
(def input-real "resources/day05-gitignore.txt")

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

(defn- parse-range
  "Returns function to match if is in range. Expects - as a separator"
  [r]
  (let [[_ start-string stop-string](re-matches #"(\d+)-(\d+)" r)
        start (->long start-string)
        stop (->long stop-string)]
    (fn [x] (and (>= x start) (<= x stop)))))

(defn- parse-input
  "Uses empty row as discrimator"
  [lines]
  (let [[ranges ingredients] (split-with #(not (= "" %)) lines)]
    {:ranges (map parse-range ranges)
     :ingredients (map ->long (rest ingredients))}
    ))

(defn p1
  [in]
  (let [{:keys [ranges ingredients] :as inputs} (-> in
                                                    get-input
                                                    parse-input)
        _ (def inp inputs)
        checker-fn (fn [ingredient] (some (fn [f] (f ingredient)) ranges))
        ]
    (->>
     ingredients
     (keep checker-fn)
     count
     )))

(p1 input-example)
(p1 input-real)
(comment
  (def inx (get-input input-example))
  (def x  (p1 input-example))
  (def xr (:ranges x))
  (def xi (:ingredients x))
  )
