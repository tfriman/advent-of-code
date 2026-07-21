(ns aoc.day06)

(def input-example "resources/day06-example.txt")
(def input-real "resources/day06-gitignore.txt")

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

(defn- parse-numbers-row
  [r]
  (let [matcher (re-matcher #"\d+" r)]
    (->>
     (loop [res []]
       (let [nextval (re-find matcher)]
         (if (nil? nextval)
           res
           (recur (conj res nextval)))))
     (mapv ->long))))

(defn- ->operation [s]
  (if (= "*" s)

    (fn ([a b] (* a b)) ([a] a))
    (fn  ([a] a) ( [a b] (+ a b)))))

(defn- parse-opers-row
  [r]
  (let [matcher (re-matcher #"[\*\+]" r)]
    (->>
     (loop [res []]
       (let [nextval (re-find matcher)]
         (if (nil? nextval)
           res
           (recur (conj res nextval)))))
     (mapv ->operation))))

(defn- parse-input-lines
  [lines]
  (let [r (reverse lines)
        opers (parse-opers-row (first r))
        numbers (map parse-numbers-row (rest r))
        ]
    {:opers opers
     :numbers numbers}
    ))

(defn- get-numbers [numbers i]
  (map #(get % i) numbers))

(defn solve-p1 [{:keys [opers numbers]}]
  (for [i (range 0 (count opers))]
    (reduce (get opers i) (map #(get % i) numbers))))

(defn p1 [in]
  (apply + (solve-p1 (parse-input-lines (get-input in)))))

(p1 input-example)
(p1 input-real)

;; ----- p2 ----

(defn- get-indices
  "Assumes raw operations row and 0 based index. Searches the correct one and uses max value too"
  [operationsrow maxindex i]
  (loop [row operationsrow
         found -1
         index 0
         index-start nil]
    (if (> found i)
      [index-start (-> index dec dec dec)]
      (if (empty? row)

        [index-start maxindex]
        (let [current (first row)
              is-op? (not (= \space current))
              new-found (if is-op? (inc found) found)
              new-start (if (and (= new-found i) (nil? index-start)) index index-start)]
          (recur (rest row) new-found (inc index) new-start))))))

(comment
  (def operrow "*   +   *   +")
  (do
    (assert (= [0 2] (get-indices operrow 15 0)))
    (assert (= [8 10] (get-indices operrow 14 2)))
    (assert (= [12 14] (get-indices operrow 15 3))))
  )

(defn- get-numbers-for-index-p2 [numberrows i]
  (printf "index %s %n" i)
  (->> numberrows
       (map #(get % i))
       (apply str)
       clojure.string/trim
       ->long
       ))

(comment
  (def rows (-> input-example
                get-input
                reverse
                rest
                reverse
                ))
  (get-numbers-for-index-p2 rows 0)
  )

(defn- parse-numbers-rows-p2
  "Index based parsing. First searches the indices to look for based on operation index."
  [operationsrow rows maxindex i]
  (printf "parsing for index %s with maxindex %s%n" i maxindex)
  (let [[start stop] (get-indices operationsrow maxindex i)
        _ (printf "i %s indices %s%n" i [start stop])
        ]
    (mapv #(get-numbers-for-index-p2 rows %) (range start (inc stop)))
    ))

(defn- parse-input-lines-p2
  [lines]
  (let [r (reverse lines)
        opers (parse-opers-row (first r))
        _ (def opers opers)
        maxindex (dec (apply max (map count lines)))
        _ (def maxindex maxindex)
        numbers (mapv (partial parse-numbers-rows-p2 (first r) (reverse (rest r)) maxindex) (range 0 (count opers)))]
    {:opers opers
     :numbers numbers}
    ))

(defn solve-p2 [{:keys [opers numbers]}]
  (for [i (range 0 (count opers))]
    (reduce (get opers i) (get numbers i))))


(defn p2 [in]
  (let [input (-> in get-input parse-input-lines-p2)
        _ (def input input)
        output (solve-p2 input)
        _ (def output output)
        ]
    (apply + output)))

(p2 input-example)
(p2 input-real)
