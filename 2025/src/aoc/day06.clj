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
