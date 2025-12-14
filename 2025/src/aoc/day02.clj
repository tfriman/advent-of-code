(ns aoc.day02)

(def input-example "resources/day02-example.txt")
(def input "resources/day02.txt")

(defn check-dup [i]
  (let [s (str i)
        len (count s)]
    (if (odd? len)
      nil
      (let [[a b] (partition (/ len 2) s)]
        (when (= a b) i))
      )))

(defn ->range [s]
  (-> s
      (clojure.string/split #"-")
      (as-> x (map Long/parseLong x))
      ((fn [[a b]] (range a (inc b))))))

(defn parse-to-numbers [input]
  (->
    (slurp input)
    (clojure.string/split #",")
    (as-> x (map ->range x)
          (apply concat x))))

(defn p1 [inputs]
  (-> (filter check-dup (parse-to-numbers inputs))
      (as-> x (apply + x))))

(p1 input-example)

(defn check-some-dup
  "If some of the sublists matches, return value"
  [s]
  )

(defn check-all-repetitions [s]
  (some check-some-dup (gen-all-partitions s))
  )
(comment
  (do
    (assert (= true (check-all-repetitions "99")))
    (assert (= true (check-all-repetitions "999")))
    (assert (= true (check-all-repetitions "2121212121")))
    (assert (= true (check-all-repetitions "565656")))
    )
  )
(defn p2 [inputs]
  (parse-to-numbers inputs)
  )

(comment
  (assert (= 4174379265 (p2 input-example)))
  )
