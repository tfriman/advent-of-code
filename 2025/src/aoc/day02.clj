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

(comment
  (parse-to-numbers input-example)

  (-> (filter check-dup (parse-to-numbers input))
      (as-> x (apply + x))
      )

  (do
    (assert (= nil (check-dup 1)))
    (assert (= 11 (check-dup 11)))
    (assert (= nil (check-dup 121)))
    (assert (= 1212 (check-dup 1212)))
    )
  )


(defn part1 [inputs]
  )

(defn p1 [i]
  (let [lines (clojure.string/split-lines (slurp i))]
    (part1 lines)))


(p1 input-example)
