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
