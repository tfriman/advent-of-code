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

(defn has-repetitive?
  "Generates all possible partitions for a given string.
  Logic is straightforward, first 2,3... until length is reached.
  The result can be given if all partitions are equal.
  Quite ineffective!"
  [l]
  (let [s (str l)
        len (if (odd? (count s)) (dec (count s)) (count s))
        half (/ len 2)
        all-partitions (map #(partition % % "x" s) (range half 0 -1))
        equal-ones (filter (fn [s] (= 1 (count (into #{} s)))) all-partitions)]
    ;;(println "half" half "all" all-partitions "equal" equal-ones)
    (not (empty? equal-ones))))

(comment
  (do
    (assert (has-repetitive? "12341234"))
    (assert (has-repetitive? "1111"))
    (assert (has-repetitive? "99"))
    (assert (not (has-repetitive? "9")))
    (assert (has-repetitive? "11111"))
    (assert (not (has-repetitive? "12121")))
    (assert (has-repetitive? "2121212121"))
    )

  )

(defn p2 [inputs]
  (-> (filter has-repetitive? (parse-to-numbers inputs))
      (as-> x (apply + x))))

(p2 input)
(comment
  (assert (= 4174379265 (p2 input-example)))
  )
