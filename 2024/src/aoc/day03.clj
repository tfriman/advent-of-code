(ns aoc.day03)

(def input "resources/day03.txt")

(defn- mul [a b]
  (* (Integer/parseInt a) (Integer/parseInt b)))

(comment
  (def test-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

  (re-find #"\d+" "abc12345def")

  (re-find #"mul\((\d+),(\d+)\)" "mul(2,3)")
  (re-seq #"mul\((\d+),(\d+)\)" test-input)

  (->> (re-seq #"mul\((\d+),(\d+)\)" test-input)
       (reduce (fn [acc [_ a b]] (+ acc (mul a b))) 0))

  (reduce (fn [a [_ b]] (+ a b)) 10 [[1 2] [1 0]]))

(defn p1 []
  (let [lines (clojure.string/split-lines (slurp input))
        all (clojure.string/join "" lines)]
    (->> (re-seq #"mul\((\d+),(\d+)\)" all)
         (reduce (fn [acc [_ a b]] (+ acc (mul a b))) 0)
         )))

(p1)

(defn- part2 [input]
  (loop [in input
         active true
         result 0]
    (if (or (empty? in) (empty? (re-find #"(.*?)mul\((\d+),(\d+)\)" in)))
      result
      (if active
        (let [[whole _ a b] (re-find #"(.*?)mul\((\d+),(\d+)\)" in)
              [nopes _] (re-find #"(.*?)don't\(\)" in)]
          (if (and nopes (> (count whole) (count nopes)))
            (recur (apply str (drop (count nopes) in)) false result)
            (recur (apply str (drop (count whole) in)) true (+ result (mul a b)))
            ))
        (let [letsgo (re-find #"(.*?)do\(\)" in)]
          (if letsgo
            (recur (apply str (drop (count (first letsgo)) in)) true result)
            (recur nil true result)
            ))))))

(comment
  (def test-input "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
  (part2 test-input)

  (re-find #"(.+?)mul\((\d+),(\d+)\)" test-input)
  (re-find #"(.+?)don't\(\)" test-input)
  (re-find #"(.+?)do\(\)" test-input)
  (re-find #"(.+?)do\(\)" "xtest-input")
  (re-find #"(.*?)do\(\)" "do()")
  )

(defn p2 []
  (let [lines (clojure.string/split-lines (slurp input))
        all (clojure.string/join "" lines)]
    (part2 all)))
;; 85508223
(p2)
