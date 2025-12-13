(ns aoc.day01)

(def input-example "resources/day01-example.txt")
(def input "resources/day01.txt")

(def example1 (clojure.string/split-lines (slurp input-example)))
(def input1 (clojure.string/split-lines (slurp input)))

(defn- parse-dir [dir]
  (-> dir
      (clojure.string/replace-first #"L" "-")
      (clojure.string/replace-first #"R" "")
      Integer/parseInt))

(defn- part1 [lines]
  (reduce
    (fn [{:keys [counter current]} v]
      (let [new-current (mod (+ current v) 100)
            new-counter (if (= 0 new-current) (inc counter) counter)
            ]
        {:current new-current
         :counter new-counter}
        )

      )
    {:counter 0, :current 50}
    (map parse-dir lines))
  )

(defn p1 [i]
  (let [lines (clojure.string/split-lines (slurp i))]
    (part1 lines)))

(defn counting-mod- [start add]
  (let [
        x (+ start add)
        modx (mod x 100)
        totals (- x modx)
        rounds (/ totals 100)
        ]
    {:x      x
     :modx   modx
     :totals totals
     :rounds rounds
     :result (abs rounds)}
    ))

(defn make-right
  "Changes left rotation to be towards right"
  [n]
  (if (pos-int? n)
    n
    (let [a (mod n 100)
          ]
      (+ (* -1 n) (* 2 (- a 50)))
      )
    ))

(comment
  (do
    (assert (= 145 (make-right -155)))
    (assert (= 155 (make-right 155)))
    )
  )

(defn counting-mod [start add]
  (let [
        x (+ start add)
        q (quot (make-right x) 100)
        ]
    (abs q)
    ))

(comment
  (do
    (assert (= 1 (counting-mod 95 55)))
    (assert (= 0 (counting-mod 0 -5)))
    (assert (= 1 (counting-mod 95 5)))
    (assert (= 1 (counting-mod 5 95)))
    (assert (= 0 (counting-mod 95 -55)))
    (assert (= 0 (counting-mod 15 55)))
    (assert (= 3 (counting-mod 95 255)))
    (assert (= 2 (counting-mod 95 -200)))
    (assert (= 1 (counting-mod 95 -155)))
    (assert (= 10 (counting-mod 50 1000)))
    (assert (= 10 (counting-mod 0 1000)))
    (assert (= 10 (counting-mod 50 -1000)))
    (assert (= 2 (counting-mod 45 155)))
    (assert (= 1 (counting-mod 45 56)))
    (assert (= 1 (counting-mod 45 -46)))
    (assert (= 2 (counting-mod 45 -145)))
    (assert (= 1 (counting-mod 45 -45)))
    )
  )

(defn- part2 [lines]
  (reduce
    (fn [{:keys [counter current]} v]
      (let [new-current (mod (+ current v) 100)
            new-counter (+ counter (counting-mod current v))
            ]
        (println new-current " " new-counter)
        {:current new-current
         :counter new-counter}
        )
      )
    {:counter 0, :current 50}
    (map parse-dir lines))
  )
(defn p2 [i]
  (let [lines (clojure.string/split-lines (slurp i))]
    (part2 lines)))

#_(p1 input-example)
#_(p1 input)
(p2 input-example)
#_(p2 input)
