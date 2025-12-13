(ns aoc.day01)

(def input-example "resources/day01-example.txt")
(def input "resources/day01.txt")

(defn- parse-dir [dir]
  (-> dir
      (clojure.string/replace-first #"L" "-")
      (clojure.string/replace-first #"R" "")
      Integer/parseInt))

(defn- part1 [lines]
  (reduce
    (fn [{:keys [counter current]} v]
      (let [new-current (mod (+ current v) 100)
            new-counter (if (= 0 new-current) (inc counter) counter)]
        {:current new-current
         :counter new-counter}
        ))
    {:counter 0, :current 50}
    (map parse-dir lines)))

(defn p1 [i]
  (let [lines (clojure.string/split-lines (slurp i))]
    (part1 lines)))

(defn handle-neg
  "If neg, add 100 until not neg, count how many needed"
  [x start]
  (let [end (mod x 100)
        res (quot (abs (- x end)) 100)
        x (if (= 0 start)
            (dec res)
            res
            )]
    (if (= 0 end)
      (inc x)
      x)))

(comment
  (do
    (assert (= 1 (handle-neg -1 1)))
    (assert (= 0 (handle-neg -1 0)))
    (assert (= 1 (handle-neg -99 1)))
    (assert (= 2 (handle-neg -101 1)))
    (assert (= 2 (handle-neg -199 1)))
    (assert (= 1 (handle-neg -199 0)))
    (assert (= 2 (handle-neg -100 45)))
    ))

(defn handle-pos [x]
  (quot x 100))

(comment
  (do
    (assert (= 1 (handle-pos 101)))
    (assert (= 1 (handle-pos 199)))
    (assert (= 2 (handle-pos 201)))
    (assert (= 2 (handle-pos 299))))
  )

(defn counting-mod [start add]
  ;; lahtoluku on aina 0-99. siihen lisataan jotain, jos tulos on valilla 1-99, noop
  (let [new-sum (+ start add)]
    #_(println "new sum" new-sum)
    (if (<= 1 new-sum 99)
      0
      (if (= 0 new-sum)
        1
        (if (neg? new-sum)
          (handle-neg new-sum start)
          (handle-pos new-sum)
          )))))

(comment
  (do
    (assert (= 1 (counting-mod 95 55)))
    (assert (= 0 (counting-mod 0 -6)))
    (assert (= 1 (counting-mod 0 100)))
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
        #_(println new-current " " new-counter)
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
(p1 input)
#_(p2 input-example)
(p2 input)
