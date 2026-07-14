(ns aoc.day03)

(def input-example "resources/day03-example.txt")
(def input-real "resources/day03-gitignore.txt")

(defn get-input [in]
  (-> in
      slurp
      clojure.string/split-lines))

(def in (get-input input-example))

(defn- ->int [s]
  {:pre [(clojure.test/is (not (nil? s)))]}
  (if (char? s)
    (Long/parseLong (str s))
    (if (string? s)
      (Long/parseLong s)
      (if (int? s)
        s
        (throw (RuntimeException. (str "Nopes!" s " type was:" (type s))))))))

(defn- calc-joltage
  "Calc joltage for a row"
  [s]
  {:pre [(string? s)]}
  (:maxseen
   (reduce (fn [{:keys [a b maxseen]} v]
             (if (= :notseen a)
               {
                :a (->int v)
                :b :notseen
                :maxseen (->int v)
                }
               (if (= :notseen b)
                 {
                  :a a
                  :b (->int v)
                  :maxseen (->int (str a v))
                  }
                 (let [maxv (max a b)
                       newmax (->int (str maxv v))]
                   (if (> newmax maxseen)
                     {
                      :a maxv
                      :b (->int v)
                      :maxseen newmax
                      }
                     {
                      :a a
                      :b b
                      :maxseen maxseen
                      }
                     )))))
           {
            :maxseen 0
            :a :notseen
            :b :notseen
            }
           s
           )))

(comment
  (do
    (assert (= 98 (calc-joltage "987654321111111")))
    (assert (= 89 (calc-joltage "811111111111119")))
    (assert (= 78 (calc-joltage "234234234234278")))
    (assert (= 92 (calc-joltage "818181911112111")))
    )
  )

(defn p1 [in]
  (let [input (get-input in)]
    (->> (map calc-joltage input)
         (apply +)
         )))

(print "p1:" (p1 input-real))

(defn search-largest-leftmost
  "Searches leftmost largest number from the string between indices. Returns {:largest x :index i}"
  [s]
  {:pre [(clojure.test/is (string? s))]}
  (let [maxval (-> s
                   frequencies
                   keys
                   sort
                   reverse
                   first)]
    {:largest (->int maxval)
     :index (clojure.string/index-of s maxval)
     }))

(comment
  (do
    (assert (= {:largest 1 :index 0} (search-largest-leftmost "11111")))
    (assert (= {:largest 2 :index 2} (search-largest-leftmost "11211")))
    (assert (= {:largest 2 :index 3} (search-largest-leftmost "111221121")))
    (assert (= {:largest 7 :index 6} (search-largest-leftmost "12345677773")))
    )
  )

(defn calc-joltage-p2
  "Calc joltage for part 2, take 12 digits so that the number is the largest possible one"
  [s]
  ;; Idea is the same basically. Is there a better alternative for the number position?
  ;; Sweep from beginning. Search for largest number before 12th last or the 12th last using the 0 as leftmost index.
  ;; Store the index for the largest possible starting number, it should be the leftmost of those
  ;; matching instances.
  ;; Now you have the most significant bit and you know the leftmost index which is 0 or more. You can now check the 11th
  ;; last position and between 12th fixed number index and that you find the largest number and use the leftmost. That is the new largest.
  (let [len (count s)]
    (->
     (reduce (fn [acc i]
               (println (str "acc:" acc "i:" i))
               (let [_ (def xx {:acc acc :i i})
                     leftmost-index (:index acc)
                     searchable (subs s leftmost-index (inc (- len i)))
                     _ (println (str "searchable:" searchable))
                     {:keys [largest index]} (search-largest-leftmost searchable)
                     ]
                 {:index (+ (:index acc) (inc index))
                  :result (str (:result acc) largest)
                  }
                 ))
             {
              :index 0
              :result ""
              }
             (range  12 0 -1))
     :result
     ->int
     )))

(comment
  (do
    (assert (= 987654321111 (calc-joltage-p2 "987654321111111")))
    (assert (= 811111111119 (calc-joltage-p2 "811111111111119")))
    (assert (= 434234234278 (calc-joltage-p2 "234234234234278")))
    (assert (= 888911112111 (calc-joltage-p2 "818181911112111")))
    )
  )

(defn p2 [in]
  (let [input (get-input in)]
    (->> (map calc-joltage-p2 input)
         (apply +)
         )))

(print "p2:" (p2 input-example))
(print "p2:" (p2 input-real))
