(ns aoc.day04)

(def input-example "resources/day04-example.txt")
(def input-example2 "resources/day04-example-2.txt")
(def input-real "resources/day04-gitignore.txt")

(defn get-input [in]
  (-> in
      slurp
      clojure.string/split-lines))

(defn- ->int [s]
  {:pre [(clojure.test/is (not (nil? s)))]}
  (if (char? s)
    (Long/parseLong (str s))
    (if (string? s)
      (Long/parseLong s)
      (if (int? s)
        s
        (throw (RuntimeException. (str "Nopes!" s " type was:" (type s))))))))

(defn- get-empty-row
  "Pads length with pre and post ."
  [l]
  (apply str (repeat (+ 2 l) ".")))

(defn- pad-row [s]
  (str "." s "."))

(defn- input-to-matrix
  [s]
  (let [width (-> s first count)
        height (count s)]
    (printf "width %s height %s %n" width height)
    (concat
     [(get-empty-row width)]
     (map pad-row s)
     [(get-empty-row width)])))

(defn ->coords
  "Expects padded list. Gives 8 coords around x,y"
  [x y]
  (mapv (fn [[a b]] [(+ x a) (+ y b)]) [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])
  )

(defn- can-be-accessed?
  [s x y]
  (when (= \@ (nth (nth s x) y))
    (let [res (filterv true? (keep (fn [[a b]] (= \@ (nth (nth s a) b))) (->coords x y)))
          ;;_ (println "res foo" res)
          cres (count res)
          result (> 4 cres)
          ]
      (when result
        (printf "x %s y %s can be accessed%n" x y)
        )
      result
      )))

(defn- solve-p1
  [m]
  (printf "m l %s w %s%n" (count (first m)) (count m))
  (for [x (range 1 (- (count (first m)) 1))
        y (range 1 (- (count m) 1))]
    (can-be-accessed? m x y)))

(defn p1 [s]
  (count
   (filterv true?
            (-> s
                get-input
                input-to-matrix
                solve-p1
                )
            )))

(comment
  (p1 input-example)

  (p1 input-example2)

  (p1 input-real))

;; p2 starts

(defn- mark-coords
  [in-m coords]
  (reduce (fn [m [x y]]
            (update-in (vec m) [x] (fn [r]
                                     (->>
                                      (assoc-in (vec r) [y] \.)
                                      (apply str)))))
          in-m
          coords
          )
  )

(defn- solve-p2-one-round
  [m]
  (let [coords (for [x (range 1 (- (count (first m)) 1))
                     y (range 1 (- (count m) 1))
                     :when (can-be-accessed? m x y)]
                 [x y]
                 )
        changed-count (count coords)
        new-m (mark-coords m coords)
        ]

    {:changed changed-count
     :new new-m
     })
  )

(defn- solve-p2
  [m]
  (loop [in m
         latest 1
         acc 0
         ]
    (if (= 0 latest)
      acc
      (let [{:keys [changed new]} (solve-p2-one-round in)]
        (recur new changed (+ acc changed))
        ))))

(defn p2 [s]
  (-> s
      get-input
      input-to-matrix
      solve-p2
      ))

(p2 input-example)

(p2 input-real)
