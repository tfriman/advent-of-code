(ns aoc.day11)

(def input-example "resources/day11-example.txt")
(def input-example2 "resources/day11-example-p2.txt")
(def input-real "resources/day11-gitignore.txt")

(defn get-input [in]
  (-> in
      slurp
      clojure.string/split-lines))

(defn- parse-cables
  [r]
  (let [[_ tag outputs] (re-find #"(.*): (.*)" r)
        outs (clojure.string/split outputs #" ")]
    {:tag tag
     :outputs outs
     :outnode (= (first outs) "out")
     }))

(defn- parse-cables-p2
  [r]
  (let [[_ tag outputs] (re-find #"(.*): (.*)" r)
        outs (clojure.string/split outputs #" ")]
    {tag outs}))


(def example (get-input input-example))

(def parsed (map parse-cables example))

(defn- find-by-tag [l tag] (-> (filter (fn [p] (= tag (:tag p))) l) first))


(defn- p1
  "Returns list of matching nodes"
  [input tag-from tag-to]

  (let [start (find-by-tag input tag-from)]

    (loop [paths (mapv (fn [x] [x]) (:outputs start))
           completed []]
      (do
        (if (empty? paths)
          completed
          (let [path (first paths)
                other-paths (rest paths)
                new-paths (map (fn [p] (conj (vec path) p)) (:outputs (find-by-tag input (last path))))
                new-completed (filter (fn [p] (= tag-to (last p))) new-paths)
                new-paths-without-completions (filter (fn [p] (not= tag-to (last p))) new-paths)
                ]
            (recur (concat other-paths new-paths-without-completions) (concat completed new-completed))))))))

(count (p1 (->> input-real get-input (map parse-cables)) "you" "out"))

;; P2 paths start from srv and finally end to out. They need to visit dac and fft in the middle.
;; Result can be gotten by searching following paths:
;; 1: srv to dac
;; 2: srv to fft
;; 3: dac to fft
;; 4: fft to dac
;; 5: dac to out
;; 6: fft to out
;; Result can be calculated like this: R1 * R3 * R5 + R2 * R4 * R6

(defn get-count-gen [input]
  (let [f (memoize (fn [self from to] ;; This is a way create recursive memoized lambdas
                   (if (= from to)
                     1
                     (reduce +' (map #(self self % to) (get input from [])))
                     )))]
    (fn [from to] (f f from to))))

(defn p2 [in]
  (let [input (->> in get-input (map parse-cables-p2) (into {}))
        get-count (get-count-gen input)]
    (+' (*' (get-count "svr" "dac") (get-count "dac" "fft") (get-count "fft" "out"))
        (*' (get-count "svr" "fft") (get-count "fft" "dac") (get-count "dac" "out"))
        )
    ))

(p2 input-example2)

(p2 input-real)
