(ns aoc.day11)

(def input-example "resources/day11-example.txt")
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

(def example (get-input input-example))

(def parsed (map parse-cables example))

(defn- find-by-tag [l tag] (-> (filter (fn [p] (= tag (:tag p))) l) first))


(defn- p1 [in]

  (let [input (->> in get-input (map parse-cables))
        start (find-by-tag input "you")
        _ (def start start)]

    (loop [paths (mapv (fn [x] [x]) (:outputs start))
           completed []]
      (do
        (if (empty? paths)
          (count completed)
          (let [path (first paths)
                other-paths (rest paths)
                new-paths (map (fn [p] (conj (vec path) p)) (:outputs (find-by-tag input (last path))))
                new-completed (filter (fn [p] (= "out" (last p))) new-paths)
                new-paths-without-completions (filter (fn [p] (not= "out" (last p))) new-paths)
                ]
            (recur (concat other-paths new-paths-without-completions) (concat completed new-completed))
            ))))))

(p1 input-real)
