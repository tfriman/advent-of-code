(ns aoc2021.day12)

(def short-example "resources/day12-short-example.txt")
(def example "resources/day12-example.txt")
(def example226 "resources/day12-226-example.txt")

(def path "resources/day12.txt")
(def input short-example)
#_(def input example)
(def input example226)
(def input path)

(defn ->vertex [e]
  (let [[_ start end] (re-matches #"(.*)-(.*)" e)]
    [start end]))

(defn add-vertex [m e]
  (let [[start end] (->vertex e)
        dir1  (if (= "start" end)
                m
                (update m start (fn [o] (if (nil? o)
                                          #{end}
                                          (conj o end)))))]
    (update dir1 end (fn [o] (if (nil? o)
                               #{start}
                               (conj o start))))))

(defn no-lower-case-visited-twice? [m]
  (let [revisited (filter (fn [[k v]] (> v 1)) (frequencies m))
        doublelowers (filter #(re-matches #"[a-z]+" (first %)) revisited)]
    (empty? doublelowers)))

(defn lower-case-visited-twice? [m]
  (let [freqs (frequencies m)
        revisited (filter (fn [[k v]] (> v 1)) freqs)
        doublelowers (filter #(re-matches #"[a-z]+" (first %)) revisited)
        doublelowers-single-freqs (merge {1 0} (frequencies (map #(second %) doublelowers)))
        keycount (count (keys doublelowers-single-freqs)) ;; only 1 2 should be here
        sf (get doublelowers-single-freqs 2 0) ;; 2 should be 0 or 1
        fstart (get freqs "start") ;; should be 1
        fend (get freqs "end" 0)
        mx (apply max (keys doublelowers-single-freqs))
        result (and (> 3 keycount)
                    (> 2 sf)
                    (= 1 fstart)
                    (> 2 fend)
                    (> 3 mx))
        ]
    result))

(defn find-paths [vertices fil-fn]
  (let [start (vertices "start")
        start-segments (into #{} (map #(conj '("start") %) start))]
    (loop [segments start-segments
           path-segments-seen start-segments]
      (if (empty? segments)
        (filter #(= "end" (first %) ) path-segments-seen)
        (let [segment (first segments)
              next-vertices (vertices (first segment)) ;; get first segments "last" and find next possible ones.
              next-segment-candidates (map #(conj segment %) next-vertices)
              next-segment-cands-without-small-twice (filter fil-fn next-segment-candidates)
              next-cands-without-seen (filter (complement path-segments-seen) next-segment-cands-without-small-twice)
              new-segments (concat  next-cands-without-seen (rest segments))
              new-path-segments-seen (into path-segments-seen next-cands-without-seen)]
          (recur new-segments new-path-segments-seen))))))

(defn p1 []
  (let [raw (clojure.string/split-lines (slurp input))
        _ (def raw (clojure.string/split-lines (slurp input)))
        vertices (reduce add-vertex {} raw)
        _ (def vertices (assoc (reduce add-vertex {} raw) "end" #{"end"}))
        paths (find-paths vertices no-lower-case-visited-twice?)
        _ (def paths (find-paths vertices no-lower-case-visited-twice?))
        result (count paths)
        ]
    (println "result:" result)
    result))

(defn p2 []
  (let [raw (clojure.string/split-lines (slurp input))
        _ (def raw (clojure.string/split-lines (slurp input)))
        vertices (reduce add-vertex {} raw)
        _ (def vertices (assoc (reduce add-vertex {} raw) "end" #{"end"}))
        paths (find-paths vertices lower-case-visited-twice?)
        _ (def paths (find-paths vertices lower-case-visited-twice?))
        result (count paths)
        ]
    (println "result:" result)
    result))
