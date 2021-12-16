(ns aoc2021.day04
  (:require [clojure.set :refer [superset?]]))

(def example "resources/day04-example.txt")
(def path "resources/day04.txt")
(def input example)
#_(def input path)

(def targets [#{0 1 2 3 4}
              #{5 6 7 8 9}
              #{10 11 12 13 14}
              #{15 16 17 18 19}
              #{20 21 22 23 24}
              #{0 5 10 15 20}
              #{1 6 11 16 21}
              #{2 7 12 17 22}
              #{3 8 13 18 23}
              #{4 9 14 19 24}])

(defn win? [{:keys [hits] :as orig}]
  (some #(if (superset? hits %) orig) targets))

(defn update-board [draw {:keys [all foundsum indexed hits] :as orig}]
  (if (contains? all draw)
    (let [i-d (get indexed draw)
          res (conj hits i-d)]
      {:all all
       :indexed indexed
       :foundsum (+ foundsum (Integer/parseInt draw))
       :hits res}
      )
    orig))

(defn ->board
  "6 rows, first is always empty"
  [rows]
  (let [x (rest rows)
        mat (mapcat (fn [i]
                      (-> i
                          clojure.string/trim
                          (clojure.string/replace #"  " " ")
                          (clojure.string/split #" "))) x)
        idx-map (zipmap mat (iterate inc 0))]
    {:raw mat
     :all (into #{} mat)
     :indexed idx-map
     :foundsum 0
     :hits #{}}))

(defn check [boards-in draws-in]
  (loop [draw (first draws-in)
         boards boards-in
         draws (rest draws-in)]
    (let [updated-boards (map #(update-board draw %) boards)
          win (some #(win? %) updated-boards)]
      (if (or win (empty? draws))
        [win draw]
        (recur (first draws) updated-boards (rest draws))))))

(defn ->boards [rows]
  (let [parts (partition 6 rows)]
    (mapv (fn [p] (->board p)) parts)))

(defn p1 []
  (let [raw (clojure.string/split-lines (slurp input))
        draws (clojure.string/split (first raw) #",")
        boards (->boards (rest raw))
        [winboard windraw] (check boards draws)
        totalsum (apply + (map #(Integer/parseInt %) (:all winboard)))
        result (* (Integer/parseInt windraw) (- totalsum (:foundsum winboard)))]
    (println "result:" result)))

(defn check-last [boards-in draws-in]
  (loop [draw (first draws-in)
         boards boards-in
         draws (rest draws-in)]
    (let [updated-boards (map #(update-board draw %) boards)
          no-wins (filter #((complement win?) %) updated-boards)]
      (if (empty? no-wins)
        [(first updated-boards) draw]
        (recur (first draws) no-wins (rest draws))))))

(defn p2 []
  (let [raw (clojure.string/split-lines (slurp input))
        draws (clojure.string/split (first raw) #",")
        boards (->boards (rest raw))
        [winboard windraw] (check-last boards draws)
        totalsum (apply + (map #(Integer/parseInt %) (:all winboard)))
        result (* (Integer/parseInt windraw) (- totalsum (:foundsum winboard)))]
    (println "result:" result)))
