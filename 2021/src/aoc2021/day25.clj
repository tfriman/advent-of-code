(ns aoc2021.day25
  (:require [clojure.data.int-map :refer [int-set]]))

(def example "resources/day25-example.txt")
(def example57 "resources/day25-example57.txt")
(def path "resources/day25.txt")
(def input example)
#_(def input example57)
(def input path)

(def in (clojure.string/split-lines (slurp input)))

(def carr (char-array (apply concat in)))

(defn ->east
  "Next idx east"
  [columns idx]
  (let [column (mod idx columns)]
    (if (= (dec columns) column)
      (- idx column)
      (inc idx))))

(defn ->south
  "Next idx south"
  [rows columns idx]
  (let [row (int (/ idx columns))]
    (if (= (dec rows) row)
      (- idx (* row columns))
      (+ idx columns))))

(defn ->indices
  "Gets indices for given c"
  [carr c]
  (set (filter some? (map-indexed (fn [idx itm] (when (= itm c) idx)) carr))))

(defn loop-east [columns carr east]
  (loop [e       (first east)
         r       (rest east)
         changed [] ;; triplets fromidx toidx ch
         ]
    (if (nil? e)
      (do
        #_(println "east changes:" changed)
        changed)
      (let [ne         (->east columns e)
            avail?     (= \. (aget carr ne))
            newchanged (if avail?
                         (conj changed [e ne \>])
                         changed)]
        (recur (first r) (rest r) newchanged)))))

(defn loop-south [rows columns carr south]
  #_(println "loop-south" rows columns south carr)
  (loop [e       (first south)
         r       (rest south)
         changed []]
    (if (nil? e)
      (do
        #_(println "south changes:" changed)
        changed)
      (let [ne         (->south rows columns e)
            avail?     (= \. (aget carr ne))
            newchanged (if avail?
                         (conj changed [e ne \v])
                         changed)]
        (recur (first r) (rest r) newchanged)))))

(defn do-it [rows columns in-carr in-east in-south]
  (loop [carr    in-carr
         east    in-east
         south   in-south
         step    0
         changes 1]
    (println "looping:" step)
    (if (= changes 0)
      step
      (let [east-changes  (loop-east columns carr east)
            _             (doseq [[from to ch] east-changes]
                            (aset carr from \.)
                            (aset carr to ch))
            south-changes (loop-south rows columns carr south)
            all-changes   (concat east-changes south-changes)
            changecount   (count all-changes)
            neweast       (reduce (fn [ec [from to _]]
                                    (-> (disj ec from)
                                        (conj to))) east east-changes)
            newsouth      (reduce (fn [sc [from to _]]
                                    (-> (disj sc from)
                                        (conj to))) south south-changes)]
        (doseq [[from to ch] south-changes]
          (aset carr from \.)
          (aset carr to ch))
        (recur carr neweast newsouth (inc step) changecount)))))

(defn p1 []
  (let [in    (clojure.string/split-lines (slurp input))
        rows  (count in)
        cols  (count (first in))
        carr  (char-array (apply concat in))
        east  (->indices carr \>)
        south (->indices carr \v)
        result (do-it rows cols carr east south)]
    (println "p1 result:" result)
    result))
