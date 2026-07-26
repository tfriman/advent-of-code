(ns aoc.aoccore
  (:gen-class)
  (:require
    [aoc.day08 :as day]
    ))

(defn -main [& args]
  (time (println (day/p2-run))))
