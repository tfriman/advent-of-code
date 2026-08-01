(ns aoc.aoccore
  (:gen-class)
  (:require
    [aoc.day09 :as day]
    ))

(defn -main [& args]
  (time (println (day/p2-run))))
