(defproject aoc2021 "0.1.0-SNAPSHOT"
  :main aoc2021.aoccore
    :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/data.priority-map "1.1.0"]
                 [org.clojure/data.int-map "1.0.0"]
                 [net.mikera/core.matrix "0.62.0"]
                 [criterium "0.4.6"]]
    :repl-options {:init-ns aoc2022.aoccore2022})
