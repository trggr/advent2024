(ns advent-2024.core
  (:require [advent-2024.d01 :as d01]
            [advent-2024.d02 :as d02]
            [advent-2024.d03 :as d03]
            [advent-2024.d04 :as d04]
            [advent-2024.d05 :as d05]
            [advent-2024.d06 :as d06]))

(defn -main
  []
  (d01/main)
  (d02/main)
  (d03/main)
  (d04/main)
  (d05/main)
  ; (d06/main)
  (println "Day 6: semi-brute force results. Works but slow: 5312 1748")
)
