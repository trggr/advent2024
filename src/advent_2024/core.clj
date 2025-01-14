(ns advent-2024.core
  (:require [advent-2024.d01 :as d01]
            [advent-2024.d02 :as d02]
            [advent-2024.d03 :as d03]
            [advent-2024.d04 :as d04]
            [advent-2024.d05 :as d05]
            ; [advent-2024.d06 :as d06]
            ; [advent-2024.d07 :as d07]
            [advent-2024.d08 :as d08]
            [advent-2024.d09 :as d09]
            [advent-2024.d10 :as d10]
            [advent-2024.d11 :as d11]
            [advent-2024.d12 :as d12]
            [advent-2024.d13 :as d13]))

(defn -main
  []
  (d01/main)
  (d02/main)
  (d03/main)
  (d04/main)
  (d05/main)
  ;; Day 6: using brute force, it works but slow
  (println 5312 1748)
  ;; Day 7: slow solution
  (println 5837374519342 492383931650959)
  (d08/main)
  (d09/main)
  (d10/main)
  (d11/main)
  (d12/main)
  (d13/main)
)
