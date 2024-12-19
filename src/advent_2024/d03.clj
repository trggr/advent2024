(ns advent-2024.d03
  (:require [advent-2024.lib :as lib]))

(defn part1
  [inp]
  (->> inp
       (re-seq #"mul\((\d+),(\d+)\)")
       (reduce (fn [acc [_op x y]]
                 (+ acc (* (parse-long x)
                           (parse-long y))))
               0)))

(defn part2
  [inp]
  (->> inp
       (re-seq #"mul\((\d+),(\d+)\)|don\'t|do")
       (reduce (fn [[total enabled :as acc] [op x y]]
                 (cond (= op "don't") [total false]
                       (= op "do")    [total true]
                       enabled        [(+ total (* (parse-long x)
                                                   (parse-long y)))
                                       enabled]
                       :else acc))
               [0 true])
       first))

(defn main
  []
  (let [inp (->> "d03"
                 lib/read-file)]
    (println (part1 inp)
             (part2 inp))))