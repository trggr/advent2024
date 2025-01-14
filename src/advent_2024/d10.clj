(ns advent-2024.d10
  (:require [advent-2024.lib :as lib]
            [clojure.string  :as str]))

(def inp (->> "d10"
              lib/read-file
              str/split-lines
              (map-indexed (fn [i s] [i (zipmap (range) (seq s))]))
              (reduce (fn [acc [i mp]]
                        (reduce (fn [a [j v]]
                                  (assoc a [i j] (-> v str parse-long)))
                                acc
                                mp))
                      (sorted-map))))

(def trailheads (->> inp
                     (filter (fn [[_ v]] (zero? v)))
                     (map first)))
;; ([0 2] [0 4] [2 4] [4 6] [5 2] [5 5] [6 0] [6 6] [7 1])

(def dirs {:up     [-1 0],
           :right  [0 1],
           :down   [1 0],
           :left   [0 -1]})

(defn add
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn neighbors
  "Returns collection of neighboring points with +1 values"
  [p]
  (let [v  (get inp p)
        nv (inc v)]
    (->> dirs
         vals
         (map (partial add p))
         (filter (fn [q] (and (contains? inp q)
                              (= nv (get inp q))))))))

(def rc
  (reduce (fn [acc _lvl]
            (concat
             (for [path acc, ngb (neighbors (last path))]
               (conj path ngb))))
          (map vector trailheads)
          (range 1 10)))

(defn main
  []
  (println (->> rc (map (juxt first last)) distinct count)
           (->> rc (map ffirst) count)))
