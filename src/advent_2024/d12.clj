(ns advent-2024.d12
  (:require [advent-2024.lib :as lib]
            [clojure.string  :as str]))

(def inp (->> "d12"
              lib/read-file
              str/split-lines
              (map-indexed (fn [i s] [i (zipmap (range) (seq s))]))
              (reduce (fn [acc [i mp]]
                        (reduce (fn [a [j v]]
                                  (assoc a [i j] v))
                                acc
                                mp))
                      (sorted-map))))

(def dirs {:up     [-1 0],
           :right  [0 1],
           :down   [1 0],
           :left   [0 -1]})

(defn add
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn neighbors
  "Takes a point returns neighboring points with the same value"
  [p]
  (let [v  (get inp p)]
    (->> dirs
         vals
         (map (partial add p))
         (filter (fn [x] (and (contains? inp x)
                              (= v (get inp x))))))))

;; if a point from plot A has a new, and unknown before, neighbor
;; which belongs to plot B, then A and B are part of the same plot

(defn determine-plot
  "Takes point p and returns a collection of points comprising a plot"
  [p]
  (loop [plot    (sorted-map p 0)
         todo    [p]
         step    1]
    (if-not (seq todo)
      plot
      (let [[p & more]  todo
            nbs         (->> p
                             neighbors
                             (remove (fn [x] (contains? plot x))))]
        (recur (reduce (fn [acc x] (assoc acc x step)) plot nbs)
               (concat more nbs)
               (inc step))))))

(defn find-plots
  [inp]
  (loop [plots   []
         unalloc (-> inp keys set)]
    (if-not (seq unalloc)
      plots
      (let [p       (first unalloc)
            plot    (determine-plot p)]
        (recur (conj plots plot)
               (apply disj unalloc (keys plot)))))))

(defn area
  [plot]
  (count plot))

(defn perimeter
  [plot]
  (->> plot
       keys
       (map neighbors)
       (map count)
       (map (partial - 4))
       (reduce +)))

(defn price
  [plot]
  (* (area plot)
     (perimeter plot)))

(defn main
  []
  (let [plots (find-plots inp)]
    (println (->> plots (map price) (reduce +)))))

(main)