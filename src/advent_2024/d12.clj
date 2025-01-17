(ns advent-2024.d12
  (:require [advent-2024.lib :as lib]
            [clojure.string  :as str]))

(def inp (->> "d12-test1"
              lib/read-file
              str/split-lines
              (map-indexed (fn [i s] [i (zipmap (range) (seq s))]))
              (reduce (fn [acc [i mp]]
                        (reduce (fn [a [j v]]
                                  (assoc a [i j] v))
                                acc
                                mp))
                      (sorted-map))))

(def dirs {:n   [-1 0],
           :e   [0 1],
           :s   [1 0],
           :w   [0 -1]})

(defn add
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn plot-neighbors
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
                             plot-neighbors
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
       (map plot-neighbors)
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

;; (main)


;;---- part2

(defn all-neighbors
  "Takes a point returns neighboring points with the same value"
  [p]
  (->> dirs
       vals
       (map (partial add p))
       (filter (fn [x] (contains? inp x)))))


(defn determine-plot4
  "Takes p and returns a vector of two: a
   plot to which p belongs and bordering points"
  [p]
  (let [crop (get inp p)]
    (loop [plot    (sorted-set)
           border  (sorted-set)
           todo [p]]
      (if-not (seq todo)
        [plot border]
        (let [[x & more] todo]
          (if (= crop (get inp x))
            (recur (conj plot x)
                   border
                   (concat more (->> x all-neighbors (remove plot))))
            (recur plot
                   (conj border x)
                   more)))))))

; (determine-plot4 [1 2])

(defn find-plots2
  [inp]
  (loop [plots   []
         unalloc (-> inp keys set)]
    (if-not (seq unalloc)
      plots
      (let [p     (first unalloc)
            rc (determine-plot4 p)]
        (recur (conj plots rc)
               (apply disj unalloc (first rc)))))))

(def plot-border-pairs (find-plots2 inp))

(def plots (mapv first plot-border-pairs))
(def borders (mapv second plot-border-pairs))
(def plot-map
  (->> (for [i      (range (count plots)),
             p      (get plots i)]
         [p i])
       (into (sorted-map))))
;; {[0 0] 1
;;  [0 1] 1
;;  [0 2] 1
;;  [0 3] 1
;;  [1 0] 2
;;  [1 1] 2
;;  [1 2] 0
;;  [1 3] 4
;;  [2 0] 2
;;  [2 1] 2
;;  [2 2] 0
;;  [2 3] 0
;;  [3 0] 3
;;  [3 1] 3
;;  [3 2] 3
;;  [3 3] 0}
plot-border-pairs

[[#{[1 2] [2 2] [2 3] [3 3]} #{[0 2] [1 1] [1 3] [2 1] [3 2]}]
 [#{[0 0] [0 1] [0 2] [0 3]} #{[1 0] [1 1] [1 2] [1 3]}]
 [#{[1 0] [1 1] [2 0] [2 1]} #{[0 0] [0 1] [1 2] [2 2] [3 0] [3 1]}]
 [#{[3 0] [3 1] [3 2]} #{[2 0] [2 1] [2 2] [3 3]}]
 [#{[1 3]} #{[0 3] [1 2] [2 3]}]]

(->> plot-border-pairs
     (map-indexed (fn [idx [_plot border]]
                    [idx (map plot-map border)])))

(->> (for [b borders] (map plot-map b))
     (map-indexed (fn [idx x]
                    (interleave (repeat idx) x))))

(->>
 (for [i (range (count borders))
       :let [border (get borders i),
             rels   (map plot-map border)]
       rel rels]
   [i rel])
 distinct
 sort)
