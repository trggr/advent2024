;; Solved the first part qucickly.
;;
;; Got stuck really bad on a second one.

;; I was able to handle the first several cases, but was stuck on a test
;; case with Es and Xs
;; After 267 attempts to use all kinds of tricks to connect blocks, walk along them,
;; connect their sides, define compatible sides, connecting compatible sides,
;; definining one-fers, two-fers and three-fers, defining island plots,
;; subtracting their perimeter from the host, calculating the external
;; perimeters of each plot and on and on.
;;
;; After two weeks, I finally gave up and found a solution by tschady:
;; https://github.com/tschady/advent-of-code/blob/main/src/aoc/2024/d12.clj
;;
;; which uses the idea that the perimeter of a polygon is equal to the
;; number of its internal angles. I borrowed two functions from that solution:
;; - corner
;; - count-angles
;;


(ns advent-2024.d12
  (:require [advent-2024.lib :as lib]
            [clojure.string  :as str]
            [clojure.set     :as set]
            [clojure.math.combinatorics :as combo]))

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

(def dirs {:n   [-1  0],
           :ne  [-1  1],
           :e   [0  1],
           :se  [1  1],
           :s   [1  0],
           :sw  [1 -1],
           :w   [0 -1]
           :nw  [-1 -1]})

(defn add
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn neighboring-squares
  "Takes p, returns neighboring points"
  [p]
  (->> (select-keys dirs [:n :e :s :w])
       vals
       (map (partial add p))))


(defn plot-neighbors
  "Takes a point returns neighboring points with the same value"
  [p]
  (let [v  (get inp p)]
    (->> p
         neighboring-squares
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

(defn part1 [inp]
  []
  (->> inp find-plots (map price) (reduce +)))

;;---- part2


(defn point->plot-and-border
  "Takes p and returns a vector of size two:
     1. a plot to which p belongs and
     2. bordering points"
  [p]
  (let [letter (get inp p)]
    (loop [plot    (sorted-set)
           border  (sorted-set)
           todo    #{p}]
      (if (not (seq todo))
        [plot border]
        (let [x     (first todo)
              more  (disj todo x)
              val   (get inp x)]
          (cond
            ;; outside of map
            (nil? val)
            (recur plot
                   (conj border x)
                   more)

            ;; in the plot
            (= val letter)
            (recur (conj plot x)
                   border
                   (apply conj more (->> x neighboring-squares (remove plot) (remove border))))

            ;; one of neighboring plots
            :else (recur plot (conj border x) more)))))))


(defn plots-and-borders
  "Takes inp and returns coll of plots and corresponding borders"
  [inp]
  (loop [acc  []
         todo   (-> inp keys set)]
    (if-not (seq todo)
      acc
      (let [p     (first todo)
            pb    (point->plot-and-border p)]
        (recur (conj acc pb)
               (set/difference todo (first pb)))))))


(defn corner
  "Returns a block that lies between two neighbors of p.
   Returns nil, when that block is p itself"
  [p pair-of-neighbors]
  (let [ret (mapv - (apply mapv + pair-of-neighbors) p)
        rc (when (not= ret p) ret)]
    rc))

;; Number of sides of a polygon are the same as number of interior angles
(defn count-angles
  "Return the number of interior angles bordering this block."
  [inp p nbs]
  (let [corners  (keep (partial corner p) (combo/combinations nbs 2))
        our-guys (filter #(= (get inp p) (get inp %)) corners)
        rc     (condp = (count nbs)
                 0 4
                 1 2
                 2 (if (= 0 (count corners))
                     0                             ; adjacents are in a line
                     (- 2 (count our-guys)))
                 3 (- 2 (count our-guys))
                 4 (- 4 (count our-guys)))]
    rc))

(defn part2 [inp]
  (let [plots     (->> inp
                       plots-and-borders
                       (map-indexed (fn [plot-id [plot border]] [plot-id {:points plot :borders border}]))
                       (reduce (fn [acc [k v]] (assoc acc k v)) {}))
        answer    (reduce (fn [acc plot-id]
                            (let [plot (get-in plots [plot-id :points])
                                  angles (map (fn [p] (count-angles inp p (plot-neighbors p))) plot)]
                              (assoc-in acc [plot-id :angles] angles)))
                          plots
                          (keys plots))]
    (->> (map (fn [k] (* (count (get-in answer [k :points]))
                         (reduce + (get-in answer [k :angles]))))
              (keys answer))
         (reduce +))))

(defn main []
  (println (part1 inp) (part2 inp)))

