(ns advent-2024.d08
  (:require [advent-2024.lib :as lib]
            [clojure.string  :as str]))

(def inp (->> "d08"
              lib/read-file
              str/split-lines
              (map-indexed (fn [i s]
                             [i (into (sorted-map) (zipmap (range) (seq s)))]))
              (into (sorted-map))))

inp
;; ([0  {0 . 1 . 2 . 3 . 4 . 5 . 6 . 7 . 8 . 9 . 10 . 11 .}]
;;  [1  {0 . 1 . 2 . 3 . 4 . 5 . 6 . 7 . 8 0 9 . 10 . 11 .}]
;;  [2  {0 . 1 . 2 . 3 . 4 . 5 0 6 . 7 . 8 . 9 . 10 . 11 .}]
;;  [3  {0 . 1 . 2 . 3 . 4 . 5 . 6 . 7 0 8 . 9 . 10 . 11 .}]
;;  [4  {0 . 1 . 2 . 3 . 4 0 5 . 6 . 7 . 8 . 9 . 10 . 11 .}]
;;  [5  {0 . 1 . 2 . 3 . 4 . 5 . 6 A 7 . 8 . 9 . 10 . 11 .}]
;;  [6  {0 . 1 . 2 . 3 . 4 . 5 . 6 . 7 . 8 . 9 . 10 . 11 .}]
;;  [7  {0 . 1 . 2 . 3 . 4 . 5 . 6 . 7 . 8 . 9 . 10 . 11 .}]
;;  [8  {0 . 1 . 2 . 3 . 4 . 5 . 6 . 7 . 8 A 9 . 10 . 11 .}]
;;  [9  {0 . 1 . 2 . 3 . 4 . 5 . 6 . 7 . 8 . 9 A 10 . 11 .}]
;;  [10 {0 . 1 . 2 . 3 . 4 . 5 . 6 . 7 . 8 . 9 . 10 . 11 .}]
;;  [11 {0 . 1 . 2 . 3 . 4 . 5 . 6 . 7 . 8 . 9 . 10 . 11 .}])

(def N (-> inp (get 0) count))

(def antennas (->>  (for [i (range N), j (range N)
                          :when (not= \. (get-in inp [i j]))]
                      [(get-in inp [i j]) i, j])
                    (reduce (fn [acc [k i j]] (update acc k conj [i j]))
                            {})))

;; {\0 ([1 8] [2 5] [3 7] [4 4]),
;;  \A ([5 6] [9 9] [8 8])}

antennas

(def A (get antennas \T))

(defn diff [p2 p1] (map - p2 p1))

(defn antinodes
  [p1 p2]
  (let [delta (diff p2 p1)]
    [(map + p2 delta)
     (map - p1 delta)]))


(defn antinodes-of
  [antenna]
  (let [A (get antennas antenna)]
    (->> (for [p1 A,
               p2 A
               :when (not= p1 p2)]
           [(antinodes p1 p2) p1 p2])
         (map first)
         (reduce (fn [acc [a1 a2]] (conj acc a1 a2)) [])
         distinct
         (filter (fn [[x y]] (and (<= 0 x (dec N))  (<= 0 y (dec N))))))))

;; part 1
(->> antennas
     keys
     (mapcat (fn [a] (antinodes-of a)))
     distinct
     count)


(defn line-x->y
  "Line going through p1 and p2, returns fn that takes x which gives y"
  [p1 p2]
  (let [[r1 c1] p1
        [r2 c2] p2
        k  (/ (- r2 r1) (- c2 c1))]
    (fn [b]
      (+ (* k (- b c1)) r1))))

(defn line-y->x
  "Line going through p1 and p2, returns fn that takes y which gives x"
  [p1 p2]
  (let [[r1 c1] p1
        [r2 c2] p2
        k  (/ (- c2 c1) (- r2 r1))]
    (fn [b]
      (+ (* k (- b r1)) c1))))

(defn antinodes4-y-x
  [p1 p2]
  (let [x1   (first p1)
        x2   (first p2)
        dx   (abs (- x1 x2))
        -dx  (* dx -1)
        f    (line-y->x p1 p2)]
    (->> (for [x (range x1 N dx)] x)
         (concat (for [x (range x1 -1 -dx)] x))
         (map (fn [x] [(f x) x]))
        ;;  (remove (fn [p] (or (= p p1)
        ;;                      (= p p2))))
         (filter (fn [[row col]] (and (<= 0 row (dec N))
                                      (<= 0 col (dec N))))))))

(defn antinodes4
  [p1 p2]
  (let [x1   (second p1)
        x2   (second p2)
        dx   (abs (- x1 x2))
        -dx  (* dx -1)
        f    (line-x->y p1 p2)]
    (if (zero? dx)
      (antinodes4-y-x p1 p2)
      (->> (for [x (range x1 N dx)] x)
           (concat (for [x (range x1 -1 -dx)] x))
           (map (fn [x] [(f x) x]))
           (filter (fn [[row col]] (and (<= 0 row (dec N))
                                        (<= 0 col (dec N)))))))))


(defn antinodes4-of
  [antenna]
  (let [A (get antennas antenna)]
    (->> (for [p1 A,
               p2 A
               :when (not= p1 p2)]
           (antinodes4 p1 p2))
         (apply concat)
         distinct)))

(->> antennas
     keys
     (mapcat (fn [a] (antinodes4-of a)))
     distinct
     count)
