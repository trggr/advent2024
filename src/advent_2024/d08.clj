(ns advent-2024.d08
  (:require [advent-2024.lib :as lib]
            [clojure.string  :as str]))

(def inp (->> "d08"
              lib/read-file
              str/split-lines
              (map-indexed (fn [i s]
                             [i (into (sorted-map) (zipmap (range) (seq s)))]))
              (into (sorted-map))))

(def N (-> inp (get 0) count))

(def antennas (->>  (for [i (range N),
                          j (range N)
                          :when (not= \. (get-in inp [i j]))]
                      [(get-in inp [i j]) i, j])
                    (reduce (fn [acc [k i j]] (update acc k conj [i j])) {})))

(defn diff [p2 p1] (map - p2 p1))

(defn antinodes
  [p1 p2]
  (let [delta (diff p2 p1)]
    [(map + p2 delta)
     (map - p1 delta)]))

(defn antinodes-part1
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

(defn part1
  []
  (->> antennas
       keys
       (mapcat (fn [a] (antinodes-part1 a)))
       distinct
       count))


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

(defn antinodes2-y-x
  [p1 p2]
  (let [x1   (first p1)
        x2   (first p2)
        dx   (abs (- x1 x2))
        -dx  (* dx -1)
        f    (line-y->x p1 p2)]
    (->> (for [x (range x1 N dx)] x)
         (concat (for [x (range x1 -1 -dx)] x))
         (map (fn [x] [(f x) x]))
         (filter (fn [[row col]] (and (<= 0 row (dec N))
                                      (<= 0 col (dec N))))))))

(defn antinodes2
  [p1 p2]
  (let [x1   (second p1)
        x2   (second p2)
        dx   (abs (- x1 x2))
        -dx  (* dx -1)
        f    (line-x->y p1 p2)]
    (if (zero? dx)
      (antinodes2-y-x p1 p2)
      (->> (for [x (range x1 N dx)] x)
           (concat (for [x (range x1 -1 -dx)] x))
           (map (fn [x] [(f x) x]))
           (filter (fn [[row col]] (and (<= 0 row (dec N))
                                        (<= 0 col (dec N)))))))))

(defn antinodes-part2
  [antenna]
  (let [A (get antennas antenna)]
    (->> (for [p1 A,
               p2 A
               :when (not= p1 p2)]
           (antinodes2 p1 p2))
         (apply concat)
         distinct)))

(defn part2
  []
  (->> antennas
       keys
       (mapcat (fn [a] (antinodes-part2 a)))
       distinct
       count))

(defn main
  []
  (println (part1)
           (part2)))
