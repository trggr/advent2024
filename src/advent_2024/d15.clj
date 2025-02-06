(ns advent-2024.d15
  (:require [advent-2024.lib :as lib]
            [clojure.string :as str]))

(def file "d15-test1")

(defn parse [file]
  (let [inp (-> file
                lib/read-file
                (str/split #"\r\n\r\n"))
        moves  (->> inp second (remove #{\return \newline}))
        wh  (->> inp
                 first
                 (lib/str-split #"\r\n"))
        N   (count wh)
        wh3 (->> wh
                 (map-indexed (fn [y s] [(- N y) (zipmap (range) (seq s))]))
                 (reduce (fn [acc [y mp]]
                           (reduce (fn [a [x v]]
                                     (assoc a [x y] v))
                                   acc
                                   mp))
                         (sorted-map)))]
    [wh3, moves, N]))

(def d1    (parse file))
(def wh    (first d1))
(def moves (second d1))
(def N     (nth d1 2))

(count wh)
N
(for [j (range N)]
  (get wh [0 j]))

(range 10 0 -1)

(defn display [wh, N]
  (let [tmp  (for [y (range N 0 -1)]
               (apply str
                      (for [x (range N)]
                        (get wh [x y]))))]
    (doseq [line (map str tmp)]
      (println line))))

(display wh N)




;; (map-indexed (fn [i s] [i (zipmap (range) (seq s))]))
;; )



(-> inp second (remove #{\n}))

(str/split "a b c " #"\s")

;;------- part1

(defn position-after [inp seconds]
  (map (partial position seconds) inp))

(defn part1 [inp]
  (let [lob (position-after inp 100)]
    (->> lob
         (remove (fn [[x y]] (or (= x vhalf) (= y hhalf))))
         (group-by (fn [[x y]]
                     (cond (and (< x vhalf) (< y hhalf)) :nw
                           (and (> x vhalf) (< y hhalf)) :ne
                           (and (< x vhalf) (> y hhalf)) :sw
                           (and (> x vhalf) (> y hhalf)) :se)))
         (map second)
         (map count)
         (reduce *))))

;;------- part2

(def empty-lobby (->> 0
                      (repeat width)
                      vec
                      (repeat height)
                      vec))

(defn lobby-chart [robots]
  (reduce (fn [acc [x y]] (update-in acc [y x] inc))
          empty-lobby
          robots))

(defn symmetrical-coeff [chart]
  (let [diffs (for [y (range height), x (range hhalf)]
                (abs (- (get-in chart [y x])
                        (get-in chart [y (- width x 1)]))))]
    (float (/ (reduce + diffs) (count inp)))))

(defn part2 [inp]
  (let [start       6200  ;; experimentalnal values
        end         6800
        good-enough 0.6]
    (->> (range start end)
         (map (fn [sec]
                [sec (-> inp
                         (position-after sec)
                         lobby-chart
                         symmetrical-coeff)]))
         (filter (fn [[_sec coeff]] (<= coeff good-enough)))
         ffirst)))

(defn main []
  (println (part1 inp) (part2 inp)))

(comment
  (defn print-chart [chart]
    (doseq [row chart]
      (println (apply str (map (fn [c] (if (zero? c) \. c)) row)))))

  ;; to see the picture of a tree:
  (print-chart (lobby-chart (position-after inp 6512))))

