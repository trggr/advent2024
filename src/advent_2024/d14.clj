(ns advent-2024.d14
  (:require [advent-2024.lib :as lib]
            [clojure.string :as str]))

(def width  101)
(def height 103)
(def vhalf  (/ (dec width) 2))
(def hhalf  (/ (dec height) 2))

(def inp (->> (-> "d14"
                  lib/read-file
                  (str/replace #"p=" "")
                  (str/replace #"v=" "")
                  (str/replace #"," " "))
              str/split-lines
              (map (fn [s] (lib/str-split #"\s" s)))
              (map (fn [row] (map parse-long row)))))

(defn fx [seconds, line]
  (let [[x0, _y0, dx, _dy] line]
    (+ x0 (* dx seconds))))

(defn fy [seconds, line]
  (let [[x0, y0, dx, dy] line
        x                (fx seconds line)
        a                (/ dy dx)]
    (+ y0 (* a (- x x0)))))


(defn position [line, seconds]
  [(mod (fx line seconds) width)
   (mod (fy line seconds) height)])

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

