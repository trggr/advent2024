(ns advent-2024.d01
  (:require [advent-2024.lib :as lib]))

(defn part1
  [a b]
  (->> (sort b)
       (map (fn [x y] (abs (- x y))) (sort a))
       (reduce +)))

(defn part2
  [a b]
  (let [freq (frequencies b)]
    (->> a
         (map (fn [x] (* x (get freq x 0))))
         (reduce +))))

(defn main
  []
  (let [inp (->> "d01"
                 lib/read-file
                 (lib/str-split #"\r\n")
                 (map (fn [s] (lib/str-split #"\s+" s)))
                 (map (fn [[x y]] [(parse-long x) (parse-long y)])))
        a   (map first inp)
        b   (map second inp)]
    (println (part1 a b)
             (part2 a b))))