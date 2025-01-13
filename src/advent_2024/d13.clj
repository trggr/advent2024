(ns advent-2024.d13
  (:require [advent-2024.lib :as lib]
            [clojure.string :as str]))

(def inp
  (->> (-> "d13"
           lib/read-file
           (str/replace #"Button A: X\+" "")
           (str/replace #"Button B: X+\+" "")
           (str/replace #"Prize: X=" "")
           (str/replace #", Y\+" " ")
           (str/replace #", Y=" " ")
           (str/replace #"\r\n\r\n" "\r\n")
           (str/replace #"\r\n" " ")
           (str/split #"\s"))
       (map parse-long)
       (partition 2)
       (partition 3)))

(defn solve-cramer
  "Takes vectors of size 2: a and b are columns of matrix and
   vector is vector in a right side. Using Cramer's method,
   solves linear equation.
   Returns a vector of two integers.
   If the solution is not in integers, returns nil"
  [a b p]
  (let [[ax ay] a
        [bx by] b
        [px py] p
        d       (- (* ax by) (* ay bx))
        d1      (- (* px by) (* py bx))
        d2      (- (* ax py) (* ay px))
        n       (/ d1 d)
        m       (/ d2 d)]
    (when (and (int? n) (int? m))
      [n m])))

;; part1
(->> inp
     (map (fn [[a b p]] (solve-cramer a b p)))
     (remove nil?)
     (map (fn [[a b]] (+ (* 3 a) (* 1 b))))
     (reduce +))

(defn add_1E13 [[x y]]
  [(+ 10000000000000 x) (+ 10000000000000 y)])

;; part2
(->> inp
     (map (fn [[a b p]] (solve-cramer a b (add_1E13 p))))
     (remove nil?)
     (map (fn [[a b]] (+ (* 3 a) (* 1 b))))
     (reduce +))
