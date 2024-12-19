(ns advent-2024.lib
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn str-split
  "Flip parameters on clojure.string for more convenient use in ->>"
  [x y]
  (str/split y x))

(defn read-file [name]
  (-> (str name ".txt")
      io/resource
      slurp))

(defn read-lines [name]
  (-> (str name ".txt")
      io/resource
      slurp
      (str/split #"\r\n")))

(defn to-number [s]
  (Integer/parseInt s))

(defn to-map
  "Presents input as map, where key is [x y] coordinate of a point,
   and a value is point's value"
  [inp]
  (let [nlines (count inp)
        nrows  (count (first inp))]
    (reduce (fn [acc [k v]]
              (assoc acc k v))
            {}
            (for [i (range nlines)
                  j (range nrows)]
              [[i j] (to-number (str (nth (nth inp i) j)))]))))

(defn median1
  "Median which always points to a real element of a seq"
  [xs]
  (let [n (count xs)]
    (nth (sort xs) (Math/floor (/ n 2)))))

; Works like + , except (nvl+ nil) will give 1. Works better for updates in maps
(def nvl+
  (fnil + 0))

; Works like inc, except (nvlinc nil) will give 1. Works better for updates in maps
(def nvlinc
  (fnil inc 0))

;; returns n-th column of matrix as vector
(defn col [n matrix]
    (map #(nth % n) matrix))

(defn now []
   (-> (new java.text.SimpleDateFormat "MM/dd/yyyy HH:mm:ss")
       (.format (new java.util.Date))))

(defn sum
  "Computes the sum of elements in the sequence"
  [xs]
  (reduce + xs))

;; returns string of length n, if original is shorted, it's padded with spaces,
;; if it's longer it's truncated to n
(defn pad
    ([n s] (pad n s \space))
    ([n s fill]
        (let [x   (str s)
              len (count x)]
            (cond (= len n) x
                  (> len n) (subs x 0 n)
                  :else     (apply str x (take (- n len) (repeat fill)))))))

(defn de-uglify [xs]
    (let [ys      (vec xs)
          ncols   (count (first ys))
          maxlens (for [i (range ncols)]
                      (->> ys (col i) (map str) (map count) (reduce max)))]
        (str/join \newline
            (for [row ys]
                 (str/join "  " (map pad maxlens row))))))

(defn de-uglify2 [xs]  (str/join \newline
                          (for [row (vec xs)]
                                 (str/join \tab row))))

(defn de-uglify3 [xs]
    (let [ys      xs
          ncols   (count (first ys))
          maxlens (for [i (range ncols)]
                      (->> ys (col i) (map str) (map count) (reduce max)))]
        (str/join \newline
            (for [row ys]
                 (str/join "  " (map pad maxlens row))))))

(defn in? [xs x]      (contains? (set xs) x))