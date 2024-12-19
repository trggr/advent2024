(ns advent-2024.d02
  (:require [advent-2024.lib :as lib]))

(defn safe-report?
  [coll]
  (let [diffs (->> coll
                   (partition-all 2 1)
                   (reduce (fn [acc [a b :as pair]]
                             (if (< (count pair) 2)
                               acc
                               (conj acc (- a b))))
                           []))
        mx    (reduce max diffs)
        mn    (reduce min diffs)]
    (cond (or (zero? mx) (zero? mn)) false
          (or (< mn -3) (> mx 3))    false
          (< (* mx mn) 0)            false   ;; presence of increases and decreases?
          :else                      true)))

(defn safe-report-part2?
  [coll]
  (or (safe-report? coll)
      (->> coll
           count
           range
           (map (fn [n]
                  (let [[h t] (split-at n coll)]
                    (concat h (rest t)))))
           (some safe-report?))))

(defn part1
  [inp]
  (->> inp
       (map safe-report?)
       (filter true?)
       count))

(defn part2
  [inp]
  (->> inp
       (map safe-report-part2?)
       (filter true?)
       count))

(defn main
  []
  (let [inp (->> "d02"
                 lib/read-file
                 (lib/str-split #"\r\n")
                 (map (fn [s]
                        (map parse-long (lib/str-split #"\s+" s)))))]
    (println (part1 inp)
             (part2 inp))))