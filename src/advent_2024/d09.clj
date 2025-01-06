(ns advent-2024.d09
  (:require [advent-2024.lib :as lib]))

(def inp (->> "d09"
              lib/read-file
              (map str)
              (map parse-long)
              (partition-all 2)))

(defn arange
  "Assign v to m for block of keys between from and from + len"
  [m v from len]
  (reduce (fn [acc k] (assoc acc k v)) m (range from (+ from len))))

(def blocks (loop [inp     inp
                   id      0
                   ptr     0
                   acc    (sorted-map)]
              (if-not (seq inp)
                acc
                (let [[[file gap] & more]  inp
                      end                  (+ ptr file)
                      tmp                  (arange acc id ptr file)
                      gap                  (or gap 0)]
                  (recur more
                         (inc id)
                         (+ ptr file gap)
                         (if (zero? gap)
                           tmp
                           (arange tmp \. end gap)))))))

(defn checksum
  [coll]
  (reduce (fn [acc [k v]]
            (if (= v \.) acc (+ acc (* k v))))
          0
          coll))

(defn part1
  [blocks]
  (->> (map (fn [from to] [from to])
            (->> blocks (filter (fn [[_k v]] (not= v \.))) (map first) reverse)
            (->> blocks (filter (fn [[_k v]] (= v \.))) (map first)))
       (reduce (fn [acc [from to]]
                 (if (>= to from)
                   acc
                   (assoc acc to (get acc from) from \.)))
               blocks)
       checksum))


;; map of gaps, where each gap is a vector of [address length]
(def gaps (->> inp
               (reduce (fn [[mp addr :as acc] [file gap]]
                         (let [addr (+ addr file)]
                           (if-not gap
                             acc
                             (if (zero? gap)
                               [mp addr]
                               [(assoc mp addr gap) (+ addr gap)]))))
                       [(sorted-map) 0])
               first))


(def files (->> inp
                (reductions (fn [acc [a b]] (+ acc a (or b 0))) 0)
                (map (fn [file-id [a _b] total]
                       [file-id total a])
                     (range)
                     inp)
                (reduce (fn [acc [file-id addr len]] (assoc acc file-id {:addr addr :len len}))
                        (sorted-map))))

(defn part2
  [blocks files gaps]
  (->> files
       keys
       reverse
       (reduce (fn [[blx gapps :as acc] id]
                 (let [file               (get files id)
                       {:keys [addr len]} file
                       gap                (->> gapps
                                               (filter (fn [[_ free]] (>= free len)))
                                               first)]
                   (if (not gap)
                     acc
                     (let [[to free] gap]
                       (if (>= to addr)
                         acc
                         (let [blx      (-> blx
                                            (arange id to len)
                                            (arange \. addr len))
                               remain   (- free len)
                               gapps     (cond-> gapps
                                           true          (dissoc to)
                                           (pos? remain) (assoc (+ to len) remain))]
                           [blx gapps]))))))
               [blocks gaps])
       first
       checksum))


(defn main
  []
  (println (part1 blocks)
           (part2 blocks files gaps)))
