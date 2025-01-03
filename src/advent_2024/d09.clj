(ns advent-2024.d09
  (:require [advent-2024.lib :as lib]
            [clojure.string  :as str]))

(def inp (->> "d09"
              lib/read-file))
; "2333133121414131402"

(def inp-nums  (->> inp
                    (map str)
                    (map parse-long)))
; (2 3 3 3 1 3 3 1 2 1 4 1 4 1 3 1 4 0 2)

(def inp-pairs (->> inp-nums
                    (partition-all 2)))
; ((2 3) (3 3) (1 3) (3 1) (2 1) (4 1) (4 1) (3 1) (4 0) (2))

;; (def files (->> inp-pairs
;;                 (map first)
;;                 (map-indexed (fn [id len] [id len]))
;;                 (reduce (fn [acc [id len]] (assoc acc id len))
;;                         (sorted-map))))
;; ; {0 2, 1 3, 2 1, 3 3, 4 2, 5 4, 6 4, 7 3, 8 4, 9 2}

(defn arange
  "Assign val to map for a range of keys"
  [mp val from to]
  (reduce (fn [acc k] (assoc acc k val)) mp (range from to)))

(defn get-blocks
  [inp-pairs]
  (loop [s             inp-pairs
         next-file-id  0
         next-block-id 0
         blocks        (sorted-map)]
    (if-not (seq s)
      blocks
      (let [[[flen slen] & more] s
            fend                 (+ next-block-id flen)
            blocks               (arange blocks next-file-id next-block-id fend)
            slen                 (or slen 0)
            next-file-id         (inc next-file-id)]
        (if (zero? slen)
          (recur more next-file-id fend blocks)
          (recur more
                 next-file-id
                 (+ fend slen)
                 (arange blocks \. fend (+ fend slen))))))))

(def blocks (get-blocks inp-pairs))

;; (apply str (vals blocks))
;; "00...111...2...333.44.5555.6666.777.888899"
;;  2 3  3  3  13  3  12 14   14   13  14  02)
;;  01---+----1----+----2----+----3----+----4----+----5"
blocks
files


;; packing
(->> (map (fn [from to] [from to])
          (->> blocks (filter (fn [[_k v]] (not= v \.))) (map first) reverse)
          (->> blocks (filter (fn [[_k v]] (= v \.))) (map first)))
     (reduce (fn [acc [from to]]
               (if (>= to from)
                 acc
                 (assoc acc to (get acc from) from \.)))
             blocks)
     (remove (fn [[_k v]] (= v \.)))
     (reduce (fn [acc [k v]] (+ acc (* k v)))
             0))

