(ns advent-2024.d09
  (:require [advent-2024.lib :as lib]))

(def inp (->> "d09"
              lib/read-file
              (map str)
              (map parse-long)
              (partition-all 2)))
;; ((2 3) (3 3) (1 3) (3 1) (2 1) (4 1) (4 1) (3 1) (4 0) (2))

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

(reduce (fn [acc [f g]] (+ acc f (or g 0))) 0 inp)

;; (apply str (vals block-map))
;; "00...111...2...333.44.5555.6666.777.888899"
;;  2 3  3  3  13  3  12 14   14   13  14  02)
;;  01---+----1----+----2----+----3----+----4----+----5"


;; part 1
(->> (map (fn [from to] [from to])
          (->> blocks (filter (fn [[_k v]] (not= v \.))) (map first) reverse)
          (->> blocks (filter (fn [[_k v]] (= v \.))) (map first)))
     (reduce (fn [acc [from to]]
               (if (>= to from)
                 acc
                 (assoc acc to (get acc from) from \.)))
             blocks)
     (reduce (fn [acc [k v]] (if (= v \.) acc (+ acc (* k v)))) 0))

;; map of free chunks, where each chunk is a vector of [chunk-address chunk-len]
(def chunks  (loop [acc       (sorted-map)
                    addr       0
                    coll       inp]
               (if-not (seq coll)
                 acc
                 (let [[[filen gaplen] & more] coll
                       addr                    (+ addr filen)]
                   (cond (nil? gaplen)   acc
                         (zero? gaplen)  (recur acc addr more)
                         :else           (recur (assoc acc addr gaplen)
                                                (+ addr gaplen)
                                                more))))))

;; files
;;----------------------;;  {0 2, 1 3, 2 1, 3 3, 4 2, 5 4, 6 4, 7 3, 8 4, 9 2}
;; {0 {:addr 0, :len 2},
;;  1 {:addr 5, :len 3},
;;  2 {:addr 11, :len 1},
;;  3 {:addr 15, :len 3},
;;  4 {:addr 19, :len 2},
;;  5 {:addr 22, :len 4},
;;  6 {:addr 27, :len 4},
;;  7 {:addr 32, :len 3},
;;  8 {:addr 36, :len 4},
;;  9 {:addr 40, :len 2}}

chunks
;; {  2 3,      8 3,     12 3,     18 1,     21 1,     26 1,     31 1,     35 1}

;; block-map
;; "00...111...2...333.44.5555.6666.777.888899"
;;  2 3  3  3  13  3  12 14   14   13  14  02)
;;  01---+----1----+----2----+----3----+----4----+----5"

;; inp-pairs ((2 3) (3 3) (1 3) (3 1) (2 1) (4 1) (4 1) (3 1) (4 0) (2))

(def files (->> inp
                (reductions (fn [acc [a b]] (+ acc a (or b 0))) 0)
                (map (fn [file-id [a _b] total]
                       [file-id total a])
                     (range)
                     inp)
                (reduce (fn [acc [file-id addr len]] (assoc acc file-id {:addr addr :len len}))
                        (sorted-map))))

;; {0 {:addr 0,  :len 2},
;;  1 {:addr 5,  :len 3},
;;  2 {:addr 11, :len 1},
;;  3 {:addr 15, :len 3},
;;  4 {:addr 19, :len 2},
;;  5 {:addr 22, :len 4},
;;  6 {:addr 27, :len 4},
;;  7 {:addr 32, :len 3},
;;  8 {:addr 36, :len 4},
;;  9 {:addr 40, :len 2}}

(def output
  (->> files
       keys
       reverse
       (reduce (fn [[blx chnx :as acc] id]
                 (let [file   (get files id)
                       {:keys [addr len]} file
                       chunk  (->> chnx
                                   (filter (fn [[_ clen]] (>= clen len)))
                                   first)]
                   (if-not chunk
                     acc
                     (let [[caddr clen] chunk
                           blx          (-> (arange blx id caddr len)
                                            (arange     \. addr  len))
                           remain       (- clen len)
                           chnx         (dissoc chnx caddr)
                           chnx         (if (zero? remain)
                                          chnx
                                          (assoc chnx (+ caddr len) remain))]
                       [blx chnx]))))
               [blocks chunks])
       first
       (reduce (fn [acc [k v]] (if (= v \.) acc (+ acc (* k v)))) 0)))


;; too high:      8654184283366
;; right answer = 6469636832766
