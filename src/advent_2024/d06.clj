(ns advent-2024.d06
  (:require [advent-2024.lib :as lib]
            [clojure.string  :as str]))

(def inp (->> "d06"
              lib/read-file
              str/split-lines
              (map-indexed (fn [i s] [i (zipmap (range) (seq s))]))
              (reduce (fn [acc [i mp]]
                        (reduce (fn [a [j v]]
                                  (assoc a [i j] v))
                                acc
                                mp))
                      (sorted-map))))

(def guard (->> inp
                (filter (fn [[_k v]] (= v \^)))
                ffirst))

;; directions are rows and columns of matrix (not x and y)
(def dirs {:up     [-1 0],
           :right  [0 1],
           :down   [1 0],
           :left   [0 -1]})

(def rotate {:up    :right,
             :right :down,
             :down  :left,
             :left  :up})

(defn next-step
  [guard dir]
  (mapv + guard (get dirs dir)))

(defn walk [inp]
  (loop [guard  guard
         dir    :up
         ahead  (next-step guard dir)
         acc    #{}]
    (cond (not (contains? inp ahead))  (conj acc guard)

          (= (get inp ahead) \#)       (recur guard
                                              (rotate dir)
                                              (next-step guard (rotate dir))
                                              acc)

          :else                        (recur ahead
                                              dir
                                              (next-step ahead dir)
                                              (conj acc guard)))))

(defn loop? [inp]
  (loop [guard    guard
         dir      :up
         ahead    (next-step guard dir)
         visited  (sorted-map)]
    ; (println (keys visited))
    (cond ; (> (count visited) 70)
          ; :over20

          (contains? visited [guard dir])
          :loop

          (not (contains? inp ahead))
          :at-edge

          :else  (if (= (get inp ahead) \#)
                   (recur guard
                          (rotate dir)
                          (next-step guard (rotate dir))
                          (assoc visited [guard dir] 1))
                   (recur ahead
                          dir
                          (next-step ahead dir)
                          (assoc visited [guard dir] 1))))))


(def inp2 (assoc inp guard \.))

;; test
;; (map #(loop? (assoc inp2 % \#)) [[6 3] [7 6] [7 7] [8 1] [8 3] [9 7]])

(->> (for [x (range 130) y (range 130)] [x y])
     (pmap (fn [obstacle] (loop? (assoc inp2 obstacle \#))))
     (filter #(= % :loop))
     count)


(defn main
  []
  (println (count (walk inp))))


