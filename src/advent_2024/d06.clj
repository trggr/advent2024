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
    (cond (contains? visited [guard dir])
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

;; (def walked (walk inp2))

;; (count walked)

;; ;; test
;; (map #(loop? (assoc inp2 % \#)) [[6 3] [7 6] [7 7] [8 1] [8 3] [9 7]])

;; (->> walked
;;      (pmap (fn [obstacle] (loop? (assoc inp2 obstacle \#))))
;;      (filter #(= % :loop))
;;      count)

(defn main
  []
  (let [walked (walk inp)
        p2 (->> walked
                (pmap (fn [obstacle] (loop? (assoc inp obstacle \#))))
                (filter #(= % :loop))
                count)]
    (println (count walked) p2)))

;; Part 2 algorithm:
;; This algorithm worked on the test data. But on the real one it gives:
;; "too low" result. Some improvement can be to use it, subtract it from
;; "visited" from part 1 and then use brute force on a difference.
;;
;; Part2 algorithm: if "ahead" was already visited
;; going in a direction that is "right" to a current, then an obstacle
;; can be placed directly beyond "ahead"
;; -or-
;; if at the "guard" point I turn right and, looking as far as I can,
;; I see already visited point with "right"
;; direction, then I can add an "ahead" point as an obstacle

;; (defn see-visited?
;;   [visited inp guard dir]
;;   (reduce (fn [acc _p]
;;             (let [p (mapv + acc (get dirs dir))]
;;               (cond (not (contains? inp p))     (reduced nil)
;;                     (= (get inp p) \#)          (reduced nil)
;;                     (contains? visited [p dir]) (reduced p)
;;                     :else p)))
;;           guard
;;           (range 130)))

  ;; (defn walk2
  ;;   [inp]
  ;;   (loop [guard     guard
  ;;          dir       :up
  ;;          ahead     (next-step guard dir)
  ;;          visited   (sorted-map)
  ;;          obstacles  #{}]
  ;;     (cond (not (contains? inp ahead))                   obstacles
  ;;           (= (get inp ahead) \#)                        (recur guard
  ;;                                                                (rotate dir)
  ;;                                                                (next-step guard (rotate dir))
  ;;                                                                visited
  ;;                                                                obstacles)
  ;;           (contains? visited [ahead (rotate dir)])      (recur ahead
  ;;                                                                dir
  ;;                                                                (next-step ahead dir)
  ;;                                                                (assoc visited [guard dir] 1)
  ;;                                                                (conj obstacles (next-step ahead dir)))
  ;;           ;; (see-visited? visited inp guard (rotate dir)) (recur ahead
  ;;           ;;                                                      dir
  ;;           ;;                                                      (next-step ahead dir)
  ;;           ;;                                                      (assoc visited [guard dir] 1)
  ;;           ;;                                                      (conj obstacles ahead))
  ;;           :else                                         (recur ahead
  ;;                                                                dir
  ;;                                                                (next-step ahead dir)
  ;;                                                                (assoc visited [guard dir] 1)
  ;;                                                                obstacles))))
