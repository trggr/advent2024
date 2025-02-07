(ns advent-2024.d15
  (:require [advent-2024.lib :as lib]
            [clojure.string :as str]))

(def file "d15")
(def m2c {\^ [0 1], \v [0 -1], \< [-1 0], \> [1 0]})

(defn parse [file]
  (let [inp (-> file
                lib/read-file
                (str/split #"\r\n\r\n"))
        moves  (->> inp second (remove #{\return \newline}))
        wh     (->> inp first lib/string->matrix)]
    [wh, moves]))

(def puzzle    (parse file))
(def plane     (-> puzzle first lib/matrix->plane))
(def moves     (second puzzle))
(def robot     (->> plane
                    (filter (fn [[_k v]] (= v \@)))
                    ffirst))

(defn tx [plane pos newpos]
  (assoc plane
         pos \.
         newpos (plane pos)))

(defn lookup-stop [plane pos dir]
  (let [x (->> pos
               (iterate (partial lib/add-vector dir))
               (partition-by (fn [k] (get plane k)))
               (take 2))
        [a [stop & _]] x]
    [a stop]))

(defn move-robot [plane robot move]
  (let [dir    (m2c move)
        newpos (lib/add-vector robot dir)
        obj    (plane newpos)]
    (cond (= \# obj)
          [robot plane]
          (= \. obj)
          [newpos (tx plane robot newpos)]
          (= \O obj)
          (let [[boxes stop] (lookup-stop plane newpos dir)
                stopper (plane stop)
                items (conj boxes robot)]
            (cond (= \# stopper) [robot plane]
                  (= \. stopper) [newpos (reduce (fn [acc k] (tx acc k (lib/add-vector k dir)))
                                                 plane
                                                 (reverse items))]
                  :else [robot plane])))))

(defn part1 []
  (let [rc  (loop [moves moves
                   plane plane
                   robot robot]
              (if-not (seq moves)
                plane
                (let [[move & more]       moves
                      [newrobot newplane] (move-robot plane robot move)]
                  (recur more newplane newrobot))))
        N    (->> rc keys (map first) (reduce max))]
    (->> rc
         (filter (fn [[_k v]] (= v \O)))
         (map first)
         (map (fn [[x y]] (+ x (* 100 (- N y)))))
         (reduce +))))


(part1)

(comment
  (lib/print-plane plane)

;; ########
;; #..O.O.#
;; ##@.O..#
;; #...O..#
;; #.#.O..#
;; #...O..#
;; #......#
;; ########
  )
