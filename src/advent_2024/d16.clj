(ns advent-2024.d16
   (:require [advent-2024.lib :as lib]
             [clojure.string :as str]
             [clojure.set :as set]
             [advent-2024.graph :as graph]))

 (def file "d16")

 (defn parse [file]
   (-> file
       lib/read-file
       lib/string->matrix))

(def graph  (parse file))

(def initial {:position  (->> graph (filter (fn [[_k v]] (= v \S))) ffirst)
              :direction :e
              :finish    (->> graph (filter (fn [[_k v]] (= v \E))) ffirst)
              :graph     graph})

 (def dirs {:n   [-1  0],
            :e   [0  1],
            :s   [1  0],
            :w   [0 -1]})

(defn generate-moves
  "Returns list of [new-state new-cost] which
   can be achieved from a given state and cost"
  [[state cost]]
  (->> dirs
       (map (fn [[dir adj]]
              [dir (lib/add-vector (state :position) adj)]))
       (filter (fn [[_dir adj]] (contains? #{\. \E} (get-in state [:graph adj]))))
       (map (fn [[dir adj]]
              [(assoc state :position adj :direction dir)
               (+ cost (if (= dir (state :direction)) 1 1001))]))))

(graph/dijkstra-search-imm initial
                           (fn [state] (= (state :position) (state :finish)))
                           generate-moves)

