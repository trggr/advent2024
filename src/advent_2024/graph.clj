;; from https://cuddly-octo-palm-tree.com/posts/2021-12-26-graph-search/
(ns advent-2024.graph
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn search
  [initial final? generate-moves]
  (let [INF Long/MAX_VALUE
        put-in (fn [m [k v]] (update m k (fnil min INF) v))]
    (loop [[state cost :as current] [initial 0]
           to-visit {}
           visited {}]
      (let [to-visit (if (<= (visited state INF) cost)
                       to-visit
                       (->> (generate-moves current)
                            (remove (fn [[nxt-state nxt-cost]]
                                      (>= nxt-cost (visited nxt-state INF))))
                            (reduce put-in
                                    to-visit)))
            visited (put-in visited current)]
        (if (empty? to-visit)
          (->> visited
               (filter (fn [[state cost]] (final? state)))
               (map (fn [[state cost]] cost))
               (reduce min))
          (recur (first to-visit)
                 (into {} (rest to-visit))
                 visited))))))

(defn depth-first-search
  [initial final? generate-moves]
  (let [INF Long/MAX_VALUE
        put-in (fn [m [k v]] (update m k (fnil min INF) v))]
    (loop [[state cost :as current] [initial 0]
           to-visit () ;; changed to a list
           visited {}]
      (let [to-visit (if (<= (visited state INF) cost)
                       to-visit
                       (->> (generate-moves current)
                            (remove (fn [[nxt-state nxt-cost]]
                                      (>= nxt-cost (visited nxt-state INF))))
                            (reduce conj ;; changed `put-in` to `conj`
                                    to-visit)))
            visited (put-in visited current)]
        (if (empty? to-visit)
          (->> visited
               (filter (fn [[state cost]] (final? state)))
               (map (fn [[state cost]] cost))
               (reduce min))
          (recur (first to-visit)
                 (rest to-visit) ;; no need to rebuild a map anymore
                 visited))))))

(defn breadth-first-search
  [initial final? generate-moves]
  (let [INF Long/MAX_VALUE
        put-in (fn [m [k v]] (update m k (fnil min INF) v))]
    (loop [[state cost :as current] [initial 0]
           to-visit clojure.lang.PersistentQueue/EMPTY ;; changed
           visited {}]
      (let [to-visit (if (<= (visited state INF) cost)
                       to-visit
                       (->> (generate-moves current)
                            (remove (fn [[nxt-state nxt-cost]]
                                      (>= nxt-cost (visited nxt-state INF))))
                            (reduce conj ;; changed `put-in` to `conj`
                                    to-visit)))
            visited (put-in visited current)]
        (if (empty? to-visit)
          (->> visited
               (filter (fn [[state cost]] (final? state)))
               (map (fn [[state cost]] cost))
               (reduce min))
          (recur (peek to-visit) ;; gets the "least recently added" item
                 (pop to-visit) ;; returns queue without (peek q)
                 visited))))))

(defn dijkstra-search
  [initial final? generate-moves]
  (let [to-visit (java.util.PriorityQueue. 100 compare)]
    (loop [[cost state] [0 initial]
           visited #{}]
      (when (not (visited state))
        (doseq [[nxt-state nxt-cost] (generate-moves [state cost])]
          (when (not (visited nxt-state))
            (.add to-visit [nxt-cost nxt-state]))))
      (if (final? state)
        cost
        (recur (.poll to-visit)
               (conj visited state))))))

(defn dijkstra-search-imm
  [initial final? generate-moves]
  (loop [[cost states] [0 [initial]]
         to-visit (sorted-map 0 [initial])
         visited #{}]
    (let [to-visit (->> states
                        (remove visited)
                        (mapcat (fn [state] (generate-moves [state cost])))
                        (reduce (fn [m [s c]]
                                  (if (visited s)
                                    m
                                    (update m c (fnil conj #{}) s)))
                                (dissoc to-visit cost)))]
      (if (not (every? (complement final?) states))
        cost
        (recur (first to-visit)
               to-visit
               (reduce conj visited states))))))

(defn a-star-search
  [initial final? generate-moves heuristic]
  (let [to-visit (java.util.PriorityQueue. 100 compare)]
    (loop [[guess cost state] [(heuristic initial) 0 initial]
           visited #{}]
      (when (not (visited state))
        (doseq [[nxt-state nxt-cost] (generate-moves [state cost])]
          (when (not (visited nxt-state))
            (.add to-visit [(+ nxt-cost (heuristic nxt-state))
                            nxt-cost nxt-state]))))
      (if (final? state)
        cost
        (recur (.poll to-visit)
               (conj visited state))))))
