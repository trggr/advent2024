(ns advent-2024.d05
  (:require [advent-2024.lib :as lib]))

(def rules (atom nil))

(defn middle
  "Returns middle element of coll"
  [coll]
  (nth coll (int (/ (count coll) 2))))

(defn reqs-met?
  "Returns true when all requirements in update
   have been met for a page with index idx"
  [upd idx]
  (let [[done todo]      (split-at idx upd)
        [page & todo]    todo
        deps             (get @rules page)
        [done todo]      (map set [done todo])]
    (cond (nil? deps)                                                                         true
          (every? (fn [page] (or (contains? done page) (not (contains? todo page)))) deps)    true
          :else                                                                               false)))

(defn ordered-update?
  [upd]
  (every? (fn [i] (reqs-met? upd i)) (range (count upd))))

(defn part1
  [updates]
  (->> updates
       (filter ordered-update?)
       (map middle)
       (reduce +)))

(defn depends-on?
  "Returns true when page x depends on y"
  [x y]
  (boolean (get-in @rules [x y])))

(defn part2
  [updates]
  (->> updates
       (remove ordered-update?)
       (map (fn [update] (sort (complement depends-on?) update)))
       (map middle)
       (reduce +)))

(defn main
  []
  (let [inp      (->> "d05"
                      lib/read-file
                      (lib/str-split #"\r\n\r\n"))
        updates  (->> inp
                      second
                      (lib/str-split #"\r\n")
                      (map (fn [s] (mapv parse-long (lib/str-split #"," s)))))]
    (reset! rules (->> inp
                       first
                       (lib/str-split #"\r\n")
                       (map (fn [s] (lib/str-split #"\|" s)))
                       (map (fn [[a b]] [(parse-long a) (parse-long b)]))
                       (reduce (fn [acc [before page]]
                                 (update acc
                                         page (fnil conj #{}) before))
                               {})))
    (println (part1 updates)
             (part2 updates))))

