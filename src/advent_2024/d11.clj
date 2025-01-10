(ns advent-2024.d11
  (:require [advent-2024.lib :as lib]))

(def inp (->> "d11"
              lib/read-file
              (lib/str-split #"\s")
              (map parse-long)))
;; (0 1 10 99 999)

(def blink
  (memoize (fn [stone]
             (if (zero? stone)
               1
               (let [s  (str stone)
                     n  (count s)
                     h  (/ n 2)]
                 (if (even? n)
                   [(parse-long (subs s 0 h)) (parse-long (subs s h))]
                   (* 2024 stone)))))))

;; My method is using a map in which stones are the keys
;; and their counts are vals. As levels progress,
;; I maintain this map
(defn blink-count
  "Takes level and stone, returns number of stones
   produced by this stone at a given level"
  [level stone]
  (let [f (fnil + 0)]
    (loop [acc {stone 1}
           i   0]
      (if (= i level)
        (reduce + (vals acc))
        (recur (->> acc
                    keys
                    (map blink)
                    (map (fn [cnt k] [k cnt]) (vals acc))
                    (reduce (fn [mp [k cnt]]
                              (if (number? k)
                                (update mp k f cnt)
                                (let [[a b] k]
                                  (-> mp
                                      (update a f cnt)
                                      (update b f cnt)))))
                            {}))
               (inc i))))))

(defn main
  []
  (println (->> inp (map (partial blink-count 25)) (reduce +))
           (->> inp (map (partial blink-count 75)) (reduce +))))




