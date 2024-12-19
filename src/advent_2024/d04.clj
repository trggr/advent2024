(ns advent-2024.d04
  (:require [advent-2024.lib :as lib]))


(defn coll->indexed-map
  "Takes coll and returns a map in which keys are the
   numbered positions of elements in coll"
  [s]
  (->> s
       (map (fn [x y]
              [x y])
            (range))
       (into {})))


(def directions {:east  [0  1]
                 :west  [0 -1]
                 :south [1  0]
                 :north [-1  0]
                 :se    [1  1]
                 :nw    [-1 -1]
                 :sw    [1  -1]
                 :ne    [-1  1]})

(def patterns (->> directions
                   (map (fn [[dir [v h]]]
                          [dir
                           (for [step [1 2 3]]
                             [(* v step) (* h step)])]))))


(defn count-mas-around-x
  [patterns puzzle posx]
  (reduce (fn [acc [dir mas]]
            (let [posmas (map (fn [x] (map + x posx)) mas)]
              (if (= [\M \A \S]
                     (map (fn [e] (get-in puzzle e)) posmas))
                (conj acc [dir (into [posx] posmas)])
                acc)))
          []
          patterns))


(defn part1
  [puzzle positions]
  (reduce (fn [acc letter]
            (if-not (= \X (get-in puzzle letter))
              acc
              (+ acc (count (count-mas-around-x patterns puzzle letter)))))
          0
          positions))

(defn m-and-s-pairs?
  [[m1 m2] [s1 s2]]
  (and (= m1 m2 \M) (= s1 s2 \S)))


(defn x-mas? [puzzle directions pos]
  (let [f   (fn [dir] (->> dir (get directions) (map + pos) (get-in puzzle)))
        ne  (f :ne)
        nw  (f :nw)
        se  (f :se)
        sw  (f :sw)]
    (or (m-and-s-pairs? [nw ne] [sw se])
        (m-and-s-pairs? [ne se] [nw sw])
        (m-and-s-pairs? [se sw] [nw ne])
        (m-and-s-pairs? [sw nw] [ne se]))))


(defn part2
  [puzzle positions]
  (reduce (fn [acc letter]
            (if (and (= \A (get-in puzzle letter))
                     (x-mas? puzzle directions letter))
              (inc acc)
              acc))
          0
          positions))


(defn main
  []
  (let [inp       (->> "d04"
                       lib/read-file
                       (lib/str-split #"\r\n"))
        width     (-> inp first count)
        puzzle    (->> inp
                       (map coll->indexed-map)
                       coll->indexed-map)
        positions (for [i (range width) j (range width)] [i j])]
    (println (part1 puzzle positions)
             (part2 puzzle positions))))



(comment


;; (with-open [out (io/writer "output2.txt")]
;;   (doseq [[dir xmas] part1_out]
;;     (.write out (format "%s,%s\n" xmas dir))))
;;
  )