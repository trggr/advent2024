(ns advent-2024.d12
  (:require [advent-2024.lib :as lib]
            [clojure.string  :as str]
            [clojure.set     :as set]
            [clojure.math.combinatorics :as combo]))

(def inp (->> "d12"
              lib/read-file
              str/split-lines
              (map-indexed (fn [i s] [i (zipmap (range) (seq s))]))
              (reduce (fn [acc [i mp]]
                        (reduce (fn [a [j v]]
                                  (assoc a [i j] v))
                                acc
                                mp))
                      (sorted-map))))

(defn pp [[a b]]
  (str a b))

(defn ppcoll [coll]
  (str/join " " (map pp coll)))

(def N 5)

(def hs (for [i (range (inc N)), j (range N)]  [[i j] [i (inc j)]]))
(def vs (for [j (range (inc N)), i (range N)]  [[i j] [(inc i)]]))


(def ths (zipmap (concat hs vs) (repeat true)))

;; (count ths)
;; (println ths)
;; (for [i range (inc N), j (range (inc N)]
;; (if (

(def dirs {:n   [-1  0],
           :ne  [-1  1],
           :e   [0  1],
           :se  [1  1],
           :s   [1  0],
           :sw  [1 -1],
           :w   [0 -1]
           :nw  [-1 -1]})

(defn add
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn neighboring-squares
  "Takes p, returns neighboring points"
  [p]
  (->> (select-keys dirs [:n :e :s :w])
       vals
       (map (partial add p))))


(defn plot-neighbors
  "Takes a point returns neighboring points with the same value"
  [p]
  (let [v  (get inp p)]
    (->> p
         neighboring-squares
         (filter (fn [x] (and (contains? inp x)
                              (= v (get inp x))))))))

;; if a point from plot A has a new, and unknown before, neighbor
;; which belongs to plot B, then A and B are part of the same plot

(defn determine-plot
  "Takes point p and returns a collection of points comprising a plot"
  [p]
  (loop [plot    (sorted-map p 0)
         todo    [p]
         step    1]
    (if-not (seq todo)
      plot
      (let [[p & more]  todo
            nbs         (->> p
                             plot-neighbors
                             (remove (fn [x] (contains? plot x))))]
        (recur (reduce (fn [acc x] (assoc acc x step)) plot nbs)
               (concat more nbs)
               (inc step))))))

(defn find-plots
  [inp]
  (loop [plots   []
         unalloc (-> inp keys set)]
    (if-not (seq unalloc)
      plots
      (let [p       (first unalloc)
            plot    (determine-plot p)]
        (recur (conj plots plot)
               (apply disj unalloc (keys plot)))))))

(defn area
  [plot]
  (count plot))

(defn perimeter
  [plot]
  (->> plot
       keys
       (map plot-neighbors)
       (map count)
       (map (partial - 4))
       (reduce +)))

(defn price
  [plot]
  (* (area plot)
     (perimeter plot)))

(defn main
  []
  (let [plots (find-plots inp)]
    (println (->> plots (map price) (reduce +)))))

;; (main)


;;---- part2


(defn point->plot-and-border
  "Takes p and returns a vector of size two:
     1. a plot to which p belongs and
     2. bordering points"
  [p]
  (let [letter (get inp p)]
    (loop [plot    (sorted-set)
           border  (sorted-set)
           todo    #{p}]
      (if (not (seq todo))
        [plot border]
        (let [x     (first todo)
              more  (disj todo x)
              val   (get inp x)]
          (cond
            ;; outside of map
            (nil? val)
            (recur plot
                   (conj border x)
                   more)

            ;; in the plot
            (= val letter)
            (recur (conj plot x)
                   border
                   (apply conj more (->> x neighboring-squares (remove plot) (remove border))))

            ;; one of neighboring plots
            :else (recur plot (conj border x) more)))))))


;; AAAA
;; BBCD
;; BBCC
;; EEEC

(defn plots-and-borders
  "Takes inp and returns coll of plots and corresponding borders"
  [inp]
  (loop [acc  []
         todo   (-> inp keys set)]
    (if-not (seq todo)
      acc
      (let [p     (first todo)
            pb    (point->plot-and-border p)]
        (recur (conj acc pb)
               (set/difference todo (first pb)))))))


;; (plots-and-borders inp)
;; [[#{[0 0] [0 1] [0 2] [0 3] [0 4] [1 0] [2 0] [2 1] [2 2] [2 3] [2 4] [3 0] [4 0] [4 1] [4 2] [4 3] [4 4]}
;;   #{[-1 0] [-1 1] [-1 2] [-1 3] [-1 4]
;;     [0 -1] [0 5] [1 -1] [1 1] [1 2] [1 3] [1 4] [2 -1] [2 5] [3 -1] [3 1] [3 2] [3 3] [3 4] [4 -1] [4 5] [5 0] [5 1] [5 2] [5 3] [5 4]}] [#{[3 1] [3 2] [3 3] [3 4]} #{[2 1] [2 2] [2 3] [2 4] [3 0] [3 5] [4 1] [4 2] [4 3] [4 4]}] [#{[1 1] [1 2] [1 3] [1 4]} #{[0 1] [0 2] [0 3] [0 4] [1 0] [1 5] [2 1] [2 2] [2 3] [2 4]}]]

;; plots
;; {0 {:points #{[0 0] [0 1] [0 2] [0 3] [0 4] [1 0] [2 0] [2 1] [2 2] [2 3] [2 4] [3 0] [4 0] [4 1] [4 2] [4 3] [4 4]},
;;     :borders #{[-1 0] [-1 1] [-1 2] [-1 3] [-1 4] [0 -1] [0 5] [1 -1] [1 1] [1 2] [1 3] [1 4] [2 -1]
;;                [2 5] [3 -1] [3 1] [3 2] [3 3] [3 4] [4 -1] [4 5] [5 0] [5 1] [5 2] [5 3] [5 4]}},
;;  1 {:points #{[3 1] [3 2] [3 3] [3 4]},
;;     :borders #{[2 1] [2 2] [2 3] [2 4] [3 0] [3 5] [4 1] [4 2] [4 3] [4 4]}},
;;  2 {:points #{[1 1] [1 2] [1 3] [1 4]},
;;     :borders #{[0 1] [0 2] [0 3] [0 4] [1 0] [1 5] [2 1] [2 2] [2 3] [2 4]}}}


(def plots     (->> inp
                    plots-and-borders
                    (map-indexed (fn [plot-id [plot border]] [plot-id {:points plot :borders border}]))
                    (reduce (fn [acc [k v]] (assoc acc k v)) {})))

(def plot-map  (->> (for [i      (keys plots),
                          p      (get-in plots [i :points])]
                      [p i])
                    (into (sorted-map))))

;; plot-map
;; {[0 0] 0, [0 1] 0, [0 2] 0, [0 3] 0, [0 4] 0,
;;  [1 0] 0, [1 1] 2, [1 2] 2, [1 3] 2, [1 4] 2,
;;  [2 0] 0, [2 1] 0, [2 2] 0, [2 3] 0, [2 4] 0,
;;  [3 0] 0, [3 1] 1, [3 2] 1, [3 3] 1, [3 4] 1,
;;  [4 0] 0, [4 1] 0, [4 2] 0, [4 3] 0, [4 4] 0}

;;---------------------------------------------------
;; establish :rels
(def plots2 (->> plots (reduce-kv (fn [acc k v]
                                    (assoc-in acc [k :rels] (->> (get v :borders)
                                                                 (map plot-map)
                                                                 (remove nil?)
                                                                 set)))
                                  plots)))

;; plots2
;; {0 {:points #{[0 0] [0 1] [0 2] [0 3] [0 4]
;;               [1 0]
;;               [2 0] [2 1] [2 2] [2 3] [2 4]
;;               [3 0]
;;               [4 0] [4 1] [4 2] [4 3] [4 4]},
;;     :borders #{[-1 0] [-1 1] [-1 2] [-1 3] [-1 4]
;;                [0 -1] [0 5]
;;                [1 -1] [1 1] [1 2] [1 3] [1 4]
;;                [2 -1] [2 5]
;;                [3 -1] [3 1] [3 2] [3 3] [3 4]
;;                [4 -1] [4 5]
;;                [5 0] [5 1] [5 2] [5 3] [5 4]}, :rels #{1 2}},
;;  1 {:points #{[3 1] [3 2] [3 3] [3 4]}, :borders #{[2 1] [2 2] [2 3] [2 4] [3 0] [3 5] [4 1] [4 2] [4 3] [4 4]}, :rels #{0}},
;;  2 {:points #{[1 1] [1 2] [1 3] [1 4]}, :borders #{[0 1] [0 2] [0 3] [0 4] [1 0] [1 5] [2 1] [2 2] [2 3] [2 4]}, :rels #{0}}}


;; ;;---------------------------------------------------
;; ;; set :is-island? indicator
;; (def plots3 (->> plots2
;;                  (reduce-kv (fn [acc k v]
;;                               (assoc-in acc [k :is-island?] (and (= 1 (count (get v :rels)))
;;                                                                  (every? inp (get v :borders)))))
;;                             plots2)))

;; ; plots3
;; (def islands (->> plots3 (filter (fn [[_k v]] (get v :is-island?)))))
;; islands

(def test2 [[-1 0] [-1 1] [-1 2] [-1 3] [-1 4] [0 -1] [0 5] [1 -1]
               ; [1 1]  ;; remove
               ; [1 3]  ;; remove
            [1 5] [2 -1] [2 5] [3 -1]
               ; [3 1]  ;; remove
               ; [3 3]  ;; remove
            [3 5] [4 -1] [4 5]
            [5 0] [5 1] [5 2] [5 3] [5 4]])

;; According to border's geometry, if we are going East, then
;; we can only turn NE, continue E, or turn SE. There
;; are no other options.
;; Our method is to touch the border with a right hand at all times,
;; then eventually we'll return to the same point where we started
(def next-point  {:e  [:ne :e :se],  ;; added :w to solve "turnaround"
                  :s  [:se :s :sw],  ;; added :n to solve "turnaround"
                  :w  [:sw :w :nw],  ;; added :e
                  :n  [:nw :n :ne]}) ;; added :s

;; This is to correct the direction. Yes, we found the next point on
;; border in the NE direction, but that just means that from the next
;; point we'll be going N (since there's no NE)
(def correct-dir {:ne :n,
                  :e :e,
                  :se :s,
                  :s :s,
                  :sw :w,
                  :w :w,
                  :nw :n,
                  :n :n})

(def opposite-dir {:n :s, :e :w, :s :n, :w :e})

;; (defn line-up-clockwise
;;   "Takes border and returns sequence of bordering points
;;    aligned in a circle, also returns the sequence of directions
;;    in which they are aligned"
;;   [border]
;;   (println "line-up-clockwise: " border)
;;   (let [border     (set border)
;;         topline    (->> border (map first) (reduce min))
;;         leftmost   (->> border (filter (fn [[i _j]] (= i topline))) (map second) (reduce min))]
;;     (loop [p       [topline leftmost]
;;            dir     :e
;;            ps      []
;;            ds      []
;;            right-of-return false
;;            safety 25]
;;       (println "line-up-clockwise: p=" p ", dir=" dir ", ps=" ps ", ds=" ds)
;;       (if (or (zero? safety)
;;               (= p (first ps)))
;;         [ps ds]
;;         (let [nd (->> dir
;;                       (get next-point)
;;                       (some (fn [d]
;;                               (let [d1 (get dirs d)
;;                                     p1 (add p d1)]
;;                                 (println "p=" p "d=" d "d1=" d1 "p1=" p1)
;;                                 (cond (not (contains? border p1))
;;                                       nil
;;                                       right-of-return
;;                                       d
;;                                       (contains? (set ps) p1)
;;                                       nil
;;                                       :else
;;                                       d)))))
;;               _ (println "nd=" nd)]
;;           (if-not (nil? nd)
;;             (recur (add p (get dirs nd))
;;                    (get correct-dir nd)
;;                    (conj ps p)
;;                    (conj ds dir)
;;                    (and right-of-return (= dir nd))
;;                    (dec safety))
;;             (do (println "I new that. p=" p)
;;                 (recur (add p (get dirs (opposite-dir dir)))
;;                        (opposite-dir dir)
;;                        (conj ps p)
;;                        (conj ds dir)
;;                        true
;;                        (dec safety)))))))))

(defn line-up-clockwise
  "Takes border and returns sequence of bordering points
   aligned in a circle, also returns the sequence of directions
   in which they are aligned"
  [border]
  (println "line-up-clockwise: " border)
  (let [border     (set border)
        topline    (->> border (map first) (reduce min))
        leftmost   (->> border (filter (fn [[i _j]] (= i topline))) (map second) (reduce min))]
    (loop [p       [topline leftmost]
           dir     :e
           ps      []
           safety  25]
      (println "line-up-clockwise: p=" (pp p) ", dir=" dir ", ps=" ps)
      (if (or (zero? safety)
              (= p (first ps)))
        [(map first ps) []]
        (let [nd (->> dir
                      (get next-point)
                      (some (fn [d]
                              (let [d1 (get dirs d)
                                    p1 (add p d1)]
                                (println "p=" (pp p) "d=" d "d1=" d1 "p1=" p1)
                                (when (and (contains? border p1)
                                           (not (contains? ps [p1 d1])))
                                  d)))))
              _ (println "nd=" nd)]
          (if-not (nil? nd)
            (recur (add p (get dirs nd))
                   (get correct-dir nd)
                   (conj ps [p dir])
                   (dec safety))
            (do (println "I new that. p=" p)
                (recur (add p (get dirs (opposite-dir dir)))
                       (opposite-dir dir)
                       (conj ps [p dir])
                       (dec safety)))))))))

;; 1. When plot is an island, remove its *plot* from *border* of host;
;; then:
;;   2.1 The perimeter of an island is equal to the length of its border
;;   2.2 The perimeter of non-island is equal to its border plus sum of
;;      perimeters of its islands

;; ;;---------------------------------------------------
;; ;; remove plots of islands from hosts
;; (def plots4 (reduce-kv (fn [acc k v]
;;                          (if-not (get v :is-island?)
;;                            acc
;;                            (let [island-plot (get v :points)
;;                                  host        (first (get-in acc [k :rels]))
;;                                  host-border (get-in acc [host :borders])]
;;                              (assoc-in acc [host :borders] (set/difference host-border island-plot)))))
;;                        plots3
;;                        plots3))


(defn touching-sides
  "Takes lined up border and returns the coll of colls of touch points (:u, :d, :l :r)
   For example, the return value [:u] - means a border touches plot on its upper side
                                 [:u :r] - on upper and right side, etc."
  [linedup-border plot]
  (reduce (fn [acc b]
            (assoc acc b
                   (cond-> #{}
                     (contains? plot (add b (get dirs :n))) (conj :u)
                     (contains? plot (add b (get dirs :e))) (conj :r)
                     (contains? plot (add b (get dirs :s))) (conj :d)
                     (contains? plot (add b (get dirs :w))) (conj :l))))
          {}
          linedup-border))

(def side-compatibility-map {:d {:d (fn [[i1 _j1] [i2 _j2]] (= i1 i2))                   ;; :d can connect to :d, provided they are on the same row
                                 :l (fn [[i1 j1] [i2 j2]]   (and (< i1 i2) (< j1 j2)))
                                 :r (fn [[i1 j1] [i2 j2]]   (and (< i1 i2) (> j1 j2)))}
                             :r {:r (fn [[_i1 j1] [_i2 j2]] (= j1 j2))
                                 :d (fn [[i1 j1] [i2 j2]]   (and (> i1 i2) (< j1 j2)))
                                 :u (fn [[i1 j1] [i2 j2]]   (and (< i1 i2) (< j1 j2)))}
                             :u {:u (fn [[i1 _j1] [i2 _j2]] (= i1 i2))
                                 :r (fn [[i1 j1] [i2 j2]]   (and (> i1 i2) (> j1 j2)))
                                 :l (fn [[i1 j1] [i2 j2]]   (and (> i1 i2) (< j1 j2)))}
                             :l {:l (fn [[_i1 j1] [_i2 j2]] (= j1 j2))
                                 :d (fn [[i1 j1] [i2 j2]]   (and (> i1 i2) (> j1 j2)))
                                 :u (fn [[i1 j1] [i2 j2]]   (and (< i1 i2) (> j1 j2)))}})

;; Takes b1 and one chosen side and connects it to
;;  b2 to one of its sides"
(defn connect-two-border-points [b1 side b2 sides2]
  (println "connect-two-border-points:" b1 side b2 sides2)
  (let [rc (cond (= 0 (count sides2)) (throw (Exception. (str b2 " has no sides")))
                 (= 3 (count sides2)) (throw (Exception. (str b2 " is threefer!")))
                 (= 1 (count sides2)) (first sides2)
                 :else (->> side side-compatibility-map (filter (fn [[_k compat-fn]] (compat-fn b1 b2))) ffirst))
        _ (println "rc=" rc)]
    rc))

(defn connect-sides [linedup-border sides]
  (let [almost (map (fn [b priorb]
                      (let [xs (sides b)]
                        (if (= 1 (count xs))
                          (first xs)
                          [priorb b xs])))
                    linedup-border
                    (vec (cons nil linedup-border)))]
    (->> almost
         (reduce (fn [[acc prior] x]
                   (if-not (vector? x)
                     [(conj acc x) x]
                     (let [[b1 b2 b2sides] x
                           this (connect-two-border-points b1 prior b2 b2sides)
                           other (-> b2sides (disj this) first)]
                       [(-> acc (conj this) (conj other)) other])))
                 [[] nil])
         first)))

(defn perimeter-of-sides
  "Takes plotdb and plot-id, returns a perimeter of sides"
  [plotdb plot-id]
  (println "perimeter-of-sides: " plotdb plot-id)
  (let [plot      (get-in plotdb [plot-id :points])
        _         (println "plot=" plot)
        border    (get-in plotdb [plot-id :borders])
        _         (println "border=" border)
        lborder   (first (line-up-clockwise border))
        _         (println "lborder=" lborder)
        sides     (touching-sides lborder plot)
        _         (println "sides=" sides)]
    (connect-sides lborder sides)))

(defn compress
  "Takes [:e :e :e :e :e :s :s :s :s :s :w :w :w :w :w :n :n :n :n :n]
   returns [[5 e] [5 s] [5 w] [5 n]]"
  [coll]
  (loop [acc []
         prior (first coll)
         cnt 1
         coll (rest coll)]
    (if-not (seq coll)
      (conj acc [prior cnt])
      (let [[x & more] coll]
        (if (= prior x)
          (recur acc prior (inc cnt) more)
          (recur (conj acc [prior cnt]) x 1 more))))))

;; (def plots5 (reduce (fn [acc [k v]]
;;                       (println k v (map (fn [[_k v]] (get v :perim)) acc))
;;                       (assoc-in acc [k :outer-perim]
;;                                 (count (compress (perimeter-of-sides acc k)))))
;;                     plots4
;;                     plots4))

(defn corner
  "Returns a block that lies between two neighbors of p"
  [p pair-of-neighbors]
  (let [ret (mapv - (apply mapv + pair-of-neighbors) p)
        rc (when (not= ret p) ret)]
;;    (println "corner: p=" p "->" rc)
    rc))

;; Number of sides of a polygon are the same as number of interior angles
(defn count-angles
  "Return the number of interior angles bordering this block."
  [inp p nbs]
  (let [corners  (keep (partial corner p) (combo/combinations nbs 2))
        our-guys (filter #(= (get inp p) (get inp %)) corners)
        rc     (condp = (count nbs)
                 0 4
                 1 2
                 2 (if (= 0 (count corners))
                     0                             ; adjacents are in a line
                     (- 2 (count our-guys)))
                 3 (- 2 (count our-guys))
                 4 (- 4 (count our-guys)))]
;;    (println "count-angles: l=" p "nbs=" nbs "corners=" corners "fcorners=" our-guys "rc=" rc)
;;    (println "count-angles: p=" p " -> " rc)
    rc))


;; (defn part2 [plots]
;;   (let [ks         (->> plots keys sort)
;;         ;; _          (println "ks=" ks)
;;         bulk-perim (->> ks
;;                         (map (fn [k]
;;                                (let [self (get-in plots [k :outer-perim])
;;                                      islands
;;                                      (->> (get-in plots [k :rels])
;;                                           (filter (fn [plot] (get-in plots [plot :is-island?])))
;;                                           (map (fn [island] (get-in plots [island :outer-perim])))
;;                                           (reduce +))]
;;                                  (+ self islands)))))
;;         ;; _          (println "bulk-perim=" bulk-perim)
;;         areas      (->> ks
;;                         (map (fn [k] (get-in plots [k :points])))
;;                         (map count))]
;;     (reduce + (map * bulk-perim areas))))

; (part2 plots5)

;; {0 {:points #{[0 0] [0 1] [0 2] [0 3] [0 4] [1 0] [2 0] [2 1] [2 2] [2 3] [2 4] [3 0] [4 0] [4 1] [4 2] [4 3] [4 4]},
;;     :borders #{[-1 0] [-1 1] [-1 2] [-1 3] [-1 4] [0 -1] [0 5] [1 -1]
;;                [1 1] [1 2] [1 3] [1 4] [2 -1] [2 5] [3 -1] [3 1] [3 2] [3 3] [3 4] [4 -1] [4 5] [5 0] [5 1] [5 2] [5 3] [5 4]},
;;     :rels #{1 2}},
;;  1 {:points #{[3 1] [3 2] [3 3] [3 4]},
;;     :borders #{[2 1] [2 2] [2 3] [2 4] [3 0] [3 5] [4 1] [4 2] [4 3] [4 4]},
;;     :rels #{0}},
;;  2 {:points #{[1 1] [1 2] [1 3] [1 4]}, :borders #{[0 1] [0 2] [0 3] [0 4] [1 0] [1 5] [2 1] [2 2] [2 3] [2 4]},
;;     :rels #{0}}}


(defn corner
  "Returns a block that lies between two neighbors of p"
  [p pair-of-neighbors]
  (let [ret (mapv - (apply mapv + pair-of-neighbors) p)
        rc (when (not= ret p) ret)]
    ;; (println "corner: p=" p "pair=" pair-of-neighbors "rc=" rc)
 ;;   (println "corner: p=" p "->" rc)
    rc))

;; Number of sides of a polygon are the same as number of interior angles
(defn count-angles
  "Return the number of interior angles bordering this block."
  [inp p nbs]
  (let [corners  (keep (partial corner p) (combo/combinations nbs 2))
        our-guys (filter #(= (get inp p) (get inp %)) corners)
        rc     (condp = (count nbs)
                 0 4
                 1 2
                 2 (if (= 0 (count corners))
                     0                             ; adjacents are in a line
                     (- 2 (count our-guys)))
                 3 (- 2 (count our-guys))
                 4 (- 4 (count our-guys)))]
;;    (println "count-angles: l=" p "nbs=" nbs "corners=" corners "fcorners=" our-guys "rc=" rc)
;;    (println "count-angles: p=" p " -> " rc)
    rc))

;; Good
;; (->>
;;  (get-in plots2 [0 :points])
;;  (map (fn [p] (count-angles inp p (plot-neighbors p)))))

(def answer
  (reduce (fn [acc plot-id]
            (let [plot (get-in plots2 [plot-id :points])
                  angles (map (fn [p] (count-angles inp p (plot-neighbors p))) plot)]
              (assoc-in acc [plot-id :angles] angles)))
          plots2
          (keys plots2)))

(->> (map (fn [k] (* (count (get-in answer [k :points]))
                     (reduce + (get-in answer [k :angles]))))
          (keys answer))
     (reduce +))





