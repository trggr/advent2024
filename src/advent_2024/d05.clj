(ns advent-2024.d05
  (:require [advent-2024.lib :as lib]
            [clojure.math :as math]))

(defn rearrange
  "Returns true when all conditions in a coll
   have been met for a page with index idx"
  [coll rules idx]
  (let [[a b]                     (split-at idx coll)
        [page & b]                b
        deps                      (get rules page)
        processed                 (set a)
        unprocessed               (set b)]
    (cond (nil? deps)
          true
          (every? (fn [d] (or (contains? processed d)
                              (not (contains? unprocessed d)))) deps)
          true
          :else false)))

(defn part1
  [puzzle rules]
  (->> puzzle
       (map (fn [ucoll]
              (let [len (count ucoll)]
                (if (every? (fn [n] (rearrange ucoll rules n)) (range len))
                  (nth ucoll (int (math/floor (/ len 2))))
                  0))))
       (reduce +)))

;; (defn rearrange
;;   "Rearranges a bad coll so all conditions are met"
;;   [coll rules idx]
;;   (let [[a b]                     (split-at idx coll)
;;         [page & b]                b
;;         deps                      (get rules page)
;;         processed                 (set a)
;;         unprocessed               (set b)]
;; (reduce (fn [acc x]
;; (if (contains?
;;     (cond (every? (fn [d] (or (contains? processed d)
;;                               (not (contains? unprocessed d)))) deps)
;;           true
;;           :else false)))


(defn main
  []
  (let [inp       (->> "d05"
                       lib/read-file
                       (lib/str-split #"\r\n\r\n"))
        rules     (->> inp
                       first
                       (lib/str-split #"\r\n")
                       (map (fn [s] (lib/str-split #"\|" s)))
                       (map (fn [[a b]] [(parse-long a) (parse-long b)]))
                       (reduce (fn [acc [before page]]
                                 (assoc acc
                                        page
                                        (if-let [was (get acc page)]
                                          (conj was before)
                                          [before])))
                               {}))
        puzzle    (->> inp
                       second
                       (lib/str-split #"\r\n")
                       (map (fn [s] (mapv parse-long (lib/str-split #"," s)))))]
    (println (part1 puzzle rules))))

;; (main)

(comment

  (def inp       (->> "d05"
                      lib/read-file
                      (lib/str-split #"\r\n\r\n")))

  (def rules (->> inp
                  first
                  (lib/str-split #"\r\n")
                  (map (fn [s] (lib/str-split #"\|" s)))
                  (map (fn [[a b]] [(parse-long a) (parse-long b)]))
                  (reduce (fn [acc [before page]]
                            (assoc acc
                                   page
                                   (if-let [was (get acc page)]
                                     (conj was before)
                                     [before])))
                          {})))
; {53 [47 75 61 97],
;  13 [97 61 29 47 75 53],
;  61 [97 47 75],
;  47 [97 75],
;  29 [75 97 53 61 47],
; 75 [97]}

  (def puzzle
    (->> inp
         second
         (lib/str-split #"\r\n")
         (map (fn [s] (mapv parse-long (lib/str-split #"," s))))))
;   ([75 47 61 53 29] [97 61 53 29 13] [75 29 13] [75 97 47 61 53] [61 13 29] [97 13 75 29 47])


  (defn rearrange
    "Returns true when all conditions when in a pageset all rules have been met for the page with index idx"
    [pagecoll rules idx]
    ;; (println pagecoll)
    ;; (println rules idx)
    (let [[done tmp] (split-at idx pagecoll)
          done (set done)
          [page & more] tmp
          deps (get rules page)
          more (set more)]
      ;; (println done page more)
      (cond (do ;; (println "in nil!, deps=" deps)
              (nil? deps)) true
            (do ;; (println "every!")
              (every? (fn [dep]
                          ;; (println "page=" page "dep=" dep)
                        (or (contains? done dep)
                            (not (contains? more dep))))
                      deps)) true
            :else false)))

  (def pagecoll (first puzzle))
;; [75 47 61 53 29]

  (def pagecoll (second puzzle))

  (every? (fn [n]
            (rearrange pagecoll rules n))
          (range (count pagecoll)))

  (->> puzzle
       (map (fn [ucoll]
              (let [len (count ucoll)]
                (if (every? (fn [n] (rearrange ucoll rules n)) (range len))
                  (nth ucoll (int (math/floor (/ len 2))))
                  0))))
       (reduce +))

;; (with-open [out (io/writer "output2.txt")]
;;   (doseq [[dir xmas] part1_out]
;;     (.write out (format "%s,%s\n" xmas dir))))
;;
  )
