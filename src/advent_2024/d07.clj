(ns advent-2024.d07
  (:require [advent-2024.lib :as lib]
            [clojure.string  :as str]
            [clojure.edn :as edn]))

(def inp (as-> "d07" $
           (lib/read-file $)
           (str/replace $ #":" "")
           (str/split $ #"\r\n")
           (map (fn [s] (edn/read-string (str "[" s "]"))) $)))

; inp
; ([190 10 19] [3267 81 40 27] [83 17 5] [156 15 6] [7290 6 8 6 15] [161011 16 10 13] [192 17 8 14] [21037 9 7 18 13] [292 11 6 16 20])

(defn eval-expression
  [xs]
  (reduce (fn [acc [op x]]
            (op acc x))
          (first xs)
          (partition 2 (rest xs))))

(defn solvable?
  [[goal & numbers]]
  (let [expressions (reduce (fn [acc n]
                              (mapcat (fn [xs] [(conj xs + n) (conj xs * n)]) acc))
                            [[(first numbers)]]
                            (rest numbers))]
    (if (some (fn [e] (= goal (eval-expression e))) expressions)
      goal
      0)))

(->> inp (map solvable?) (reduce +))

