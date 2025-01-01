(ns advent-2024.d07
  (:require [advent-2024.lib :as lib]
            [clojure.string  :as str]
            [clojure.edn :as edn]))

(defn eval2
  [xs]
  (reduce (fn [acc [op x]]
            (cond (= op \|) (parse-long (str acc x))
                  (= op *)  (* acc x)
                  (= op +)  (+ acc x)
                  :else (throw (ArithmeticException. (str "Unknown operator: " op)))))
          (first xs)
          (partition 2 (rest xs))))

(defn solvable?
  [ops [goal & numbers]]
  (let [expressions (reduce (fn [acc n] (concat (for [x acc, op ops]
                                                  (conj x op n))))
                            [[(first numbers)]]
                            (rest numbers))]
    (when (some (fn [e] (= goal (eval2 e))) expressions)
      goal)))

;; (defn solve
;;   [inp ops]
;;   (->> inp
;;        (filter (partial solvable? ops))
;;        (map first)
;;        (reduce +)))

(defn solve
  [inp ops]
  (let [xf (comp
            (filter (partial solvable? ops))
            (map first))]
    (transduce xf + inp)))


(defn main
  []
  (let [inp (as-> "d07" $
              (lib/read-file $)
              (str/replace $ #":" "")
              (str/split $ #"\r\n")
              (map (fn [s] (edn/read-string (str "[" s "]"))) $))]
    (println (solve inp [+ *])
             (solve inp [+ * \|]))))

(time (main))
; 5837374519342 492383931650959
; "Elapsed time: 90056.529 msecs"

(time (main))
; via transdusers
; 5837374519342 492383931650959
; "Elapsed time: 90830.7351 msecs"


