(ns unpack
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn str-split
  "Flip parameters on clojure.string for more convenient use in ->>"
  [x y]
  (str/split y x))

(defn extract-file [str]
  (let [[filename & src]   (str/split str #"\r\n")
        filename           (str/trim filename)
        src                (str/join "\n" src)]
    (when-not (empty? filename)
      (io/make-parents filename)
      (spit filename src)
      filename)))

(->> "source.txt"
     slurp
     (str-split #"-- STARTFILE ")
     (mapv extract-file)
     (str/join "\n")
     println)


