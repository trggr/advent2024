(defproject advent-2024 "0.1.0-SNAPSHOT"
  :description "Advent of Code 2024"
  :dependencies [[org.clojure/clojure            "1.12.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [nrepl                          "1.1.1"]
                 [zprint                         "1.2.9"]]
  :plugins       [[cider/cider-nrepl             "0.47.1"]]
  :main          advent-2024.core
  :target-path   "target/%s"

  ;; :repl-options {:init-ns advent-2024.d01}
  )

;lein update-in :dependencies conj [nrepl,"1.1.1"]
; -- update-in :plugins conj [cider/cider-nrepl,"0.47.1"] 
;-- update-in [:repl-options,:nrepl-middleware] conj '["cider.nrepl/cider-middleware"]' -- repl :headless
