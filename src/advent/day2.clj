(ns advent.day2
  (:require [advent.common :refer :all]
            [clojure.set :as sets]))


(defproblem problem3
  (letfn [(bins [s]
            (sets/intersection
              (set (vals (frequencies s)))
              #{2 3}))]
    (->> (input-lines "day2.txt")
         (mapcat bins)
         (frequencies)
         (vals)
         (product))))

(defproblem problem4
  (letfn [(delete [idx s] (str (subs s 0 idx) (subs s (inc idx))))]
    (let [input (input-lines "day2.txt")]
      (some (fn [idx] (->> input (map (partial delete idx)) first-duplicate)) (range)))))
