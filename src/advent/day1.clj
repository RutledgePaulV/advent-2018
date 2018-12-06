(ns advent.day1
  (:require [advent.common :refer :all]))


(defproblem problem1
  (->>
    (input-lines "day1.txt")
    (map parse-long)
    (sum)))


(defproblem problem2
  (->> (input-lines "day1.txt")
       (map parse-long)
       (cycle)
       (reductions + 0)
       (first-duplicate)))
