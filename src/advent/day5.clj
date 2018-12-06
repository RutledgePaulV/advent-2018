(ns advent.day5
  (:require [advent.common :refer :all]
            [clojure.string :as strings]
            [clojure.java.io :as io]
            [missing.core :as miss]))


(defn combine [[s1 s2]]
  (cond
    (nil? s2) [s1]
    (and (not= s1 s2)
         (= (strings/upper-case s1)
            (strings/upper-case s2))) []
    :else [s1 s2]))

(defn parts [coll]
  (->> coll
       (partition 2 2 (repeat nil))
       (miss/keepcat combine)
       (vec)))

(defn polymers [s]
  (reduce conj #{} (strings/lower-case s)))

(defn without [s char]
  (-> s
      (strings/replace (strings/lower-case char) "")
      (strings/replace (strings/upper-case char) "")))

(defn units [s]
  (loop [s s]
    (let [stepped1 (->> s parts (vec))
          stepped2 (vec (cons (first stepped1) (->> (rest stepped1) parts)))]
      (if (not= s stepped2) (recur stepped2) (count stepped2)))))

(defproblem problem9
  (units (vec (slurp (io/input-stream (io/resource "day5.txt"))))))

(defproblem problem10
  (let [text    (slurp (io/input-stream (io/resource "day5.txt")))
        results (into {} (pmap (fn [k] [k (units (without text k))]) (polymers text)))
        least   (apply min-key results (polymers text))]
    (get results least)))