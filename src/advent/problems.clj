(ns advent.problems
  (:require [missing.core :as miss]
            [clojure.java.io :as io]
            [clojure.string :as strings]
            [clojure.set :as sets]))

(defmacro defproblem [symbol & body]
  `(defn ~symbol []
     (let [[time# result#] (miss/timing ~@body)]
       (println (format "Problem %s completed in %s" "" (miss/duration-explain time#)))
       (println "Answer:")
       (println result#)
       (println \newline \newline \newline)
       result#)))

(defn first-dupe [coll]
  (let [result (reduce
                 (fn [agg x]
                   (if (agg x)
                     (reduced x)
                     (conj agg x)))
                 #{}
                 coll)]
    (if (set? result) nil result)))

(defproblem problem1
  (->> (slurp (io/file (io/resource "p1.txt")))
       (strings/split-lines)
       (map #(Long/parseLong %))
       (reduce + 0)))


(defproblem problem2
  (->> (slurp (io/file (io/resource "p1.txt")))
       (strings/split-lines)
       (map #(Long/parseLong %))
       (cycle)
       (reductions + 0)
       (first-dupe)))

(defproblem problem3
  (letfn [(bins [s]
            (sets/intersection
              (set (vals (frequencies s)))
              #{2 3}))]
    (->> (slurp (io/file (io/resource "p2.txt")))
         (strings/split-lines)
         (mapcat bins)
         (frequencies)
         (vals)
         (reduce * 1))))

(defproblem problem4
  (letfn [(delete [idx s]
            (str (subs s 0 idx) (subs s (inc idx))))]
    (let [input (->> (slurp (io/file (io/resource "p2.txt")))
                     (strings/split-lines))]
      (some (fn [idx]
              (->> input
                   (map (partial delete idx))
                   first-dupe))
            (range)))))

(defproblem problem5
  (letfn [(parse [s]
            (zipmap
              [:id :left :top :width :height]
              (map #(Integer/parseInt %) (rest (re-find #"#(\d+)\s+@\s+(\d+),(\d+):\s+(\d+)x(\d+)" s)))))
          (coords [{:keys [left top width height]}]
            (for [x (range left (+ width left))
                  y (range top (+ top height))]
              [x y]))]
    (->> (slurp (io/file (io/resource "p3.txt")))
         (strings/split-lines)
         (map parse)
         (mapcat coords)
         (frequencies)
         (miss/filter-vals #(> % 1))
         (keys)
         (count))))