(ns advent.problems
  (:require [missing.core :as miss]
            [clojure.java.io :as io]
            [clojure.string :as strings]
            [clojure.set :as sets])
  (:import (java.util UUID)))

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

(defn partition-with
  ([f coll]
   (let [ret (volatile! 0)]
     (partition-by
       (fn [item]
         (if (f item)
           (vswap! ret inc)
           @ret)) coll))))

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


(defproblem problem6
  (letfn [(parse [s]
            (zipmap
              [:id :left :top :width :height]
              (map #(Integer/parseInt %) (rest (re-find #"#(\d+)\s+@\s+(\d+),(\d+):\s+(\d+)x(\d+)" s)))))
          (coords [{:keys [left top width height]}]
            (for [x (range left (+ width left))
                  y (range top (+ top height))]
              [x y]))]
    (let [claims (->> (slurp (io/file (io/resource "p3.txt")))
                      (strings/split-lines)
                      (map parse))
          table  (->> claims
                      (mapcat (fn [claim] (map vector (repeat claim) (coords claim))))
                      (reduce (fn [agg [{:keys [id]} coord]] (update agg coord (fnil conj #{}) id)) {}))]
      (letfn [(exclusive? [claim] (every? (fn [coord] (= 1 (count (get table coord)))) (coords claim)))]
        (miss/find-first exclusive? claims)))))


(defproblem problem7
  (letfn [(parse [s]
            (merge (->> (rest (re-find #"^\[(\d+)-(\d+)-(\d+)\s+(\d+):(\d+)\]" s))
                        (map #(Long/parseLong %))
                        (zipmap [:year :month :day :hour :minute]))
                   (->> (rest (re-find #"#(\d+)" s))
                        (map #(Long/parseLong %))
                        (zipmap [:guard]))
                   (->> (rest (re-find #"(asleep|wakes|begins)" s))
                        (map keyword)
                        (zipmap [:action]))))]
    (let [shifts
          (->> (slurp (io/file (io/resource "p4.txt")))
               (strings/split-lines)
               (map parse)
               (sort-by (juxt :year :month :day :hour :minute))
               (partition-with #(= (:action %) :begins))
               (map #(let [guard (:guard (first %))]
                       (map (fn [x] (assoc x :guard guard)) %))))]
      )))