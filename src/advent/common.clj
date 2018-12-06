(ns advent.common
  (:require [missing.core :as miss]
            [clojure.string :as strings]
            [clojure.java.io :as io]))


(defmacro defproblem [symbol & body]
  `(defn ~symbol []
     (let [[time# result#] (miss/timing ~@body)]
       (println (format "Problem %s completed in %s" "" (miss/duration-explain time#)))
       (println "Answer:")
       (println result#)
       (println \newline \newline \newline)
       result#)))

(defn first-duplicate [coll]
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

(defn mode [coll]
  (when (not-empty coll)
    (apply max-key val (frequencies coll))))

(defn product [coll]
  (reduce * 1 coll))

(defn sum [coll]
  (reduce + 0 coll))

(defn input-lines [file]
  (->> (slurp (io/file (io/resource file)))
       (strings/split-lines)))

(defn parse-long [s]
  (Long/parseLong s))