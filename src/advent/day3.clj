(ns advent.day3
  (:require [advent.common :refer :all]
            [missing.core :as miss]))

(def input (input-lines "day3.txt"))

(defn parse [s]
  (let [regex #"#(\d+)\s+@\s+(\d+),(\d+):\s+(\d+)x(\d+)"]
    (zipmap
      [:id :left :top :width :height]
      (map #(Integer/parseInt %) (rest (re-find regex s))))))

(defn coords [{:keys [left top width height]}]
  (for [x (range left (+ width left))
        y (range top (+ top height))]
    [x y]))

(defproblem problem5
  (->> input
       (map parse)
       (mapcat coords)
       (frequencies)
       (miss/filter-vals #(> % 1))
       (keys)
       (count)))


(defproblem problem6
  (let [claims (map parse input)
        table  (->> claims
                    (mapcat (fn [claim] (map vector (repeat claim) (coords claim))))
                    (reduce (fn [agg [{:keys [id]} coord]] (update agg coord (fnil conj #{}) id)) {}))]
    (letfn [(exclusive? [claim] (every? (fn [coord] (= 1 (count (get table coord)))) (coords claim)))]
      (miss/find-first exclusive? claims))))


