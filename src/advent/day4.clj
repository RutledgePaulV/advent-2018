(ns advent.day4
  (:require [advent.common :refer :all]
            [missing.core :as miss]))

(defn parse [s]
  (merge (->> (rest (re-find #"^\[(\d+)-(\d+)-(\d+)\s+(\d+):(\d+)\]" s))
              (map #(Long/parseLong %))
              (zipmap [:year :month :day :hour :minute]))
         (->> (rest (re-find #"#(\d+)" s))
              (map #(Long/parseLong %))
              (zipmap [:guard]))
         (->> (rest (re-find #"(asleep|wakes|begins)" s))
              (map keyword)
              (zipmap [:action]))))

(defn begin? [log]
  (= (:action log) :begins))

(defn sleep? [log]
  (= (:action log) :asleep))

(defn awake? [log]
  (= (:action log) :wakes))

(defn duration [[asleep awake]]
  (- (:minute awake) (:minute asleep)))

(defn minutes [[asleep awake]]
  (range (:minute asleep) (:minute awake)))

(defn naps [coll]
  (->> (filter (some-fn sleep? awake?) coll) (partition 2)))

(defn sleepiest-minute [shifts]
  (->> (mapcat naps shifts) (mapcat minutes) (mode)))

(defn shift-sleep [shift]
  (->> (naps shift) (map duration) (sum)))

(defn shifts-sleep [shifts]
  (->> shifts (map shift-sleep) (sum)))

(defn sleepiest-guard [data]
  (->> data
       (miss/map-vals shifts-sleep)
       (sort-by second #(compare %2 %1))
       (ffirst)))

(defn shifts-by-guard []
  (->> (input-lines "day4.txt")
       (map parse)
       (sort-by (juxt :year :month :day :hour :minute))
       (partition-with begin?)
       (group-by (comp :guard first))))

(defproblem problem7
  (let [shifts            (shifts-by-guard)
        sleepyhead        (sleepiest-guard shifts)
        sleepyhead-shifts (get shifts sleepyhead)]
    (* sleepyhead (first (sleepiest-minute sleepyhead-shifts)))))

(defproblem problem8
  (let [[guard [minute frequency]]
        (->> (shifts-by-guard)
             (miss/map-vals sleepiest-minute)
             (miss/filter-vals some?)
             (sort-by #(second (val %)) #(compare %2 %1))
             (first))]
    (println guard minute)
    (* guard minute)))