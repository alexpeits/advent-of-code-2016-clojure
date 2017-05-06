(ns advent.day01
  (:use [advent.utils :only [read-resource]])
  (:require [clojure.string :as str]))

(def data (str/split (str/trim (read-resource "day01.txt"))
                     #", "))

(defn parse-data [d]
  (->> d
       (map (juxt first
                  #(Integer/parseInt (apply str (rest %)))))))

(defn positions [d]
  (->> (map first d)
       (reductions #(({\L dec \R inc} %2) %1) 0)
       rest
       (map #(mod % 4))
       (map [[0 1] [1 0] [0 -1] [-1 0]])
       (mapcat repeat (map second d))
       (reductions (partial map +) (list 0 0))))

(defn point->distance [point]
  (->> point
       (map #(Math/abs %))
       (reduce +)))

(defn solution-a []
  (->> data
       parse-data
       positions
       last
       point->distance))

(defn solution-b []
  (->> data
       parse-data
       positions
       (reductions conj (list))
       (filter (fn [[x & xs]] ((set xs) x)))
       ffirst
       point->distance))
