(ns advent.day06
  (:use [advent.utils :only [read-resource]])
  (:require [clojure.string :as str]))

(def data (str/split-lines (read-resource "day06.txt")))

(defn solution [sortfn]
  (->> data
       (apply map vector)
       (map (comp
             first
             last
             (partial sort-by sortfn)
             frequencies))
       (apply str)
       (apply str)))

(defn solution-a []
  (solution val))

(defn solution-b []
  (solution #(- (val %))))
