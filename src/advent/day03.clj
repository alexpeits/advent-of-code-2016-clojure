(ns advent.day03
  (:use [advent.utils :only [read-resource]])
  (:require [clojure.string :as str]))

(def data
  (->> (read-resource "day03.txt")
       str/split-lines
       (map (fn [line]
              (map #(Integer/parseInt %)
                   (re-seq #"\d+" line))))
       (map #(into [] %))
       (into [])))

(defn triangle? [[a b c]]
  (and (> (+ a b) c)
       (> (+ b c) a)
       (> (+ c a) b)))

(defn solution-a []
  (->> data
       (filter triangle?)
       count))

(defn solution-b []
  (->> data
       (partition 3)
       (mapcat #(apply map vector %))
       (filter triangle?)
       (count)))
