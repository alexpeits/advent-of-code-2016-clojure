(ns advent.day09
  (:use [advent.utils :only [read-resource]])
  (:require [clojure.string :as str]))

(def data (str/trim (read-resource "day09.txt")))

(def nums-re #"(\d+)x(\d+)")

(defn has-marker? [s]
  (re-find nums-re s))

(defn mapstr [sequence]
  (map #(apply str %) sequence))

(defn mapint [sequence]
  (map #(Integer/parseInt %) sequence))

(defn decompress [s]
  (loop [acc "", s s]
    (if (empty? s)
      acc
      (let [[a b] (mapstr (split-at (str/index-of s \() s))
            [n r] (mapint (rest (re-find nums-re (apply str (take 20 b)))))
            [_ tmp] (mapstr (split-at (inc (str/index-of b \))) b))
            [rep rm] (mapstr (split-at n tmp))]
        (recur
         (str acc a (apply str (repeat r rep)))
         rm)))))

(defn decompress-count [s]
  (loop [acc 0, s s]
    (if (or (empty? s)
            (not (has-marker? s)))
      (+ acc (count s))
      (let [[a b] (mapstr (split-at (str/index-of s \() s))
            [n r] (mapint (rest (re-find nums-re (apply str (take 20 b)))))
            [_ tmp] (mapstr (split-at (inc (str/index-of b \))) b))
            [rep rm] (mapstr (split-at n tmp))]
        (recur
         (+ acc (count a) (* r (decompress-count rep)))
         rm)))))


(defn solution-a []
  (->> data decompress count))

(defn solution-b []
  (decompress-count data))
