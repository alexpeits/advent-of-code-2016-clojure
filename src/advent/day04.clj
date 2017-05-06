(ns advent.day04
  (:use [advent.utils :only [read-resource]])
  (:require [clojure.string :as str]))


(def data
  (->> (read-resource "day04.txt")
       str/split-lines
       (map #(str/split % #"-"))
       (map #(split-at (dec (count %)) %))
       (map (fn [el]
              (let [a (str/join (first el))
                    b (re-seq #"(\d+)\[([a-z]+)\]" (first (second el)))
                    num (second (first b))
                    res (last (first b))]
                [a (Integer/parseInt num) res])))
       (map #(zipmap [:name :id :checksum] %))))

(defn most-frequent-n [n items]
  (->> items
       frequencies
       (sort-by (fn [[a b]] [(- b) a]))
       (take n)
       (map first)
       (apply str)))

(defn real-room? [rdata]
  (= (most-frequent-n 5 (:name rdata)) (:checksum rdata)))

(defn decrypt-char [steps c]
  (let [-min (int \a)
        -max (int \z)
        d (inc (- -max -min))]
    (-> c
        int
        (- -min)
        (+ steps)
        (mod d)
        (+ -min)
        char)))

(defn decrypt-str [steps s]
  (apply str (map (partial decrypt-char steps) s)))

(defn solution-a []
  (->> data
       (filter real-room?)
       (reduce (fn [acc {id :id}] (+ acc (int id))) 0)))

(defn solution-b []
  (->> data
       (filter real-room?)
       (map (fn [x]
              (update x :name
                      (fn [v] (decrypt-str (:id x) (:name x))))))
       (filter #(str/includes? (:name %) "north"))))
