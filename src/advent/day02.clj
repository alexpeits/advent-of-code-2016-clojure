(ns advent.day02
  (:use [advent.utils :only [read-resource]])
  (:require [clojure.string :as str]))

(def data (str/split-lines (read-resource "day02.txt")))

(def dirs {\U [0 -1] \R [1 0] \D [0 1] \L [-1 0]})

(defn in-keypad-a? [[x y]]
  (and (<= -1 x 1) (<= -1 y 1)))

(defn pos->num-a [[x y]]
  (+ 5 (* 3 y) x))

(defn in-keypad-b? [[x y]]
  (and (<= -2 x 2)
       (let [bound (- 2 (Math/abs x))]
         (<= (- bound) y bound))))

(def keypad-b
  (into {}
        (map
         vector
         (->> (for [y (range -2 3)
                    x (range -2 3)]
                [x y])
              (filter in-keypad-b?))
         (map inc (range)))))

(defn pos->num-b [pos]
  (keypad-b pos))

(defn move [check pos dir]
  (let [new-pos (map + pos (dirs dir))]
    (if (check new-pos)
      new-pos
      pos)))

(defn solution-a []
  (apply
   str
   (->> data
        (reductions #(reduce (partial move in-keypad-a?) %1 %2) [0 0])
        rest
        (map pos->num-a)
        )))

(defn solution-b []
  (apply
   str
   (->> data
        (reductions #(reduce (partial move in-keypad-b?) %1 %2) [0 0])
        rest
        (map pos->num-b)
        (map (partial format "%X"))
        )))
