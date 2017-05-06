(ns advent.day08
  (:use [advent.utils :only [read-resource]])
  (:require [clojure.string :as str]
            [clojure.set :as s]
            [instaparse.core :as insta]))

(def data (str/split-lines (read-resource "day08.txt")))

(def grammar (read-resource "day08_grammar.txt"))
(def dsl
  (insta/parser
   grammar
   :auto-whitespace :standard))

(defn parse [expr]
  (insta/transform {:row (fn [x y]
                           [:row
                            {:y (Integer/parseInt (second x))
                             :delta (Integer/parseInt (second (second y)))}])
                    :column (fn [x y]
                              [:column
                               {:x (Integer/parseInt (second x))
                                :delta (Integer/parseInt (second (second y)))}])
                    :axis identity
                    :cmd identity
                    :size (fn [x y]
                            (zipmap
                             [:x :y]
                             (into [] (map #(Integer/parseInt (second %)) [x y]))))}
                   (dsl expr)))

(defmulti rotate
  (fn [cmd screen] (first cmd)))

(defmethod rotate :row
  [[_ args] screen]
  (println "rotate row"))

(defmethod rotate :column
  [[_ args] screen]
  (println "rotate column"))

(defmulti draw
  (fn [cmd screen] (first cmd)))

(defmethod draw :rect
  [[_ coords] screen]
  (println "draw rect"))

(defmethod draw :rotate
  [[_ cmd] screen]
  (rotate cmd screen))
