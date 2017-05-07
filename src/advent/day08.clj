(ns advent.day08
  (:use [advent.utils :only [read-resource]])
  (:require [clojure.string :as str]
            [clojure.set :as s]
            [instaparse.core :as insta]))

;; resource
(def data (str/split-lines (read-resource "day08.txt")))

;; screen
(def on \#)
(def off \.)
(def blank-screen (vec (repeat 6 (vec (repeat 50 off)))))

(defn draw-screen [screen]
  (->> screen
       (map str/join)
       (str/join "\n")
       println))

;; cfg
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

;; grammar->method dispatching
(defn -rotate [arr delta]
  (let [c (count arr)
        r (- c (mod delta c))]
    (vec (concat (drop r arr) (take r arr)))))

(defn -transpose [v]
  (apply mapv vector v))

(defmulti rotate
  (fn [cmd screen] (first cmd)))

(defmethod rotate :row
  [[_ {:keys [y delta]}] screen]
  (update-in screen [y] -rotate delta)
  )

(defmethod rotate :column
  [[_ {:keys [x delta]}] screen]
  (-transpose
   (update-in (-transpose screen)
              [x]
              -rotate
              delta)))

(defmulti draw
  (fn [cmd screen] (first cmd)))

(defmethod draw :rect
  [[_ {:keys [x y]}] screen]
  (->> (for [xs (range x) ys (range y)] [ys xs])
       (reduce #(assoc-in %1 %2 on) screen)))

(defmethod draw :rotate
  [[_ cmd] screen]
  (rotate cmd screen))


;; solution

(defn solution-a []
  (->> data
       (map parse)
       (reduce #(draw %2 %1) blank-screen)
       (apply concat)
       (filter #(= % on))
       (count)))

(defn solution-b []
  (->> data
       (map parse)
       (reduce #(draw %2 %1) blank-screen)
       draw-screen))
