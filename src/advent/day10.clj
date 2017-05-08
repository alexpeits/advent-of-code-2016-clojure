(ns advent.day10
  (:use [advent.utils :only [read-resource]])
  (:require [clojure.string :as str]
            [instaparse.core :as insta]))

;; resource
(def data (str/split-lines (read-resource "day10.txt")))

(defn bot-kw [bot]
  (keyword (str "bot" bot)))

(defn output-kw [output]
  (keyword (str "out" output)))

(defn make-kw [kw n]
  (if (= :bot kw)
    (bot-kw n)
    (output-kw n)))

;; cfg
(def grammar (read-resource "day10_grammar.txt"))
(def dsl
  (insta/parser
   grammar
   :auto-whitespace :standard))

(defn parse [expr]
  (insta/transform {:cmd identity
                    :num #(Integer/parseInt %)
                    :give (fn [from & values]
                            [:give (into {:bot (second from)} values)])
                    :take (fn [value to]
                            [:take {:value value :bot (second to)}])}
                   (dsl expr)))

;; NOTE:
;; there are 2 commands:
;; take (e.g.) "value 3 goes to bot 2"
;; give (e.g.) "bot 1 gives low to bot 2 and high to output 3"

(defn give [bot state]
  (if (can-give? bot state)
    (let [kw (bot-kw bot)
          chips (first (state kw))
          task (first (second (state kw)))
          [lt ht] (map make-kw task)
          [-min -max] (sort chips)]
      (-> state
          (update-in [kw 0] #([]))
          (update-in [kw 1] #(rest %))
          ;; TODO
          (update-in [lt 0] #(into [] (conj % -min)))
          (update-in [ht 0] #(into [] (conj % -max)))))))

;; dispatching (I <3 multimethods)

(defmulti execute-cmd
  (fn [cmd _] (first cmd)))

(defmethod execute-cmd :take
  [[_ {:keys [value bot]}] state]
  (let [kw (bot-kw bot)]
    (cond
      (vector? (first (state kw))) (update-in state [kw 0]
                                              #(conj % value))
      :else (assoc state kw [[value]]))))

(defmethod execute-cmd :give
  [[_ {:keys [bot low high]}] state]
  (schedule-give bot low high state))


;; logic


(defn can-give? [bot state]
  (let [kw (bot-kw bot)
        chips (first (state kw))
        tasks (second (state kw))]
    (and (contains? state kw)
         (vector? chips)
         (= 2 (count chips))
         (vector? (first tasks)))))

(defn schedule-give [bot low high state]
  (let [kw (bot-kw bot)]
    (cond
      (vector? (second (state kw))) (update-in state [kw 1]
                                               #(into [] (conj % [low high])))
      (vector? (first (state kw))) (update-in state [kw]
                                              #(into [] (conj % [[low high]])))
      :else (assoc state kw [[] [[low high]]]))))
