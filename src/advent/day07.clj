(ns advent.day07
  (:use [advent.utils :only [read-resource]])
  (:require [clojure.string :as str]
            [clojure.set :as s]))

(def data (str/split-lines (read-resource "day07.txt")))

(defn in-brackets [s]
  (map second (re-seq #"\[([a-z]+)\]" s)))

(defn out-brackets [s]
  (map second (re-seq #"([a-z]+)(?=$|\[)" s)))

(defn window [n s]  ;; (window 4 "12345678") -> ("1234" "2345" ...)
  (partition n 1 s))

(defn abba? [[a b c d]]
  (and (= a d)
       (= b c)
       (not= a b)))

(defn aba? [[a b c]]
  (and (= a c)
       (not= a b)))

(defn bab-match [[a b c]]
  (str b a b))

(defn supports-tls? [s]
  (let [in (in-brackets s)
        out (out-brackets s)]
    (and (every? #(not (abba? %)) (mapcat (partial window 4) in))
         (some abba? (mapcat (partial window 4) out)))))

(defn supports-ssl? [s]
  (let [in (in-brackets s)
        out (out-brackets s)
        abas (set
              (map bab-match
                   (filter aba?
                           (mapcat (partial window 3) out))))
        babs (set
              (map #(apply str %)
                   (filter aba?
                           (mapcat (partial window 3) in))))]
    (not (empty? (s/intersection abas babs)))))

(defn solution-a []
  (->> data
       (filter supports-tls?)
       count))

(defn solution-b []
  (->> data
       (filter supports-ssl?)
       count))
