(ns advent.day05
  (:use [advent.utils :only [read-resource]])
  (:require [clojure.string :as str])
  (:import java.security.MessageDigest
           java.math.BigInteger))

(def data "cxdnnyjw")

(defn md5 [s]
  (let [algo (MessageDigest/getInstance "MD5")
        raw (.digest algo (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn valid-hash? [hash]
  (str/starts-with? hash "00000"))


(defn solution-a []
  (transduce
   (comp
    (map #(md5 (str data %)))
    (filter valid-hash?)
    (map #(nth % 5))
    (take 8))
   conj
   []
   (range)))

;; or (slower)
#_(->> (range)
       (map #(md5 (str data %)))
       (filter valid-hash?)
       (map #(nth % 5))
       (take 8))

(defn char->int [c]
  (Integer/parseInt (str c)))


(defn solution-b []
  (loop [res (zipmap (range 8) (repeat 8 nil))
         val 0]
    (if (every? #(not (nil? %)) (vals res))
      res
      (let [hash (md5 (str data val))
            i (nth hash 5)
            res (if (and (valid-hash? hash)
                         (Character/isDigit i)
                         (< (char->int i) 8)
                         (nil? (res (char->int i))))
                  (assoc res (char->int i) (nth hash 6))
                  res)]
        (recur res (inc val))))))
