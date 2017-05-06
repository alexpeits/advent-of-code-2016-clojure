(ns advent.day08
  (:use [advent.utils :only [read-resource]])
  (:require [clojure.string :as str]
            [clojure.set :as s]
            [instaparse.core :as insta]))

(def grammar (read-resource "day08_grammar.txt"))
(def dsl
  (insta/parser
   grammar
   :auto-whitespace :standard))
