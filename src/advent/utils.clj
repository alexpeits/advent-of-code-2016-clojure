(ns advent.utils
  (:require [clojure.java.io :as io]))

(defn read-resource [name]
  (slurp (io/resource name)))
