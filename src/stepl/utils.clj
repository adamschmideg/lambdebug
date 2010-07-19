(ns stepl.utils
  (:require
    [clojure.contrib.str-utils2 :as s]))

(defmacro either
  "Return either [nil result] in normal case,
  or [exception nil] if an an exception was thrown."
  [expr]
  `(try [nil ~expr] 
    (catch Throwable t# [t# nil])))

(defn repeat-str [n str]
  (s/join "" (repeat n str)))

(defn set-seq
  "Make a seq from a set that keeps its order.
   Note: (seq coll) wouldn't do that."
  [coll]
  (iterator-seq (.iterator coll)))

(defmacro ??
  [exp]
  `(println "debug:" '~exp ~exp))
