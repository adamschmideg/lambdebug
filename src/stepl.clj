;; Inspired by ctrace
;; http://wave.thewe.net/2009/12/17/logging-and-debugging-in-clojure-followup-complete-tracing/
(ns #^{:doc "Step and debug in clojure REPL"
       :author "Adam Schmideg"}
  stepl
  (:require
    [clojure.contrib
      [str-utils2 :as s]]
    [stepl
      [debugger :as dbg]])
  (:use 
    [clojure.test]
    [clojure.contrib.pprint]
    [clojure.contrib.repl-utils]
    [clojure.contrib.duck-streams]
    [clojure.contrib [seq-utils :only (indexed)]]
    [stepl core utils])
  (:import [java.util.Date]))

(defn debug
  [form]
  (do
    (trace-used-vars form)
    (send *function-forms* assoc nil form)
    (binding [*trace-enabled* true]
      (dbg/gui #(= "q" %) 
        (dbg/make-dispatcher (make-steps form) @*function-forms*)
        "i"))))
