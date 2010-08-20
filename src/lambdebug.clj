;; Inspired by ctrace
;; http://wave.thewe.net/2009/12/17/logging-and-debugging-in-clojure-followup-complete-tracing/
(ns #^{:doc "Step and debug in clojure REPL"
       :author "Adam Schmideg"}
  lambdebug
  (:require
    [lambdebug
      [debugger :as dbg]])
  (:use 
    [lambdebug core utils]))

(defn debug
  [form]
  (do
    (trace-used-vars form)
    (send *function-forms* assoc nil form)
    (binding [*trace-enabled* true]
      (dbg/gui #(= "q" %) 
        (dbg/make-dispatcher (make-steps form) @*function-forms*)
        "i"))))
