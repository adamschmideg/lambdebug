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

(defn trace-ns [ns-pattern]
  (let [namespaces (filter #(.startsWith (str %) ns-pattern)
                     (loaded-libs))]
    (doseq [ns namespaces]
      (do 
        (doseq [func (keys (ns-publics (symbol ns)))]
          (println "Tracing" func)
          (trace-func func))))))

(defn format-trace
  [trace]
  (let [widths (apply merge-with #(max %1 
                                    (cond
                                      (number? %2)
                                        %2
                                      (vector? %2)
                                        (count %2)
                                      :else
                                        (count (str %2))))
                  {:function 0, :path 0, :level 0, :form 0, :result 0}
                  trace)
        template (format "%%-%ss %%-%ss %%-%ss %%-%ss %n" 
                    (inc (:level widths)) (:function widths)
                    (inc (:path widths)) (max (:form widths) 
                                        (:result widths)))]
      (println "template" template)
      (map #(format template 
              (str (repeat-str (:level %) "=")
                (if (:result %) "<" ">"))
              (str (:function %))
              (str (repeat-str (count (:path %)) "-")
                (if (:result %) "<" ">"))
              (if (find % :result)
                (str (:form %) " = " (:result %))
                (str (:form %))))
         trace)))

(defn debug
  [form]
  (do
    (trace-used-vars form)
    (send *function-forms* assoc nil form)
    (binding [*trace-enabled* true]
      (dbg/gui #(= "q" %) 
        (dbg/make-dispatcher (make-steps form) @*function-forms*)))))

(defn nice-steps
  "Trace form and functions used by it.  Functions can be filtered a
  `wanted` function"
  ([form]
    (nice-steps form (complement #(= (.ns %) (find-ns 'clojure.core)))))
  ([form wanted]
    (let [vars (traverse used-vars (used-vars-in-form form *ns*))
          wanted-vars (filter wanted vars)]
      (doseq [v wanted-vars]
        (trace-func v))
      (doseq [line (maps-to-lol
                    (remove #(find % :result) 
                      (make-steps form))
                    [:path :form :function])]
      (println line)))))
