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
    [stepl
      [core :only [trace-defn *function-forms* *traces* *trace-enabled*]]
      [utils :only [repeat-str either]]])
  (:import [java.util.Date]))

(defn trace-func [func]
  (let [md (meta (resolve func))
        name (:name md)
        ns (:ns md)]
    (when-let [source (get-source func)]
      (let [form (read-string source)]
        (when (#{'defn 'defn-} (first form))
          (send *function-forms* 
            assoc 
            (resolve (second form))
            (nnext form))
          (let [new-form (trace-defn form)
                new-fn (eval new-form)]
            (intern ns (with-meta name md) new-fn)))))))

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
      (map #(format template 
              (str (repeat-str (:level %) "=")
                (if (:result %) "<" ">"))
              (str (:function %))
              (str (repeat-str (count (:path %)) "-")
                (if (:result %) "<" ">"))
              (if (:result %)
                (str (:form %) " = " (:result %))
                (str (:form %))))
         trace)))

(defn debug
  [expr]
  (binding [*trace-enabled* true]
     (send *traces* (constantly []))
     (let [[exception result] (either (eval expr))]
       (await-for 1000 *traces*)
       (doseq [tr (format-trace @*traces*)] (print tr))
       ;; call debugger
       (dbg/gui #(= "q" %) 
         (dbg/make-dispatcher @*traces* @*function-forms*))
       (or result exception))))
