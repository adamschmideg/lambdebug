;; Inspired by ctrace
;; http://wave.thewe.net/2009/12/17/logging-and-debugging-in-clojure-followup-complete-tracing/
(ns #^{:doc "Step and debug in clojure REPL"
       :author "Adam Schmideg"}
  stepl
  (:require
    [clojure.contrib
      [str-utils2 :as s]])
  (:use 
    [clojure.test]
    [clojure.contrib.pprint]
    [clojure.contrib.repl-utils]
    [clojure.contrib.duck-streams]
    [clojure.contrib [seq-utils :only (indexed)]])
  (:import [java.util.Date]))

(def *level* 0)

(defn macro? 
  "Test if a symbol refers to a macro"
  [sym]
  (:macro (meta (resolve sym))))

(defn function-called [name args]
  (println name args))

(defmacro trace
  [form decorated-form]
  `(binding [*level* (inc *level*)]
     (let [indent# (s/join "" (repeat *level* "-"))]
       (println (str indent# ">") '~form)
       (let [result# ~decorated-form]
         (println (str indent# "<") result#)
         result#))))

(declare trace-form)

(defn trace-pairs*
  "Decorate even elements of a vector with trace"
  [pairs]
  (into []
    (map #(if (even? (first %))
            (fnext %)
            (trace-form (fnext %)))
       (indexed pairs))))

(defn trace-map*
  [form]
  `(trace ~form
      ~(zipmap
          (map trace-form (keys form))
          (map trace-form (vals form)))))

(defn trace-set*
  [form]
  `(trace ~form 
      ~(set 
          (for [item form]
             (trace-form item)))))

(defn trace-list*
  [form]
  `(trace ~form
      ~(for [item form]
          (trace-form item))))

(defn trace-vector*
  [form]
  `(trace ~form
      ~(into[]
          (for [item form]
            (trace-form item)))))

(defn trace-multi-binding*
  "Trace a macro whose first arg is a vector of
   free vars and values to be bound.  Only the values and the body is
   traced:
     (let [a b c d] ...) =>
     (let [a (trace b) c (trace d)] ..."
  [form]
  (let [func (first form)
        params (fnext form)
        body (nnext form)]
    `(trace ~form
        (~func
         ~(trace-pairs* params)
         ~@(for [item body]
            (trace-form item))))))

(defn trace-single-binding*
  "Trace a macro whose first arg is a param list, like fn*.  Only the
  body is traced:
    (fn* [a b] (inc a)) =>
    (fn* [a b] (trace (inc a) ..."
  [form]
  (let [func (first form)
        params (fnext form)
        body (nnext form)]
    `(trace ~form
        (~func
         ~params
         ~@(for [item body]
            (trace-form item))))))

(defn trace-rest*
  "Keep the head unchanged, trace only the rest.
   Used for most macros"
  [form]
  `(trace ~form
     (~(first form)
      ~@(for [item (next form)]
         (trace-form item)))))

(defn trace-symbol*
  "Trace symbols, trying to resolve them first, to display a nicer name"
  [form]
  `(trace ~form ~(or (resolve form) form)))

(defn trace-primitive*
  [form]
  form)

(defn trace-form [form]
  (cond
    (seq? form)
      (let [func (first form)]
        (if (or
              (special-symbol? func)
              (macro? func))
           (condp contains? func
              #{'let 'for 'doseq 'binding}
                (trace-multi-binding* form)
              #{'fn*}
                (trace-single-binding* form)
              ;; default
                (trace-rest* form))
           (trace-list* form)))
    (map? form)
      (trace-map* form)
    (vector? form)
      (trace-vector* form)
    (set? form)
      (trace-set* form)
    (symbol? form)
      (trace-symbol* form)
    :else
      (trace-primitive* form)))

(defn trace-body [name args body]
  (list args 
    `(do
       (function-called '~name '~args)
       ~(trace-form body))))

(defn trace-defn [form]
  (let [fn-form (macroexpand-1 form)
        [_ name [_ & bodies]] fn-form
        bodies (map #(let [[arg body] %]
                        (trace-body name arg body))
                  bodies)]
    `(fn ~@bodies)))

(defn trace-func [func]
  (let [md (meta (resolve func))
        name (:name md)
        ns (:ns md)]
    (when-let [source (get-source func)]
      (let [form (read-string source)]
        (if (#{'defn 'defn-} (first form))
          (let [new-fn (eval (trace-defn form))]
            (intern ns (with-meta name md) new-fn)))))))

(defn trace-ns [ns-pattern]
  (let [namespaces (filter #(.startsWith (str %) ns-pattern)
                     (loaded-libs))]
    (doseq [ns namespaces]
      (do 
        (doseq [func (keys (ns-publics (symbol ns)))]
          (prn ">" func)
          (trace-func func))))))

(defn foo [a & [b]]
  (let [x (inc a)]
    (* b (inc x))))

(defn bar [b] (+ (foo 2 b) 3))

