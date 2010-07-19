(ns #^{:doc "Step and debug in clojure REPL"
       :author "Adam Schmideg"}
  stepl.core
  (:require
    [clojure.contrib
      [str-utils2 :as s]])
  (:use 
    [clojure.test]
    [clojure.contrib.pprint]
    [clojure.contrib.repl-utils]
    [clojure.contrib.duck-streams]
    [clojure.contrib [seq-utils :only (indexed)]]
    [stepl
      [utils :only [repeat-str set-seq either]]])
  (:import [java.util.Date]))

(def *level* 0)
(def *function* nil)
(def *path* [])
(def *trace-enabled* nil)

(def *traces* (agent nil))
(def *function-forms* (agent nil))

;; utils {{{
;; }}}

(defn macro? 
  "Test if a symbol refers to a macro"
  [sym]
  (and 
    (symbol? sym)
    (:macro (meta (resolve sym)))))

(defmacro enter-function
  [name args body]
  `(binding [*function* (resolve ~name)
             *path* []]
      ~body))

(defmacro tr
  [path form decorated-form]
  `(binding [*level* (inc *level*)
             *path* (into *path* ~path)]
     (send *traces* conj
        {:function *function* :path *path* :level *level*
         :form '~form})
     (let [[exception# result#] (either ~decorated-form)]
       (send *traces* conj
          {:function *function* :path *path* :level *level*
           :form '~form :result (or result# exception#)})
       (if exception#
         (throw exception#)
         result#))))

(declare trace-form)

(defn trace-pairs*
  "Decorate even elements of a vector with trace"
  [path pairs]
  (into []
    (map #(if (even? (first %))
            (fnext %)
            (trace-form (conj path (first %)) (fnext %)))
       (indexed pairs))))

(defn trace-map*
  [path form]
  `(tr ~path ~form
      ~(zipmap
          (map (fn [[idx frm]] (trace-form [(* 2 idx)] frm))
            (indexed (keys form)))
          (map (fn [[idx frm]] (trace-form [(inc (* 2 idx))] frm))
            (indexed (vals form))))))

(defn trace-set*
  [path form]
  `(tr ~path ~form 
      ~(set 
          (for [item (indexed (set-seq form))]
             (trace-form [(first item)] (second item))))))

(defn trace-list*
  [path form]
  `(tr ~path ~form
      ~(for [item (indexed form)]
          (trace-form [(first item)] (second item)))))

(defn trace-vector*
  [path form]
  `(tr ~path ~form
      ~(into []
          (for [item (indexed form)]
            (trace-form [(first item)] (second item))))))

(defn trace-multi-binding*
  "Trace a macro whose first arg is a vector of
   free vars and values to be bound.  Only the values and the body is
   traced:
     (let [a b c d] ...) =>
     (let [a (tr b) c (trace d)] ..."
  [path form]
  (let [func (first form)
        params (fnext form)
        body (nnext form)]
    `(tr ~path ~form
        (~func
         ~(trace-pairs* [1] params)
         ~@(for [item (indexed body)]
            (trace-form [2] (second item)))))))

(defn trace-single-binding*
  "Trace a macro whose first arg is a param list, like fn*.  Only the
  body is traced:
    (fn* [a b] (inc a)) =>
    (fn* [a b] (tr (inc a) ..."
  [path form]
  (let [func (first form)
        params (fnext form)
        body (nnext form)]
    `(tr ~path ~form
        (~func
         ~params
         ~@(for [item (indexed body)]
            (trace-form [2 (first item)] (second item)))))))

(defn trace-rest*
  "Keep the head unchanged, tr only the rest.
   Used for most macros"
  [path form]
  `(tr ~path ~form
     (~(first form)
      ~@(for [item (indexed (next form))]
         (trace-form [(inc (first item))] (second item))))))

(defn trace-fn
  "Trace fn in both forms, 
    (fn [x] ...), or (fn ([x] ..) ([x y] ..))"
  [path form]
  (if (vector? (fnext form))
    (trace-single-binding* path form)
    `(fn
      ~@(for [body (indexed (next form))]
          (trace-rest* [(inc (first body))] (second body))))))

(defn trace-symbol*
  "Trace symbols, trying to resolve them first, to display a nicer name"
  [path form]
  `(tr ~path ~form ~(or (resolve form) form)))

(defn trace-primitive*
  [path form]
  `(tr ~path ~form ~form))

(defn trace-form 
  ([form] (trace-form nil form))
  ([path form]
  (cond
    (seq? form)
      (let [func (first form)]
        (if (or
              (special-symbol? func)
              (macro? func))
           (condp contains? func
              #{'let 'for 'doseq 'binding}
                (trace-multi-binding* path form)
              #{'fn*}
                (trace-single-binding* path form)
              #{'fn}
                (trace-fn path form)
              ;; default
                (trace-rest* path form))
           (trace-list* path form)))
    (map? form)
      (trace-map* path form)
    (vector? form)
      (trace-vector* path form)
    (set? form)
      (trace-set* path form)
    (symbol? form)
      (trace-symbol* path form)
    :else
      (trace-primitive* path form))))

(defn trace-body [name args body]
  (list args 
     `(enter-function '~name '~args ~(trace-form [1] body))))

(defn trace-defn [form]
  (let [fn-form (macroexpand-1 form)
        [_ name [_ & bodies]] fn-form
        bodies (map #(let [[arg body] %]
                        (trace-body name arg body))
                  bodies)]
    `(fn ~@bodies)))

