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
      utils])
  (:import [java.util.Date]))

(def *level* 0)
(def *function* nil)
(def *path* [])
(def *trace-enabled* nil)

(def *traces* (agent nil))
(def *function-forms* (agent nil))

(defmacro enter-function
  [name ns form]
  `(binding [*function* (ns-resolve ~ns ~name)
             *path* []]
      ~form))

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
  [path pairs ns]
  (into []
    (map #(if (even? (first %))
            (fnext %)
            (trace-form (conj path (first %)) (fnext %) ns))
       (indexed pairs))))

(defn trace-map*
  [path form ns]
  `(tr ~path ~form
      ~(zipmap
          (map (fn [[idx frm]] (trace-form [(* 2 idx)] frm ns))
            (indexed (keys form)))
          (map (fn [[idx frm]] (trace-form [(inc (* 2 idx))] frm ns))
            (indexed (vals form))))))

(defn trace-set*
  [path form ns]
  `(tr ~path ~form 
      ~(set 
          (for [item (indexed (set-seq form))]
             (trace-form [(first item)] (second item) ns)))))

(defn trace-vector*
  [path form ns]
  `(tr ~path ~form
      ~(into []
          (for [item (indexed form)]
            (trace-form [(first item)] (second item) ns)))))

(defn trace-seq
  [path form ns trace-whole? from-index]
  (let [children (map (fn [kid idx]
                        (if (>= idx from-index)
                          (trace-form
                            (if trace-whole?
                              [idx]
                              (conj path idx))
                            kid ns)
                          kid))
                      form
                      (iterate inc 0))]
    (if trace-whole?
      `(tr ~path ~form
         ~children)
      children)))

(with-test
  (defn trace-multi-binding*
    "Trace a macro whose first arg is a vector of
     free vars and values to be bound.  Only the values and the body is
     traced:
       (let [a b c d] ...) =>
       (let [a (tr b) c (trace d)] ..."
    [path form ns]
    (let [func (first form)
          params (fnext form)
          body (nnext form)]
      `(tr ~path ~form
          (~func
           ~(trace-pairs* [1] params ns)
           ~@(for [item (indexed body)]
              (trace-form [2] (second item) ns))))))
)

(defn trace-fn
  "Trace fn in both forms, 
    (fn [x] ...), or (fn ([x] ..) ([x y] ..))"
  ([path form ns] (trace-fn path form nil))
  ([path form name ns]
    (if (vector? (fnext form))
      (trace-seq path form ns true 2)
      (let [bodies (map (fn [body idx]
                          (let [traced (trace-seq
                                         (conj path idx) body ns false 1)]
                             `(~(first traced)
                               (enter-function '~name '~ns
                                 (do ~@(next traced))))))
                      (next form)
                      (iterate inc 1))]
      `(tr ~path ~form
        (fn ~@bodies))))))

(defn trace-symbol*
  "Trace symbols, trying to resolve them first, to display a nicer name"
  [path form ns]
  `(tr ~path ~form ~(or (ns-resolve ns form) form)))

(defn trace-primitive*
  [path form ns]
  `(tr ~path ~form ~form))

(defn trace-form 
  ([form ns] (trace-form nil form ns))
  ([path form ns]
  (cond
    (seq? form)
      (let [func (first form)]
        (if (or
              (special-symbol? func)
              (macro? func))
           (condp contains? func
              #{'let 'for 'doseq 'binding}
                (trace-multi-binding* path form ns)
              #{'fn*}
                (trace-seq path form ns true 2)
              #{'fn}
                (trace-fn path form ns)
              #{'catch}
                (trace-seq path form ns false 3)
              #{'finally}
                (trace-seq path form ns false 1)
              ;; default for macros
                (trace-seq path form ns true 1))
           (trace-seq path form ns true 0)))
    (map? form)
      (trace-map* path form ns)
    (vector? form)
      (trace-vector* path form ns)
    (set? form)
      (trace-set* path form ns)
    (symbol? form)
      (trace-symbol* path form ns)
    :else
      (trace-primitive* path form ns))))

(defn trace-defn [form ns]
  (let [[_ name fn-form] (macroexpand-1 form)]
    (trace-fn [] fn-form name ns)))

(defn make-steps
  "Trace form, evaluate it, and return the steps taken"
  [form]
  (do
     (send *traces* (constantly []))
     (await-for 1000 *traces*)
     (try
       (eval (trace-form [] form *ns*))
       (catch Throwable t t))
     (await-for 1000 *traces*)
     @*traces*))

