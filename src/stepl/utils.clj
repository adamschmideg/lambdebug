(ns stepl.utils
  (:require
    [clojure
      [set :as set]]
    [clojure.contrib.str-utils2 :as s])
  (:use
    [clojure test
      [walk :only [prewalk]]]
    [clojure.contrib
      [repl-utils :only [get-source]]
      [seq-utils :only [flatten]]])
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (clojure.lang RT)))

(defmacro ??
  [& exprs]
  `(println " Debug:"
     ~@(mapcat
         (fn [x] 
           (if (string? x)
             `[~x]
             `['~x "=" (try ~x (catch Exception e# e#)) ","]) )
          exprs)))

(defn warn [& msgs]
  (apply println "Warning:" msgs))

(defn safe-ns-resolve
  [ns sym]
  (try
    (when (symbol? sym)
      (ns-resolve ns sym))
    (catch Exception e (?? sym e))))

(defn macro? 
  "Test if a symbol refers to a macro"
  [sym ns]
  (and 
    (symbol? sym)
    (:macro (meta (safe-ns-resolve ns sym)))))

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

(defn var-to-sym
  "Return a fully qualified symbol for var"
  [v]
  (when v
    (-> v str (subs 1) symbol)))

(defn var-source-form
  "Get source of var if it refers to a function"
  [v]
  (when-let [filepath (:file (meta v))]
    (when-let [strm (.getResourceAsStream (RT/baseLoader) filepath)]
      (with-open [rdr (LineNumberReader. (InputStreamReader. strm))]
        (dotimes [_ (dec (:line (meta v)))] (.readLine rdr))
        (let [text (StringBuilder.)
              pbr (proxy [PushbackReader] [rdr]
                    (read [] (let [i (proxy-super read)]
                               (.append text (char i))
                               i)))]
          (read (PushbackReader. pbr))
          (read-string (str text)))))))

(defn traverse
  "Traverse a graph using a transition function that returns
  nodes reachable from a given node. one-or-more-nodes is either a
  single node or a collection of them.  A set of nodes is returned."
  ([transition one-or-more-nodes]
    (traverse transition #{} (if (coll? one-or-more-nodes)
                               one-or-more-nodes
                               [one-or-more-nodes])))
  ([transition collected nodes]
    (if-let [to-visit (seq (remove collected nodes))]
      (let [visited (apply conj collected to-visit)]
        (set/union visited
          (traverse transition visited (mapcat transition to-visit))))
      collected)))

(defn used-vars-in-form
  "Return vars used in a form"
  [form ns]
  (->> form
    flatten
    distinct
    (filter symbol?)
    (remove
      #(or 
         (special-symbol? %)
         (macro? % ns)
         (var? %)))
    (map #(safe-ns-resolve ns %))
    (filter var?)))


(defn used-vars
  "Return vars used by a function"
  [func]
  (when-let [func-var (cond
                         (var? func)
                           func 
                         (symbol? func)
                           (resolve func)
                         (class? func)
                           nil
                         :else
                           (println "oops" func (type func)))]
    (if-let [form (var-source-form func-var)]
      (used-vars-in-form
        form
        (.ns func-var))
      (warn "No source for " func-var))))

;; to get all functions used by a function even indirectly:
;; (traverse used-vars #'com.acme/foo)
(with-test
  (defn flip
    "Flip a sequence of map with roughly the same keys to a map of sequences"
    ([maps] (flip maps (keys (first maps))))
    ([maps ks]
      (let [nils (zipmap ks (repeat nil))]
        (apply merge-with conj
          (zipmap ks (repeat []))
          (for [m maps]
            (merge nils m))))))
  (is (= {:a [1 nil], :b [2 9]}
         (flip [{:a 1 :b 2} {:b 9}]))))

(with-test
  (defn column-display
    "Convert items to strings right-padded with spaces so they are all of
    equal width"
    [coll]
    (let [strings (map str coll)
          width (apply max (map count strings))
          fmt (format "%%-%ss" width)]
      (map #(format fmt %) strings)))
  (is (= ["1  " "abc" "[] "]
        (column-display [1 "abc" []]))))

(defn maps-to-lol
  ([maps] (maps-to-lol maps (keys (first maps))))
  ([maps ks]
    (map #(map % ks) maps)))

(with-test
  (defn first-diff
    "Return the first different values and the path to them,
    or nil if the collections equal"
    ([coll1 coll2] (first-diff coll1 coll2 0 []))
    ([c1 c2 index path]
      (if (and (coll? c1) (coll? c2))
        (condp = [(empty? c1) (empty? c2)]
          [true true]
            nil
          [true false]
            [nil (first c2) (conj path index)]
          [false true]
            [(first c1) nil (conj path index)]
          [false false]
            (or
              (first-diff (first c1) (first c2) 0 (conj path index))
              (first-diff (rest c1) (rest c2) (inc index) path)
              ))
        (if (= c1 c2)
          nil
          [c1 c2 path]))))
  (are [c1 c2 _ diff] (is (= (first-diff c1 c2) diff))
    [1], [2] :>> [1 2 [0]]
    [1], [1 9] :>> [nil 9 [1]]
    [1 [2]], [1 [3]] :>> [2 3 [1 0]]
    [1 [2]], [1 [2 3]] :>> [nil 3 [1 1]]))

(defn assert-eq
  [expected got]
  (is (= expected got) (first-diff expected got)))

(defn resolve-tree
  "Resolve symbols in form, except for symbols from core.
  They can be resolved anyway, and they are sometimes used
  as variables (e.g. 'name')."
  [ns form]
  (let [core (find-ns 'clojure.core)]
    (prewalk
      (fn [x]
        (let [resolved (safe-ns-resolve ns x)
              good-ns (and
                         (var? resolved)
                         (not (= core (.ns resolved))))]
          (if (and resolved good-ns)
            resolved
            x)))
      form)))
