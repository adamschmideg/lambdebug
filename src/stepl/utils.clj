(ns stepl.utils
  (:require
    [clojure
      [set :as set]]
    [clojure.contrib.str-utils2 :as s])
  (:use
    [clojure.contrib
      [repl-utils :only [get-source]]
      [seq-utils :only [flatten]]])
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (clojure.lang RT)))

(defn macro? 
  "Test if a symbol refers to a macro"
  [sym]
  (and 
    (symbol? sym)
    (:macro (meta (resolve sym)))))

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

(defn var-to-sym
  "Return a fully qualified symbol for var"
  [v]
  (-> v str (subs 1) symbol))

(defn get-var-source
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
          (str text))))))

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
    (let [ns (.ns func-var)]
      (->> func-var
        get-var-source
        read-string
        flatten
        distinct
        (filter symbol?)
        (remove macro?)
        (map #(ns-resolve ns %))
        (filter var?)))))

;; to get all functions used by a function even indirectly:
;; (traverse used-vars #'com.acme/foo)
