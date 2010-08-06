(ns
  stepl.coverage
  (:use
    [stepl 
      [core :only [trace-defn]]
      [utils :only [get-var-source]]]
    [clojure
      [stacktrace :only [root-cause]]]
    [clojure.contrib
      [find-namespaces :only [find-namespaces-on-classpath]]
      [ns-utils :only [ns-vars]]]))

(defn safe-namespaces
  []
  (->> (find-namespaces-on-classpath)
    (filter #(.startsWith (name %) "clojure.contrib"))
    (remove #{'clojure.core 'clojure.main})
    (remove #(.startsWith (name %) "clojure.contrib.datalog"))
    (remove #(.startsWith (name %) "clojure.contrib.probabilities"))
    (remove #{'clojure.contrib.javadoc
              'clojure.contrib.test-contrib.walk
              })))

(defn decorate
  [func-var]
    (let [md (meta func-var)
          name (:name md)
          ns (:ns md)]
      (when-let [source (get-var-source func-var)]
        (let [form (read-string source)]
          (when (#{'defn 'defn-} (first form))
            (trace-defn form ns))))))

(defn check-ns
  [ns]
    (do
      (require ns)
      (for [var (ns-vars ns)]
        (let [func-var (ns-resolve ns var)]
          (try
            (eval (decorate func-var))
            [func-var nil]
            (catch Exception e [func-var (root-cause e)]))))))

(defn check-all
  ([] (check-all (safe-namespaces)))
  ([namespaces]
    (mapcat check-ns namespaces)))

(defn sort-result
  [funcs-and-errors]
  (sort
    #(compare
      (str (second %1))
      (str (second %2)))
    funcs-and-errors))

; (sort-result (doall (trace-and-compile)))
