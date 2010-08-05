(ns
  stepl.coverage
  (:use
    [stepl :only [trace-func]]
    [clojure.contrib
      [find-namespaces :only [find-namespaces-on-classpath]]
      [ns-utils :only [ns-vars]]]))

(defn safe-namespaces
  []
  (->> (find-namespaces-on-classpath)
    (filter #(.startsWith (name %) "clojure.contrib"))
    (remove #{'clojure.core 'clojure.main})
    (remove #(.startsWith (name %) "clojure.contrib.datalog"))
    (remove #{'clojure.contrib.datalog.example
              'clojure.contrib.datalog
              })))

(defn trace-and-compile
  ([] (trace-and-compile (safe-namespaces)))
  ([namespaces]
    (remove nil?
      (apply concat
        (for [ns namespaces]
          (do
            (require ns)
            (remove nil?
              (for [var (ns-vars ns)]
                (let [func-var (ns-resolve ns var)]
                  (try
                    (trace-func func-var false)
                    (catch Exception e [func-var e])))))))))))

(defn sort-result
  [funcs-and-errors]
  (sort
    #(compare
      (str (second %1))
      (str (second %2)))
    funcs-and-errors))

; (sort-result (doall (trace-and-compile)))
