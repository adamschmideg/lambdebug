(ns #^{:doc "Step and debug in clojure REPL"
       :author "Adam Schmideg"}
  lambdebug.debugger
  (:require
    [clojure
      [string :as s]
      [pprint :as pp]])
  (:use
    [clojure
      test]
    [lambdebug utils]))

(def *trace-index* (agent 0))

; bright inverse yellow
(def *left-marker* (str
  \u001b "[1m"
  ;\u001b "[7m"
  \u001b "[33m"
  "<<< "))

(def *right-marker* (str " >>>" \u001b "[0m"))


(with-test
  (defn at
    "Get the nth item of coll.
     It works with maps and sets, too."
    [coll n]
    (cond
      (or (vector? coll)
          (list? coll))
        (nth coll n nil)
      (map? coll)
        (let [entry (nth (seq coll)
                         (int (/ n 2))
                         nil)]
           (if (even? n)
              (first entry)
              (second entry)))
      (set? coll)
         (nth (set-seq coll) n nil)
      :else
         nil))
  (is (= 42 (at [:a 42 :b] 1)))
  (is (= 42 (at '(:a 42 :b) 1)))
  (is (= 42 (at {:a 42 :b 99} 1)))
  (is (= :b (at {:a 42 :b 99} 2)))
  (is (= 42 (at #{:a 42 :b} 1)))
  (is (= :b (at #{:a 42 :b} 2))))

(with-test
  (defn edit-at
    "Change the nth value in coll to (f old-value).
     It works with maps and sets, too.
     If n is out of bounds, the collection is returned unchanged."
    [coll n f]
    (cond
      (vector? coll)
        (if (contains? coll n)
          (assoc coll n (f (nth coll n)))
          coll)
      (map? coll)
        (let [index (int (/ n 2))
              ks (keys coll)
              vs (vals coll)]
          (if (even? n)
            (zipmap
              (edit-at (apply vector (keys coll)) index f)
              (vals coll))
            (zipmap
              (keys coll)
              (edit-at (apply vector (vals coll)) index f))))
      (set? coll)
         ;; (seq coll) wouldn't keep order
        (let [items (apply vector (set-seq coll))]
          (set
            (edit-at items n f)))
      (coll? coll)
        (seq
          (edit-at (apply vector coll) n f))
      :else
        coll))
  (are
    [coll n _ result] 
    (is (= (edit-at coll n str) result))
    [:a 42 :b] 1    :>> [:a "42" :b]
    '(:a 42 :b) 1   :>> '(:a "42" :b)
    {:a 42 99 :b} 0 :>> {":a" 42 99 :b}
    {:a 42 99 :b} 1 :>> {:a "42" 99 :b}
    {:a 42 99 :b} 2 :>> {:a 42 "99" :b}
    #{:a 42 :b} 1   :>> #{:a "42" :b}
    #{:a 42 :b} 2   :>> #{:a 42 ":b"}
    [1 2] 99        :>> [1 2]))

(with-test
  (defn edit-path
    "Return the nested collection with the item at path changed to
      (f old-value).  The collection can contains any combination of
      lists, vectors, maps, and sets.
      If path contains an index out of bounds, the collection is
      returned unchanged."
    [coll path f]
    (let [head (first path)
          tail (seq (next path))]
      (if head
        (if tail
          (edit-at coll head #(edit-path % tail f))
          (edit-at coll head f))
        (f coll))))
  (is (= (edit-path [1 2 3] [] str)
         "[1 2 3]"))
  (is (= (edit-path 
            [:a {:b 42} :c] [1 0] str)
            [:a {":b" 42} :c]))
  (is (= (edit-path 
            [:a {:b 42} :c] [1] str)
            [:a "{:b 42}" :c]))
  (is (= (edit-path [:a :b] [99] str)
            [:a :b])))

(defn decorate
  "Pretty print a form and surround it with decorations"
  [form]
  (str *left-marker*
    (s/trim
      (with-out-str
        (pp/with-pprint-dispatch pp/code-dispatch
          (pp/pprint form))))
    *right-marker*))

(defn print-trace
  "Print the input form of a trace and its result if it has one.
  - functions: a map of function forms"
  [traces index functions]
  (let [trace (nth traces index)
        func-form (functions (trace :function))
        form (edit-path func-form (trace :path) decorate)]
      (println "Function:" (or (trace :function) "Given at REPL"))
      (print "Form: ")
      (pp/with-pprint-dispatch pp/code-dispatch
        (pp/pprint form))
      (when-let [result (trace :result)]
        (print "Result: ")
        (pp/pprint result))))

(defn step-in
  "Go to the next trace, even when entering to a lower level"
  [traces index]
  (inc index))

(defn step-next
  "Go to the next trace of the same level"
  [traces index]
  (let [traces (drop index traces)
        level (:level (first traces))]
    (+ 1 index (count (take-while #(< level (:level %)) 
                         (next traces))))))

(defn step-prev
  "Go to the previous trace, sort of inverse of step-next"
  [traces index]
  (let [traces (reverse (take (inc index) traces))
        level (:level (first traces))
        offset (count (take-while #(< level (:level %))
                        (next traces)))]
    (- index offset 1)))

(defn step-back
  "Go one step back, the inverse of step-in"
  [traces index]
  (dec index))

(defn step-out
  "Go one level up, returning from the current function"
  [traces index]
  (let [traces (drop index traces)
        level (:level (first traces))]
    (+ 1 index (count (take-while #(<= level (:level %))
                        (next traces))))))

(defn print-usage
  [& _]
  (println "Help: choose
    step (i)n, (n)ext, (b)ack, (p)rev, (o)ut"))
    
(def *COMMANDS*
  {"i" step-in
   "n" step-next
   "b" step-back
   "p" step-prev
   "o" step-out
   "h" print-usage})

(defn make-dispatcher
  "Call the appropriate stepping functions based on command,
  store trace index, and print new trace"
  [traces function-forms]
  (fn [command]
    (if-let [func (*COMMANDS* command)]
      (let [index (func traces @*trace-index*)]
        (when (contains? traces index)
          (send *trace-index* (constantly index))
          (await-for 1000 *trace-index*))
        (print-trace traces @*trace-index* function-forms))
      (print-usage))))

(defn gui
  "Start an interactive gui, read commands, print the result of
   (dispatcher command) until (exit command) is true."
  [exit dispatcher first-command]
  (let [prompt ">>> "]
    (dispatcher first-command)
    (print prompt)
    (flush)
    (loop [command (read-line)]
      (when-not (exit command)
        (dispatcher command)
        (print prompt)
        (flush)
        (recur (read-line))))))
