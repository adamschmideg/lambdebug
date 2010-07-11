(ns #^{:doc "Step and debug in clojure REPL"
       :author "Adam Schmideg"}
  stepl.debugger
  (:use
    [clojure
      test]))

(defn set-seq
  "Make a seq from a set that keeps its order.
   Note: (seq coll) wouldn't do that."
  [coll]
  (iterator-seq (.iterator coll)))

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
      (list? coll)
        (seq
          (edit-at (apply vector coll) n f))
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
      (when head
        (if tail
          (edit-at coll head #(edit-path % tail f))
          (edit-at coll head f)))))
  (is (= (edit-path 
            [:a {:b 42} :c] [1 0] str)
            [:a {":b" 42} :c]))
  (is (= (edit-path 
            [:a {:b 42} :c] [1] str)
            [:a "{:b 42}" :c]))
  (is (= (edit-path [:a :b] [99] str)
            [:a :b])))
