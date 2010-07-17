(ns
  stepl.core-test
  (:use
    [clojure.test]
    [clojure.contrib
      [seq-utils :only (flatten indexed)]]
    [stepl]))

;; generic utils
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

(defn simplify-traces
  "Show only traced form and its direction, :>> for entering,
   and :<< for leaving"
  [traces]
  (mapcat #(vector
              (if (:result %) :<< :>>)
              (:form %))
    traces))

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

;; helpers for testing trace-related stuff
(defn make-steps
  "Trace form, evaluate it, and return the steps taken"
  [form]
  (do
     (send *traces* (constantly []))
     (await-for 1000 *traces*)
     (eval (trace-form form))
     (await-for 1000 *traces*)
     @*traces*))

(defn check-trace-form
  "Check if tracing form results in the expected steps"
  [form expected]
  (let [got (simplify-traces (make-steps form))]
    (is (not (first-diff got expected)))))

(deftest trace-form-test
  (testing "simple functions"
    (check-trace-form
      '(* (+ 1 2) (inc 3))
      [
        :>> '(* (+ 1 2) (inc 3))
        :>> '*
        :<< '*
        :>> '(+ 1 2)
        :>> '+
        :<< '+
        :>> 1
        :<< 1
        :>> 2
        :<< 2
        :<< '(+ 1 2)
        :>> '(inc 3)
        :>> 'inc
        :<< 'inc
        :>> 3
        :<< 3
        :<< '(inc 3)
        :<< '(* (+ 1 2) (inc 3))]))
  (testing "for (list comprehension)"
    (check-trace-form
      '(for [i (range 2)] 42)
      [
        :>> '(for [i (range 2)] 42)
        :>> '(range 2)
        :>> 'range
        :<< 'range
        :>> 2
        :<< 2
        :<< '(range 2)
        :<< '(for [i (range 2)] 42)
        :>> 42
        :<< 42
        :>> 42
        :<< 42])))
