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
  (defn diff?
    "Return the first different values and the path to them,
    or nil if the collections equal"
    ([coll1 coll2] (diff? coll1 coll2 []))
    ([coll1 coll2 path]
      (if (and (coll? coll1) (coll? coll2))
        (some
          #(let [[idx [c1 c2]] %]
            (diff? c1 c2 (conj path idx)))
          (indexed (partition 2 (interleave coll1 coll2))))
        (if (= coll1 coll2)
          nil
          [coll1 coll2 path]))))
  (are [c1 c2 diff] (is (= (diff? c1 c2) diff))
    [1] [2]  [1 2 [0]]
    [1] [1 9]  [nil 9 [1]]
    ))

;; helpers for testing trace-related stuff
(defn make-steps
  "Trace form, evaluate it, and return the steps taken"
  [form]
  (do
     (send *traces* (constantly []))
     (eval (trace-form form))
     (await-for 1000 *traces*)
     @*traces*))

(defn check-trace-form
  "Check if tracing form results in the expected steps"
  [form expected]
  (let [got (simplify-traces (make-steps form))]
     (is (= got expected))))

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
