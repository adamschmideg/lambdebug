(ns
  stepl.core-test
  (:use
    [clojure.test]
    [clojure.contrib
      [seq-utils :only (flatten indexed)]]
    [stepl.core]))

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

(defn nice-steps
  [form]
  (doseq [line (maps-to-lol
                (remove #(find % :result) 
                  (make-steps form))
                [:path :form])]
    (println line)))

(defn check-trace-form
  "Check if tracing form results in the expected steps"
  [form expected]
  (let [got (apply concat
              (maps-to-lol
                (remove #(find % :result) 
                  (make-steps form))
                [:path :form]))]
    (is (not (first-diff got expected)))))

(deftest trace-form-test
  (testing "simple functions"
    (check-trace-form
      '(* (+ 1 2) (inc 3))
      [
        [] '(* (+ 1 2) (inc 3))
        [0] '*
        [1] '(+ 1 2)
        [1 0] '+
        [1 1] 1
        [1 2] 2
        [2] '(inc 3)
        [2 0] 'inc
        [2 1] 3]))
  (testing "for (list comprehension)"
    (check-trace-form
      '(for [i (range 2)] 42)
      [
        [] '(for [i (range 2)] 42)
        [1 1] '(range 2)
        [1 1 0] 'range
        [1 1 1] 2]))
  (testing "let form"
    (check-trace-form
      '(let [a 5 b 9] (+ a b))
      [
        [] '(let [a 5 b 9] (+ a b))
        [1 1] 5
        [1 3] 9
        [2] '(+ a b)
        [2 0] '+
        [2 1] 'a
        [2 2] 'b]))
  (testing "logical"
    (check-trace-form
      '(and nil 2)
      [
        [] '(and nil 2)
        [1] nil])))
