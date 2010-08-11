(ns
  lambdebug.core-test
  (:use
    [clojure.test]
    [clojure.contrib
      [seq-utils :only (flatten indexed)]]
    [lambdebug core utils]))

;; helpers for testing trace-related stuff
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
        [1] nil]))
  (testing "try-catch"
    (check-trace-form
      '(try (+ (/ 2 0) 1) (catch Exception e -1))
      [
        [] '(try (+ (/ 2 0) 1) (catch Exception e -1))
        [1] '(+ (/ 2 0) 1)
        [1 0] '+
        [1 1] '(/ 2 0)
        [1 1 0] '/
        [1 1 1] 2
        [1 1 2] 0
        [2 3] -1])))

