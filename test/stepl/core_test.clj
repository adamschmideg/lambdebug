(ns
  stepl.core-test
  (:use
    [clojure.test]
    [clojure.contrib
      [seq-utils :only (flatten)]]
    [stepl]))

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

(defn test-trace
  "Check if tracing form results in the simplified-traces"
  [form simplified-traces]
  (do
     (send *traces* (constantly []))
     (eval (trace-form form))
     (await-for 1000 *traces*)
     (is (= (simplify-traces @*traces*)
            simplified-traces))))

(defn div [x y] (/ x y))

