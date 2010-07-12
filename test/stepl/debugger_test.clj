(ns
  stepl.debugger-test
  (:use
    [clojure.test]
    [stepl debugger]))

(deftest step-test
  (let [traces [{:level 0}
                {:level 1}
                {:level 2}
                {:level 2, :result :foo}
                {:level 1, :result :bar}
                {:level 1}
                {:level 1, :result :foobar}
                {:level 0, :result :finish}]]
    (testing "step-next"
      (are [idx new-idx] (is (= (step-next traces idx) new-idx))
        0 7
        1 4
        2 3))))
