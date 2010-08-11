(ns
  lambdebug.debugger-test
  (:use
    [clojure.test]
    [lambdebug debugger]))

(deftest step-test
  (let [traces [{:id 0, :level 0}
                {:id 1, :level 1}
                {:id 2, :level 2}
                {:id 3, :level 2, :result :foo}
                {:id 4, :level 1, :result :bar}
                {:id 5, :level 1}
                {:id 6, :level 1, :result :foobar}
                {:id 7, :level 0, :result :finish}]]
    (testing "step-next"
      (are [idx new-idx] (is (= (step-next traces idx) new-idx))
        0 7
        1 4
        4 5
        2 3))
    (testing "step-out"
      (are [idx new-idx] (is (= (step-out traces idx) new-idx))
        0 8
        1 7
        4 7
        2 4))
    (testing "step-prev"
      (are [idx new-idx] (is (= (step-prev traces idx) new-idx))
        7 0
        4 1
        5 4
        3 2))))
