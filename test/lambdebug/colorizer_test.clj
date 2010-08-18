(ns lambdebug.colorizer-test
  (:use
    [clojure test]
    [lambdebug.colorizer]))

(deftest substring-indexes
  (are [s loc _ start end]
    (is (= [start end] (substring-indexes-in-form s loc)))
    "a b" [0]  :=> 0 1
    "a b" [2]  :=> nil nil
    "a ( b ) c" [1]  :=> 2 6
    "a ( b ) c" [1 0] :=> 4 5))
