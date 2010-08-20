(ns lambdebug.colorizer-test
  (:use
    [clojure test]
    [lambdebug.colorizer]))

(comment
(deftest substring-indexes
  (are [s loc _ start end]
    (is (= [start end] (substring-indexes-in-form s loc)))
    "a b" [0]  :=> 0 1
    "a b" [2]  :=> nil nil
    "a ( b ) c" [1]  :=> 2 6
    "a ( b ) c" [1 0] :=> 4 5))
)

(deftest tokenize-line-test
  (are [line _ result]
    (is (= result (tokenize-line line)))
    "(+ 2 3)" ["(","+"," ","2"," ","3",")"]
    " ( +  2, ,3)" [" ","("," ","+","  ","2", ", ,","3",")"]
    "42 ; answer" ["42"," ; answer"]
    "42 \"answer\" life" ["42"," ","\"answer\""," ","life"]
    "42 \"semi;colon\" life" ["42"," ","\"semi;colon\"", " ","life"]
    "42 ; \"the\" answer" ["42", " ; \"the\" answer"]))

