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

(deftest tokenize-test
  (testing "Tokenize line"
    (are [line _ result]
      (is (= result (tokenize-line line false)))
      "(+ 2 3)"  :=>  ["(","+"," ","2"," ","3",")"]
      " ( +  2, ,3)"  :=>  [" ","("," ","+","  ","2", ", ,","3",")"]
      "\"only string\""  :=>  ["\"only string\""]
      ";; only comment"  :=>  [";; only comment"]
      "42 ; answer"  :=>  ["42"," ; answer"]
      "42 \"answer\" life"  :=>  ["42"," ","\"answer\""," ","life"]
      "42 \"semi;colon\" life"  :=>  ["42"," ","\"semi;colon\"", " ","life"]
      "42 ; \"the\" answer"  :=>  ["42", " ; \"the\" answer"]))
  (testing "Multi-line strings"
    (is (=
      (tokenize-line "before \"start string" false)
      ["before", " ", "\"start string", :cont]))
    (is (=
      (tokenize-line "middle of string" true)
      ["middle of string", :cont]))
    (is (=
      (tokenize-line "end string\" after" true)
      ["end string\"", " ", "after"])))
  (testing "Tokenize text"
    (is (=
      (tokenize "a\nb\n\nc")
      ["a", "\n", "b", "\n", "\n", "c"]))
    (is (=
      (tokenize "before \"begin\ncontinue\nend\" after")
      ["before", " ", "\"begin\ncontinue\nend\"", " ", "after"]))))
