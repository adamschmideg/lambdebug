(ns
  lambdebug.colorizer
  (:require
    [clojure
      [string :as s]]))

(defn substring-indexes-in-form
  "Interpret s as a form and return 
   the start and end index of the substring that would be
   found at loc in that form. It takes into account clojure nuances,
   such as whitespaces, comments, etc."
   [s loc]
   (loop [cur (first s)
          xs (next s)
          state nil
          idx 0
          level 0
          cnt 0]))

(defn tokenize-line
  "Split a line to tokens, where a token is a string of
   - whitespaces (including comma and comments)
   - a single opening or closing paren
   - a string (which may contain any of the above)
   - other atom as string"
  [line])
