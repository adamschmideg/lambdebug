(ns
  lambdebug.colorizer)

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
