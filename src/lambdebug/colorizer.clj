(ns
  lambdebug.colorizer
  (:require
    [clojure
      [string :as s]]
    [clojure.contrib
      [string :as string]])
  (:use
    [lambdebug utils]))

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

(defn tokenize-simple-line
  "Split a line to tokens, assuming it doesn't contain
  strings and comments"
  [line]
    (remove #(= "" %)
      (string/partition #"#\{|[\(\)\{\}\[\]]|[, ]+" line)))
    
(defn finish-string
  "Return the part until a quote and the rest"
  [s]
    (s/split s #"(?<!\\)\"" 2))

(defn tokenize-line
  "Split a line to tokens, where a token is a string of
   - whitespaces (including comma and comments)
   - a single opening or closing paren
   - a string (which may contain any of the above)
   - other atom as string.
  If a string is started but not finished on a line,
  the last token is :cont.  If cont? is true, the line
  is considered to continue a string from the previous line."
  [line cont?]
    (if cont?
      (let [[string rest] (finish-string line)]
        (if (= 0 (count rest))
          [string :cont]
          (concat [(str string "\"")] (tokenize-line rest false))))
      (if-let [string-start-or-comment
               (re-find #"(.*?)((\"|[ ,]*;)(.*))" line)]
        (let [[_ before sep+after sep after] string-start-or-comment
              start (tokenize-simple-line before)]
          (if (= "\"" sep)
            (let [[string rest] (finish-string after)]
              (if rest
                (concat start [(str "\"" string "\"")] (tokenize-line rest false))
                (concat start [sep+after :cont])))
            (concat start [sep+after])))
        (tokenize-simple-line line))))
