(ns
  lambdebug.colorizer
  (:require
    [clojure
      [string :as s]]
    [clojure.contrib
      [string :as string]])
  (:use
    [lambdebug utils]))

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

(defn tokenize
  "Tokenize a string line by line, then concatenate tokens,
  merging multiline strings"
  [string]
  (next
    (let [lines (s/split-lines string)]
      (loop [line (first lines)
             rest (next lines)
             cont? false
             result []]
        (let [tokens (tokenize-line line cont?)
              result
                (if (= :cont (last result))
                  (let [[good [start-string _]]
                        (split-at (- (count result) 2) result)]
                    (concat
                      good
                      [(str start-string "\n" (first tokens))]
                      (next tokens)))
                  (concat result ["\n"] tokens))]
          (if rest
            (recur
              (first rest)
              (next rest)
              (= :cont (last tokens))
              result)
            result))))))

(defn open-block? [s] (#{"[", "(", "{", "#{"} s))
(defn close-block? [s] (#{"]", ")", "}"} s))
(defn whitespace? [s] (#{\space \, \; \newline} (first s)))

(defn split-at-block
  "Return tokens before nth block and after nth block"
  [n tokens]
  (loop [before []
         rest tokens
         level 0
         count 0]
    (let [token (first rest)]
      (cond
        (and (= count n) (= level 0) (not (whitespace? token)))
          [before rest]
        (seq rest)
          (let [level (cond
                        (open-block? token) (inc level)
                        (close-block? token) (dec level)
                        :default level)
                count (if (and (= level 0) (not (whitespace? token)))
                        (inc count)
                        count)]
            (recur (conj before token) (next rest) level count))
        :else
          [before []]))))

(defn token-rtrim
  "Return [good-tokens whitespaces-after]"
  [tokens]
    (let [[rev-ws rev-good] (split-with whitespace? (reverse tokens))]
      [(reverse rev-good) (reverse rev-ws)]))

(defn token-trim
  "Trim whitespaces and outermost parens from both ends"
  [tokens]
    (let [[left rest] (split-with whitespace? tokens)
          [good right] (token-rtrim rest)]
      [left good right]))

(defn get-in-block
  "Return [before found after] where found is the part denoted by path"
  [tokens path]
  (let [
    [before middle after]
      (loop [path path
             before []
             found tokens
             after []]
        (let [n (first path)]
          (if n
            (let [token (first found)]
              (cond
                (whitespace? token)
                  (recur path (conj before token) (next found) after)
                (open-block? token)
                  (let [[till-open-block focus from-close-block] (token-trim found)
                        [skip start-found] (split-at-block n (next (butlast focus)))
                        [found after-found] (split-at-block 1 start-found)
                        before (concat before till-open-block (first focus) skip)
                        after (concat after-found (last focus) from-close-block after)]
                    (recur (next path) before found after))
                :else
                  [before found after]))
              [before found after])))
      [found ws] (token-rtrim middle)]
      [before found (concat ws after)]))

(defn insert-subseqs
  "Insert each items at nth position into coll,
  indexed-new-items is ((n items) ...)"
  [coll indexed-new-items]
    (loop [coll coll
           indexed-items (sort (complement compare) indexed-new-items)]
      (?? coll indexed-items)
      (let [[[n item] & more] indexed-items]
        (if n
          (let [[before after] (split-at n coll)]
            (recur (concat before item after) more))
          coll))))
