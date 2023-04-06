"This program creates a shuffled vector consisting of randomly generated strings 
containing balanced and unbalanced parentheses of different lengths.
Created by: Reihaneh Iranmanesh"

(ns testcases.balanced
  (:require [clojure.string :as str]))

(def maxlength 15)

(def minlength 5)

(def vectorsize 10)

; return a random parenthesis character "(" or ")"
(defn random-paren []
  (rand-nth ["(" ")"]))

; generate a string of random parentheses with a specified length
(defn random-parens-string [length]
  (apply str (repeatedly length random-paren)))

; generate a random string of parentheses with a length between minlength and maxlength
(defn random-parens []
  (let [length (inc (+ minlength (rand-int maxlength)))]
    (random-parens-string length)))

; generate a vector of vectorsize random strings of parentheses
(defn generate-random-parens []
  (vec (repeatedly vectorsize random-parens)))

; generate a random string of balanced parentheses with a length of n
(defn generate-balanced-parens [n]
  (if (<= n 0)
    ""
    (let [left-size (rand-int (inc n))
          right-size (- n left-size)]
      (str "("
           (generate-balanced-parens (dec left-size))
           ")"
           (generate-balanced-parens right-size)))))

; generate a random string of balanced parentheses with a length between minlength and maxlength
(defn random-balanced-parens []
  (let [length (inc (+ minlength (rand-int maxlength)))]
    (generate-balanced-parens length)))

; generate a vector of vectorsize random strings of balanced parentheses
(defn generate-more-balanced-parens []
  (vec (repeatedly vectorsize random-balanced-parens)))

; generate and print a shuffled vector of randomly generated strings of balanced/unbalanced parentheses
(defn -main []
  (println (shuffle (vec (concat (generate-random-parens) (generate-more-balanced-parens))))))

(-main)





