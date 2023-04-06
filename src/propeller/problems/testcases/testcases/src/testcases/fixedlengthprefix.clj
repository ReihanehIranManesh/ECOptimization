"This program generates a shuffled vector of random strings of different lengths, 
some with a fixed length prefix (length is defined in the program).
Created by: Reihaneh Iranmanesh"

(ns testcases.fixedlengthprefix
  (:require [clojure.string :as str])
  (:import (java.util Random)))

(def maxlength 15)

(def minlength 5)

(def vectorsize 10)

(def prefix-length 5) ; a variable for the length of the fixed prefix

; generate a random lowercase ASCII character
(defn random-char []
  (char (+ (rand-int 26) 97)))

; generate a random string with a length between minlength and maxlength
(defn random-string [minlength maxlength]
  (let [length (+ minlength (rand-int (- maxlength minlength))) chars (repeatedly length random-char)]
    (apply str chars)))

; generate a vector of vectorsize random strings
(defn random-strings-normal [vectorsize minlength maxlength]
  (vec (repeatedly vectorsize #(random-string minlength maxlength))))

; generate a random string of characters with a given prefix and a length of length
(defn prefixed-string [prefix length]
  (str prefix
       (apply str (take (- length prefix-length) (repeatedly random-char)))))

; generate a random prefix of the specified length
(defn random-prefix [length]
  (apply str (take length (repeatedly random-char))))

; generate a list of prefixed strings of random lengths, where all strings have the same fixed prefix
(defn generate-strings [vectorsize minlength maxlength]
  (let [prefix (random-prefix prefix-length)]
    (for [_ (range vectorsize)]
      (prefixed-string prefix (+ minlength (rand-int (- maxlength minlength)))))))

; generate and print a shuffled vector of randomly generated strings of varying lengths with or without a fixed prefix
(defn -main []
  (println (shuffle (vec (concat (generate-strings vectorsize minlength maxlength) (random-strings-normal vectorsize minlength maxlength))))))

(-main)
