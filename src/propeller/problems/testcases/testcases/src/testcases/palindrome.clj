"This program creates a shuffled vector consisting of randomly generated strings 
of different lengths that may or may not be palindrome.
Created by: Reihaneh Iranmanesh"

(ns testcases.palindrome
  (:require [clojure.string :as str])
  (:import (java.util Random)))

(def maxlength 15)

(def minlength 5)

(def vectorsize 10)

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

; generate a random palindrome string with a length between minlength and maxlength
; first generate a random half-length, and concatenate it with its reverse to create a palindrome
(defn random-palindrome-string [minlength maxlength]
  (let [half-length (-> (+ minlength (rand-int (- maxlength minlength))) (/ 2) Math/ceil int)
        half-chars (repeatedly half-length random-char)
        palindrome-chars (concat half-chars (reverse (if (even? (* 2 half-length)) half-chars (butlast half-chars))))]
    (apply str palindrome-chars)))

; generate a vector of vectorsize random palindrome strings
(defn random-strings [vectorsize minlength maxlength generate-string-fn]
  (vec (repeatedly vectorsize #(generate-string-fn minlength maxlength))))

; generate and print a shuffled vector of randomly generated palindrome and non-palindrome strings of varying lengths
(defn -main []
  (println (shuffle (vec (concat (random-strings vectorsize minlength maxlength random-palindrome-string) (random-strings-normal vectorsize minlength maxlength))))))

(-main)
