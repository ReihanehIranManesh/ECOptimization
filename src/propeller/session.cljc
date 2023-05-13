; The "session" namespace is for trying things out interactively.
; For example, you can use it to test a new Push instruction by running a program that uses it and seeing the result.
; You might just want to do this interactively in the REPL, but the session file makes it a little easier since it already
; requires most of the namespaces you'll want to refer to.
; The commented-out stuff is a reminder of how to do some basic things.


(ns ^:no-doc propeller.session
  "The \"session\" namespace is for trying things out interactively.
  For example, you can use it to test a new Push instruction by running a program that uses it and seeing the result.
  You might just want to do this interactively in the REPL, but the session file makes it a little easier since it already
  requires most of the namespaces you'll want to refer to."
  (:require [propeller.genome :as genome]
            [propeller.gp :as gp]
            [propeller.selection :as selection]
            [propeller.variation :as variation]
            [propeller.push.instructions :as instructions]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]))

;; balanced parenthesis
(require '[propeller.problems.balanced-parenthesis-classification :as parenthesis])
(parenthesis/-main)
(parenthesis/-main-efficient)

;; can form word
(require '[propeller.problems.can-form-word-classification :as word])
(word/-main)
(word/-main-efficient)

;; char follows
(require '[propeller.problems.char-follows-classification :as follows])
(follows/-main)
(follows/-main-efficient)

;; subsequence
(require '[propeller.problems.common-subsequence :as subseq])
(subseq/-main)
(subseq/-main-efficient)

;; subset
(require '[propeller.problems.contains-subset-classification :as subset])
(subset/-main)
(subset/-main-efficient)

;; differ by n
(require '[propeller.problems.differ-by-n-classification :as differ])
(differ/-main)
(differ/-main-efficient)

;; is anagram
(require '[propeller.problems.is-anagram :as anagram])
(anagram/-main)
(anagram/-main-efficient)

;; palindrome
(require '[propeller.problems.palindrome-classification :as palindrome])
(palindrome/-main)
(palindrome/-main-efficient)

;; prefix
(require '[propeller.problems.prefix-classification :as prefix])
(prefix/-main)
(prefix/-main-efficient)

;; string
(require '[propeller.problems.string-classification :as string])
(string/-main)
(string/-main-efficient)

