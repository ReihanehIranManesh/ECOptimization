(ns propeller.problems.differ-by-n-classification
"Title: problems.differ-by-n-classification:
Created By: Esteban Sanchez
Description: Given a vector of 2 strings, return true if they differ byn more than n characters"
  {:doc/format :markdown}
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            [propeller.gp :as gp]
            [propeller.tools.efficieny-error-functions :as error]
            #?(:cljs [cljs.reader :refer [read-string]])))



;; Set of original propel instructions
(def instructions
  "Set of original propel instructions"
  (list :in1
        :in2
        :integer_add
        :integer_subtract
        :integer_mult
        :integer_quot
        :integer_gte
        :integer_lte
        :integer_eq
        :exec_dup
        :exec_if
        :boolean_and
        :boolean_or
        :boolean_not
        :boolean_eq
        :string_eq
        :string_take
        :string_drop
        :string_reverse
        :string_concat
        :string_length
        :string_contains
        'close
        0
        1
        2
        true
        false
        ""
        "abcdefghijklmnopqrstuvwxyz"
        ))

(def train-and-test-data

  (let [train-inputs [["racecar" "bacecar"] ["rod" "rot"] ["aaaab", "bcdab"] ["ragtime", "raptime"] ["bolster", "rapture"] 
                      ["bat", "bod"] ["cato", "code"]]
        test-inputs [["aaba", "aaab"] ["randon", "random"] ["coat", "kite"] ["card", "late"] [ "card", "lard"] 
                     ["babb", "abbc"] ["caliber, " "baliber"]]
        train-outputs [true true false true false true false]
        test-outputs [true true false false true false true]]
    {:train (map (fn [in out] {:input1 (vector in) :output1 (vector out)}) train-inputs train-outputs)
     :test (map (fn [in out] {:input1 (vector in) :output1 (vector  out)}) test-inputs test-outputs)}))

(defn error-function
  "Finds the behaviors and errors of an individual: Error is 0 if the value and
  the program's selected behavior match, or 1 if they differ, or 1000000 if no
  behavior is produced. The behavior is here defined as the final top item on
  the BOOLEAN stack."
  [argmap data individual]
  (let [program (genome/plushy->push (:plushy individual) argmap)
        inputs (map (fn [x] (first (:input1 x))) data)
        correct-outputs (map (fn [x] (first (:output1 x))) data)
        outputs (map (fn [input]
                       (state/peek-stack
                        (interpreter/interpret-program
                         program
                         (assoc state/empty-state :input {:in1 (first input) :in2 (second input)})
                         (:step-limit argmap))
                        :boolean))
                     inputs)
        errors (map (fn [correct-output output]
                      (if (= output :no-stack-item)
                        1000000
                        (if (= correct-output output)
                          0
                          1)))
                    correct-outputs
                    outputs)]
    (assoc individual
           :behaviors outputs
           :errors errors
           :total-error #?(:clj  (apply +' errors)
                           :cljs (apply + errors)))))

(defn -main
  "Runs the top-level genetic programming function, giving it a map of 
  arguments with defaults that can be overridden from the command line
  or through a passed map."
  [& args]
  (gp/gp-efficiency
   (merge
    {:instructions            instructions
     :error-function          error/error-function2
     :training-data           (:train train-and-test-data)
     :testing-data            (:test train-and-test-data)
     :max-generations         500
     :population-size         500
     :max-initial-plushy-size 100
     :step-limit              200
     :parent-selection        :tournament-efficiency
     :tournament-size         5
     :umad-rate               0.1
     :variation               {:umad 0.5 :crossover 0.5}
     :elitism                 false
     :simplification? true
     :simplification-k 4
     :simplification-steps 1000
     :simplification-verbose? true}
    (apply hash-map (map #(if (string? %) (read-string %) %) args)))))