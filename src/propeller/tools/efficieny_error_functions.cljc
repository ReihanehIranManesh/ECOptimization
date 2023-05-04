(ns propeller.tools.efficieny-error-functions  
  {:doc/format :markdown}
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            #?(:cljs [cljs.reader :refer [read-string]])))



(defn amalgamate_error_function
  "Created by: Esteban Sanchez.
   adjusts weights of correctness error function and efficiency error function based off total number of correct cases."
  [errors runtimes]
  (+ (* errors (- 7 (* (/ 1 5) (- 7 errors))))
     (* (* (/ 1 5) (- 7 errors)) runtimes)))



(defn error-function1
  "Finds the behaviors and errors of an individual: Error is 0 if the value and
  the program's selected behavior match, or 1 if they differ, or 1000000 if no
  behavior is produced. The behavior is here defined as the final top item on
  the BOOLEAN stack. TAKES 1 INPUT"
  [argmap data individual]
  (let [program (genome/plushy->push (:plushy individual) argmap)
        inputs (map (fn [x] (first (:input1 x))) data)
        correct-outputs (map (fn [x] (first (:output1 x))) data)
        final-states (map (fn [input]
                            (interpreter/interpret-program
                             program
                             (assoc state/empty-state :input {:in1 input} :benchmark true)
                             (:step-limit argmap)))
                          inputs)
        outputs (map (fn [state]
                       (state/peek-stack
                        state
                        :boolean))
                     final-states)
        runtimes (map (fn [state]
                        (state/get-runtime
                         state))
                      final-states)
        errors (map (fn [correct-output output]
                      ;; (println output)
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
           :runtimes runtimes
           :total-amalgamated-error (amalgamate_error_function #?(:clj  (apply +' errors)
                                                                  :cljs (apply + errors))
                                                               #?(:clj  (apply +' runtimes)
                                                                  :cljs (apply + runtimes)))
           :total-runtime #?(:clj  (apply +' runtimes)
                              :cljs (apply + runtimes))
           :total-error #?(:clj  (apply +' errors)
                           :cljs (apply + errors)))))

(defn error-function2
  "Finds the behaviors and errors of an individual: Error is 0 if the value and
  the program's selected behavior match, or 1 if they differ, or 1000000 if no
  behavior is produced. The behavior is here defined as the final top item on
  the BOOLEAN stack. TAKES 2 INPUT"
  [argmap data individual]
  (let [program (genome/plushy->push (:plushy individual) argmap)
        inputs (map (fn [x] (first (:input1 x))) data)
        correct-outputs (map (fn [x] (first (:output1 x))) data)
        final-states (map (fn [input]
                            (interpreter/interpret-program
                             program
                             (assoc state/empty-state :input {:in1 input :in2 (second input)} :benchmark true)
                             (:step-limit argmap)))
                          inputs)
        outputs (map (fn [state]
                       (state/peek-stack
                        state
                        :boolean))
                     final-states)
        runtimes (map (fn [state]
                        (state/get-runtime
                         state))
                      final-states)
        errors (map (fn [correct-output output]
                      ;; (println output)
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
           :runtimes runtimes
           :total-amalgamated-error (amalgamate_error_function #?(:clj  (apply +' errors)
                                                                  :cljs (apply + errors))
                                                               #?(:clj  (apply +' runtimes)
                                                                  :cljs (apply + runtimes)))
           :total-runtime #?(:clj  (apply +' runtimes)
                             :cljs (apply + runtimes))
           :total-error #?(:clj  (apply +' errors)
                           :cljs (apply + errors)))))




