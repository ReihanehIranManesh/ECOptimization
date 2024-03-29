(ns propeller.tools.efficieny-error-functions  
  "Created By Esteban Sanchez,
   set of functions to calculate error functions which include runtime"
  {:doc/format :markdown}
  (:require [propeller.genome :as genome]
            [propeller.push.interpreter :as interpreter]
            [propeller.push.state :as state]
            #?(:cljs [cljs.reader :refer [read-string]])))



;; (defn sigmoid
;;   [x]
;;   (/ 1 (+ 1 (Math/exp (- x)))))

;; (defn thresholded_amalgamate_error_function_simple
;;   "Created by: Reihaneh Iranmanesh."
;;   "A function that considers both errors and runtimes, 
;;    giving more weight to runtimes after a certain error threshold."
;;   [errors runtimes]
;;   (let [error-threshold 5
;;         runtime-weight 1
;;         error-ratio (if (= errors 0) 0 (/ errors error-threshold))
;;         weight (sigmoid error-ratio)]
;;     (+ (* (- 1 weight) errors) (* weight runtime-weight runtimes))))


;; (defn thresholded_amalgamate_error_function
;;   "Created by: Reihaneh Iranmanesh."
;;   "A complicated function that considers both errors and runtimes, 
;;    giving more weight to runtimes after a certain error threshold."
;;   [errors runtimes error-threshold runtime-weight]
;;   (let [error-ratio (if (= errors 0) 0 (/ errors error-threshold))
;;         weight (sigmoid error-ratio)]
;;     (+ (* (- 1 weight) errors) (* weight runtime-weight runtimes))))


;; (defn amalgamate_error_function_updated
;;   "Created by: Reihaneh Iranmanesh."
;;   "A function that considers both errors and runtimes, 
;;    but reduces the penalty for longer runtimes over generations."
;;   [errors runtimes generation max-generation]
;;   (let [error-threshold 5
;;         scaling-factor (if (= max-generation 0) 1 (/ generation max-generation))
;;         error-ratio (if (= errors 0) 0 (/ errors error-threshold))
;;         weight (sigmoid error-ratio)]
;;     (+ (* (- 1 weight) errors) (* weight scaling-factor runtimes))))


;; (defn adaptive_lambda [errors]
;;   (if (< errors 3)
;;     0.1
;;     1.0))

;; (defn amalgamate_error_adaptive_lambda
;;   "Created by: Reihaneh Iranmanesh."
;;   [errors runtimes]
;;   (let [adjusted_lambda (adaptive_lambda errors)]
;;     (+ errors (* adjusted_lambda runtimes))))


(defn amalgamate_error_function
  "Created by: Esteban Sanchez.
   naturally weighted error function, errors are 0 or 1, runtimes are scaled between 0 1"
  [errors runtimes]
  (+ errors runtimes))


(defn error-function1
  "Finds the behaviors and errors of an individual: Error is 0 if the value and
  the program's selected behavior match, or 1 if they differ, or 1000000 if no
  behavior is produced. The behavior is here defined as the final top item on
  the BOOLEAN stack. appends amalgamated error to individual along with runtime information
  TAKES 1 INPUT. "
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
  the BOOLEAN stack. appends amalgamated error to individual along with runtime information
   TAKES 2 INPUT"
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




