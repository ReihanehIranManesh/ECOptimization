(ns propeller.push.interpreter
  "Interprets Push programs.
   Edited by Regina Deri: interpret-program; added optional efficiency measurement functionality"
  (:require [propeller.push.instructions :as instructions]
            [propeller.push.state :as state]
            [propeller.push.instructions.input-output :as io]
            [propeller.tools.efficiency :as efficiency]))

(defn interpret-one-step
  "Takes a Push state and executes the next instruction on the exec stack."
  [state]
  (let [popped-state (state/pop-stack state :exec)
        instruction (first (:exec state))
        literal-type (instructions/get-literal-type instruction)]     ; nil for non-literals
    (cond
      ;;
      ;; Recognize functional instruction or input instruction
      (keyword? instruction)
      (if-let [function (instruction @instructions/instruction-table)]
        (function popped-state)
        (io/handle-input-instruction popped-state instruction))
      ;;
      ;; Recognize constant literal instruction
      literal-type
      (if (= :generic-vector literal-type)
        ;; Empty vector gets pushed on all vector stacks
        (reduce #(update-in % [%2] conj []) popped-state
                [:vector_boolean :vector_float :vector_integer :vector_string])
        (state/push-to-stack popped-state literal-type instruction))
      ;;
      ;; Recognize parenthesized group of instructions
      (seq? instruction)
      (update popped-state :exec #(concat %2 %1) instruction)
      ;;
      :else
      (throw #?(:clj  (Exception. (str "Unrecognized Push instruction in program: "
                                       (name instruction)))
                :cljs (js/Error. (str "Unrecognized Push instruction in program: "
                                      (name instruction))))))))

(defn interpret-program
  "Runs the given problem starting with the stacks in start-state. If the
  start-state includes the key :keep-history with a truthy value, then
  the returned state will include the key :history with a value that is a
  vector containing all states prior to the final state.  If the
  start-state includes the key :benchmark with a truthy value, then
  the returned state will include the key :efficiency containing a hashmap
  with the runtime of program in ms and the memory allocated by the program
  in bytes under the :runtime and :memory keys respectively."
  [program start-state step-limit]
  (loop [state (assoc start-state :exec (list program) :step 1)
         history []]
    (if (or (empty? (:exec state))
            (> (:step state) step-limit))
      (let [temp_state (if (:keep-history state)
                         (assoc state :history history)
                         state)]
        (if (:benchmark state) 
          (assoc temp_state :efficiency (let
                                               [bench-start-state
                                                (dissoc start-state
                                                        :keep-history :benchmark)]; run the program without any params
                                                {:runtime (efficiency/average-runtimes
                                                                 #(interpret-program program bench-start-state step-limit))
                                                 :memory (efficiency/allocated-bytes ;;memory used = bytes allocated
                                                          #(interpret-program program bench-start-state step-limit))}))
          temp_state))

      (recur (update (interpret-one-step state) :step inc)
             (when (:keep-history state) (conj history state))))))
