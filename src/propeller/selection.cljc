(ns propeller.selection
  "Propeller includes many kinds of genetic operators to select parents within the population such as tournament selection,
  lexicase selection, and epsilon lexicase selection."
  {:doc/format :markdown}
  (:require [propeller.tools.math :as math-tools]))

(defn tournament-selection
  "Selects an individual from the population using tournaments of
  tournament-size by taking the individual in the tournament with the lowest :total-error. "
  [pop argmap]
  (let [tournament-size (:tournament-size argmap)
        tournament-set (take tournament-size (shuffle pop))]
    (apply min-key :total-error tournament-set)))

(defn tournament-efficiency
  "Selects an individual from the population using tournaments of
  tournament-size by taking the individual in the tournament with the lowest :total-error. "
  [pop argmap]
  (let [tournament-size (:tournament-size argmap)
        tournament-set (take tournament-size (shuffle pop))]
    (apply min-key :total-amalgamated-error tournament-set)))

(defn lexicase-selection
  "Selects an individual from the population using lexicase selection.
  Lexicase parent selection filters the population by considering one random training case at a time,
  eliminating any individuals with errors for the current case that are worse than the best error in the selection pool,
  until a single individual remains."
  [pop argmap]
  (loop [survivors (map rand-nth (vals (group-by :errors pop)))
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))
        ))))

(defn lexicase-selection2
  "Modifying lexicase selection so that there is a specified chance  (specified in the if statement) you take the fastest individuals, otherwise it does regular case-based lexicase selection"
  [pop argmap]
  (loop [survivors (map rand-nth (vals (group-by :errors pop)))
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))]
        (if (<= (rand) 0.5)
        (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))
          (recur (filter #(= (:total-runtime %) (apply min (map :total-runtime survivors))) survivors)
                 (rest cases))
          )
        ))))






(defn lexicase-selection-parallel
  "Lexicase modification takes both the fastest of the lowest error functions on each test case. This is valuing efficiency and correctness in parallel for selection"
  [pop argmap]
  (loop [survivors (map rand-nth (vals (group-by :errors pop)))
         cases (shuffle (range (count (:errors (first pop)))))]

    (if (or (empty? cases)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :errors survivors)))
            min-runtime-for-case (apply min (map #(nth % (first cases))
                                                 (map :runtimes survivors)))]
        (recur (filter #(or (= (nth (:errors %) (first cases)) min-err-for-case)
                             (= (nth (:runtimes %) (first cases)) min-runtime-for-case))
                       survivors)
               (rest cases))))))


(defn epsilon-list
  "List of epsilons for each training case based on median absolute deviation of errors."
  [pop]
  (let [error-list (map :errors pop)
        length (count (:errors (first pop)))]
    (loop [epsilons [] i 0]
      (if (= i length)
        epsilons
        (recur (conj epsilons
                     (math-tools/median-absolute-deviation
                       (map #(nth % i) error-list)))
               (inc i))))))

(defn epsilon-lexicase-selection
  "Selects an individual from the population using epsilon-lexicase selection.
  Epsilon lexicase selection follows the same process as lexicase selection except,
  for a test case, only individuals with an error outside of a predefined epsilon are filtered."
  [pop argmap]
  (let [epsilons (:epsilons argmap)]
    (loop [survivors pop
           cases (shuffle (range (count (:errors (first pop)))))]
      (if (or (empty? cases)
              (empty? (rest survivors)))
        (rand-nth survivors)
        (let [min-err-for-case (apply min (map #(nth % (first cases))
                                               (map :errors survivors)))
              epsilon (nth epsilons (first cases))]
          (recur (filter #(<= (Math/abs (- (nth (:errors %)
                                                (first cases))
                                           min-err-for-case))
                              epsilon)
                         survivors)
                 (rest cases)))))))

(defn epsilon-lexicase-selection-parallel
  "Selects an individual from the population using epsilon-lexicase selection.
  Epsilon lexicase selection follows the same process as lexicase selection except,
  for a test case, only individuals with an error outside of a predefined epsilon are filtered.
   Modification: for each test case, we also take the program with the lowest runtime on that test case."
  [pop argmap]
  (let [epsilons (:epsilons argmap)]
    (loop [survivors pop
           cases (shuffle (range (count (:errors (first pop)))))]
      (if (or (empty? cases)
              (empty? (rest survivors)))
        (rand-nth survivors)
        (let [min-err-for-case (apply min (map #(nth % (first cases))
                                               (map :errors survivors)))
              epsilon (nth epsilons (first cases))
              min-runtime-for-case (apply min (map #(nth % (first cases))
                                                   (map :runtimes survivors)))]
          (recur (filter #(or (<= (Math/abs (- (nth (:errors %)
                                                    (first cases))
                                               min-err-for-case))
                                  epsilon)
                         (= (nth (:runtimes %) (first cases)) min-runtime-for-case))
                         survivors)
                 (rest cases)))))))




(defn select-parent
  "Selects a parent from the population using the specified method."
  [pop argmap]
  (case (:parent-selection argmap)
    :tournament-efficiency (tournament-efficiency pop argmap)
    :tournament (tournament-selection pop argmap)
    :lexicase (lexicase-selection pop argmap)
    :epsilon-lexicase (epsilon-lexicase-selection pop argmap)
    :lexicase2 (lexicase-selection2 pop argmap)
    :lexicase-parallel (lexicase-selection-parallel pop argmap)
    :epsilon-lexicase-parallel (epsilon-lexicase-selection-parallel pop argmap)
    ))
