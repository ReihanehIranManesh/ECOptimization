(ns propeller.tools.efficiency-statistics
  (:require [propeller.gp ]
            [clojure.string]
            [clojure.pprint]
            [propeller.genome :as genome]
            [propeller.simplification :as simplification]
            [propeller.variation :as variation]
            [propeller.push.instructions.bool]
            [propeller.push.instructions.character]
            [propeller.push.instructions.code]
            [propeller.push.instructions.input-output]
            [propeller.push.instructions.numeric]
            [propeller.push.instructions.polymorphic]
            [propeller.push.instructions.string]
            [propeller.push.instructions.vector]
            [propeller.selection :as selection]))

(defn get-best-runtimes [pop]
  (let [best (first pop)]
    (:runtimes best)))

(defn get-runtime-data [pop generation argmap]
  (let [best (first pop)]
    {:generation generation
     :best-program (genome/plushy->push (:plushy best) argmap)
     :best-runtime (:runtimes best)}))

(defn runtime-stats-report [pop generation argmap]
  (println "Saving generation " generation "..." ))

(defn gp-runtime-stats

  [{:keys [population-size max-generations error-function instructions
           max-initial-plushy-size solution-error-threshold mapper out-file]
    :or   {solution-error-threshold 0.0
           ;; The `mapper` will perform a `map`-like operation to apply a function to every individual
           ;; in the population. The default is `map` but other options include `mapv`, or `pmap`.
           mapper #?(:clj pmap :cljs map)}
    :as   argmap}]
  ;;
  (prn {:starting-args (update (update argmap :error-function str) :instructions str)})
  (println)
  ;;
  (loop [generation 0
         population (mapper
                     (fn [_] {:plushy (genome/make-random-plushy instructions max-initial-plushy-size)})
                     (range population-size))]             ;creates population of random plushys
    (let [evaluated-pop (sort-by :total-amalgamated-error
                                 (mapper
                                  (partial error-function argmap (:training-data argmap))
                                  population))             ;population sorted by :total-error
          best-individual (first evaluated-pop)
          argmap (if (= (:parent-selection argmap) :epsilon-lexicase)
                   (assoc argmap :epsilons (selection/epsilon-list evaluated-pop))
                   argmap)]                                 ;adds :epsilons if using epsilon-lexicase
      (if (:custom-report argmap)
        ((:custom-report argmap) evaluated-pop generation argmap)
        (runtime-stats-report evaluated-pop generation argmap))
      (let
       [runtime-data (get-runtime-data evaluated-pop generation argmap)]
        (spit (:out-file argmap) (str (pr-str runtime-data) (newline)) :append true))
      (cond
        ;; Success on training cases is verified on testing cases
        (and (<= (:total-error best-individual) solution-error-threshold) (>= generation 30))
        (do (prn {:success-generation generation})
            (prn {:total-test-error
                  (:total-error (error-function argmap (:testing-data argmap) best-individual))})
            (when (:simplification? argmap)
              (let [simplified-plushy (simplification/auto-simplify-plushy (:plushy best-individual) error-function argmap)]
                (prn {:total-test-error-simplified (:total-error (error-function argmap (:testing-data argmap) (hash-map :plushy simplified-plushy)))}))))
        ;;
        (>= generation max-generations)
        nil
        ;;
        :else (recur (inc generation)
                     (if (:elitism argmap)
                       (conj (repeatedly (dec population-size)
                                         #(variation/new-individual evaluated-pop argmap))
                             (first evaluated-pop))         ;elitism maintains the most-fit individual
                       (repeatedly population-size
                                   #(variation/new-individual evaluated-pop argmap))))))))