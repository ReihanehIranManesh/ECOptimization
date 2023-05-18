(ns gp-analytics.statistics)

;;#################################################
;;Basic numerical and statistical helping functions
;;#################################################

(defn mean
  "Returns the mean of a number sequence"
  [seq]
  (double (/ (reduce + seq)
             (count seq))))

(defn square [n]
  (* n n))

(defn std [seq]
  (let [average (mean seq)]
    (Math/sqrt (mean (map #(square (abs (- average %))) seq)))))

(defn round-double
  "Rounds a double down to the inputed digits"
  [a digits]
  (Double/parseDouble (format (str "%." digits "f") a)))


(defn change-ratio
  "Returns the ratio of change between two numbers"
  [a b]
  (round-double (/ b a) 2))

(defn change-percentage-str
  "Returns a string from a change  ratio representing a percent value"
  [a]
  (str (- (* a 100) 100) "%"))


(defn max-indexed
  "Input: a sequence of values
   Ouput: a tuple of (idx, val) where val is the maximum element,
          and idx is its index inside the sequence."
  [seq]
  (apply max-key second (map-indexed list seq)))

(defn min-indexed
  "Input: a sequence of values
   Ouput: a tuple of (idx, val) where val is the minimum element,
          and idx is its index inside the sequence."
  [seq]
  (apply min-key second (map-indexed list seq)))

;;################################################
;;Statistical functions for analyzing a single run
;;################################################


(defn generations-count
  "Gets the number of generations from a recorded run."
  [gpstat-entries]
  ;Have to see if there is success data at the end (+3 lines) or no 
  (inc (or (get (last gpstat-entries) :generation nil)
           (get (nth gpstat-entries (- (count gpstat-entries) 3)) :generation nil))))


(defn entry-runtimes
  "Input: map representing a gpstat entry
   Output: the 5 best runtimes"
  [gpstat-entry] (:runtime gpstat-entry))

(defn get-first-best-ratio [gpstat-entries]
  "Finds the ratio of the first entry's runtime and the best runtime of all the entries."
  (let [generations (let [indexed-entries (map-indexed list gpstat-entries)
                          filtered-generations (filter #(= 0 (:total-error %))
                                                       indexed-entries)]
                      (if (empty? filtered-generations)
                        indexed-entries
                        filtered-generations))
        first-runtime (reduce + (:runtime (first gpstat-entries)))
        best-entry (apply min-key #(let [runtimes (:runtime (second %))]
                                     (reduce + runtimes))
                          generations)
        best-generation (:generation (second best-entry))
        best-runtime (reduce + (:runtime (second best-entry)))
        best-total-error (:total-error (second best-entry))]
    {:first-runtime first-runtime
     :best-generation best-generation
     :best-runtime best-runtime
     :best-total-error best-total-error
     :ratio (change-ratio first-runtime best-runtime)}))

(defn get-improvement-ratio [gpstat-entries]
  "Returns the ratio of the runtime of the first and last entry."
  (let [total-runtimes (map #(reduce + %) (map #(:runtime %) gpstat-entries))
        first-runtime (first total-runtimes)
        last-runtime (last total-runtimes)
        last-total-error (:total-error (last gpstat-entries))]
    {:first-runtime first-runtime
     :last-runtime last-runtime
     :last-total-error last-total-error
     :ratio (change-ratio first-runtime last-runtime)}))


;;########################################################################
;;Statistical functions for analyzing all runs of a problem/selection pair
;;########################################################################

(defn average-total-runtimes
  "Collects the average of total-runtime for each generation over all runs of a problem."
  [hashmaps]
  (let [max-index (- (count (:total-runtimes (first hashmaps))) 1)  ;the last index of :average-runtimes
        total-runtimes-indexed (loop [index 0
                                      total-runtime-indexed []]
                                 (if (> index max-index)
                                   total-runtime-indexed
                                   (recur
                                    (inc index)
                                    (conj total-runtime-indexed (map #(nth (:total-runtimes %) index) hashmaps)))))]
    (map-indexed
     (fn [index item]
       {:generation index
        :value (mean item)
        :std (std item)})
     total-runtimes-indexed)))

(defn average-total-errors
  "Collects the average of total-error for each generation over all runs of a problem."
  [hashmaps]
  (let [max-index (- (count (:best-total-errors (first hashmaps))) 1)  ;the last index of :total-errors
        total-errors-indexed (loop [index 0
                                    curr-errors-indexed []]
                               (if (> index max-index)
                                 curr-errors-indexed
                                 (recur
                                  (inc index)
                                  (conj curr-errors-indexed (map #(nth (:best-total-errors %) index) hashmaps)))))]
    (map-indexed
     (fn [index item]
       {:generation index
        :value (mean item)
        :std (std item)})
     total-errors-indexed)))

(defn average-amalgamated-errors
  "Collects the average of amalgamated-error for each generation over all runs of a problem."
  [hashmaps]
  (let [max-index (- (count (:best-amalgamated-errors (first hashmaps))) 1)  ;the last index of :total-errors
        total-errors-indexed (loop [index 0
                                    curr-errors-indexed []]
                               (if (> index max-index)
                                 curr-errors-indexed
                                 (recur
                                  (inc index)
                                  (conj curr-errors-indexed (map #(nth (:best-amalgamated-errors %) index) hashmaps)))))]
    (map-indexed
     (fn [index item]
       {:generation index
        :value (mean item)
        :std (std item)})
     total-errors-indexed)))

(defn average-improvement-ratio
  "Collects the average of improvement ratio for each generation over all runs of a problem."
  [hashmaps]
  (let [improvement-ratios (map #(:ratio (:improvement-ratio %)) hashmaps)]
    {:value (mean improvement-ratios)
     :std (std improvement-ratios)}))

(defn average-first-best-ratio
  "Collects the average of first-best ratio for each generation over all runs of a problem."
  [hashmaps]
  (let [first-best-ratios (map #(:ratio (:first-best-ratio %)) hashmaps)]
    {:value (mean first-best-ratios)
     :std (std first-best-ratios)}))



(defn min-total-runtimes
  "Collects the minimum total runtime for each generation over all runs of a problem."
  [hashmaps]
  (let [max-index (- (count (:total-runtimes (first hashmaps))) 1)  ;the last index of :total-runtimes
        min-total-runtimes-indexed (loop [index 0
                                          total-runtime-indexed []]
                                     (if (> index max-index)
                                       total-runtime-indexed
                                       (recur
                                        (inc index)
                                        (conj total-runtime-indexed (map #(nth (:total-runtimes %) index) hashmaps)))))]
    (map-indexed
     (fn [index item]
       {:generation index
        :value (apply min item)
        :std (std item)})
     min-total-runtimes-indexed)))


(defn fastest-individual
  "Returns the individual with the smallest total runtime over all the runs."
  [hashmaps]
  (loop [cnt 1
         best-hashmap (first hashmaps)]
    (let [best-idx (:index (:min-total-runtime best-hashmap))
          best-runtime (:value (:min-total-runtime best-hashmap))
          best-filename (:file-name best-hashmap)]
      (if (= cnt (count hashmaps))
        {:file-name best-filename
         :index best-idx
         :value best-runtime}
        (recur (inc cnt)
               (if (< (:value (:min-total-runtime (nth hashmaps cnt)))
                      best-runtime)
                 (nth hashmaps cnt)
                 best-hashmap))))))
