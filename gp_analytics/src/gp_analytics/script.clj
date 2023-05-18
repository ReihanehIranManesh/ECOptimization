(ns gp-analytics.script
  "Created by Regina Deri
   Script for applying our statistics functions"
  (:require
   [clojure.string :as string]
   [clojure.java.io :as io]
   [gp-analytics.core :as gp-analytics]
   [gp-analytics.io :as gp-io]))


(def selections '[tournament tournament-efficiency lexicase lexicase2 lexicase-parallel])


(def problems '[balanced-parenthesis can-form-word char-follows
                                     common-subsequence contains-subset differ-by-n
                                     is-anagram palindrome prefix string-classification])





(def csv-keys '([:average-total-runtimes :value] [:min-total-runtimes :value]
                 [:average-total-errors :value] [:average-amalgamated-errors :value]))

(defn get-benchmarking-list
  "Gets the list of selection-problem pairs to benchmark"
  [] (for [x selections
        y problems]
    (vector x y)))

(defn hyphen-to-underscore
  "Replaces all hyphens with underscores in a string"
  [a] (string/replace a #"-" "_"))


(defn get-input-path
  "Finds the path of the input data for a given selection-problem pair."
  [selection problem]
  (let [in-root "resources\\data"
        selection-folder (hyphen-to-underscore selection)
        problem-folder (hyphen-to-underscore problem)]
    (.getPath (io/file in-root selection-folder problem-folder))))

(defn get-output-path
  "Finds the path to output statistics for a given selection-problem pair."
  [selection problem]
  (let [out-root "resources\\statistics"
        selection-folder (hyphen-to-underscore selection)]
    (.getPath (io/file out-root selection-folder))))

(defn analyze-problem
  "Analyzes a given problem, generating the statistics we need."
  [selection problem]
  (let [file-name-txt (str (hyphen-to-underscore problem) ".txt")
        input-folder (get-input-path selection problem)
        output-folder (get-output-path selection problem)
        csv-folder (.getPath (io/file output-folder "csv"))
        output-statistics-path (.getPath (io/file output-folder file-name-txt))
        output-statistics-path2 (.getPath (io/file output-folder (str "bte_" file-name-txt)))
        statistics (gp-analytics/folder-aggregate-statistics input-folder)
        statistics2 (gp-analytics/folder-aggregate-statistics input-folder {:best-total-error true}) ;analyze the development of the individual with best-total-error
        file-name-csv (str problem ".csv")
        file-name-csv2 (str "bte_" problem ".csv")
        csv-path (.getPath (io/file csv-folder file-name-csv))
        csv-path2 (.getPath (io/file csv-folder file-name-csv2))]
    (gp-io/serialize-save output-statistics-path statistics)
    (println "Saved regular statistics at" output-statistics-path)
    (gp-io/serialize-save output-statistics-path2 statistics2)
    (println "Saved best total error statistics at" output-statistics-path2)
    (gp-io/keys-to-csv csv-path statistics csv-keys)
    (println "Saved csv of regular statistics at" csv-path)
    (gp-io/keys-to-csv csv-path2 statistics2 csv-keys)
    (println "Saved csv of best total error statistics at" csv-path2)))
    


(defn analyze-all
  "Runs analyze-problem on all selection-problem pairs"
  [] (let [tasks (get-benchmarking-list)]
       (doseq
        [i (range (count tasks))]
         (let [task (nth tasks i)
               selection (first task)
               problem (second task)]
           (analyze-problem selection problem)))))




(defn threshold-passes 
  "Ultimately not used; list how many runs of a given selection-problem pair
   had their final test-error stay under a specific value."
  [selection problem threshold]
  (let [input-folder (get-input-path selection problem)
        stats (gp-analytics/folder-gpstat-statistics input-folder {:test-error-threshold threshold})]
    (count stats); number of runs that were succesful
    ))

(defn all-threshold-passes
  "Runs threshold-passes on all the selection-problem pairs."
  [] (println "Starting all-threshold-passes...")
  (println "Please be patient, the results will be ready in a few minutes.")
  (let [tasks (get-benchmarking-list)
        threshold-passes (for [threshold (range 5)]
                           (map (fn [selection]
                                  {(keyword selection)
                                   (for [problem problems]
                                     {(keyword problem)
                                      (threshold-passes selection problem threshold)})}) selections))]

    threshold-passes))


