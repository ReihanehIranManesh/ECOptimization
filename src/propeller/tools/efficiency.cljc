(ns propeller.tools.efficiency 
  "Functions to measure memory usage (used for benchmarking)"
  (:require [propeller.tools.math :as math]
            [java.lang.System :as System]
            [java.lang.Thread :as Thread]
            [java.lang.management.ManagementFactory :as ManagementFactory]))

;;from https://github.com/jumarko/clojure-experiments/blob/master/src/clojure_experiments/performance/memory.clj#L139-L198

(defn thread-allocated-bytes [t]
  (let [thread-mbean (ManagementFactory/getThreadMXBean)
        thread-id (.getId t)]
    (.getThreadAllocatedBytes thread-mbean thread-id)))

(defn allocated-bytes
  [f]
  (let [thread (Thread/currentThread)
        start (thread-allocated-bytes thread)]
    (f)
    (- (thread-allocated-bytes thread) start)))

;;end of reused code

(defn measure-time-ms
  [expr]
  (let [start-time (System/nanoTime)
        _ (eval expr)]
    (/ (double (- (System/nanoTime) start-time)) 1000000.0)))

(defn runtimes
  ([expr] (runtimes expr 500))
  ([expr run-cnt]
   (let [jvm-warmup 50
         runtimes-ms (drop jvm-warmup (repeatedly (+ jvm-warmup run-cnt) #(measure-time-ms expr)))]
     runtimes-ms)))

(defn average-runtimes
  ([expr] (average-runtimes expr 500))
  ([expr run-cnt]
   (math/mean (runtimes expr run-cnt))))
