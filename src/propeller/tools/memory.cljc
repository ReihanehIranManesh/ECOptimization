(ns propeller.tools.memory
  "Functions to measure memory usage (used for benchmarking)")

;;from https://github.com/jumarko/clojure-experiments/blob/master/src/clojure_experiments/performance/memory.clj#L139-L198

(defn thread-allocated-bytes [t]
  (let [thread-mbean (java.lang.management.ManagementFactory/getThreadMXBean)
        thread-id (.getId t)]
    (.getThreadAllocatedBytes thread-mbean thread-id)))

(defn allocated-bytes
  [f]
  (let [thread (Thread/currentThread)
        start (thread-allocated-bytes thread)]
    (f)
    (- (thread-allocated-bytes thread) start)))
