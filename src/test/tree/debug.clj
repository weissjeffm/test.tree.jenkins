(ns test.tree.debug
  (:require test.tree
            test.tree.reporter
            [fn.trace :as trace]
            [clojure.zip :as zip]))

(def test-traces (ref {}))

(defn add-trace
  "Add trace calls and results into a zipper tree structure, so that
   it can be printed out at any time."
  [trace-zip [entry out?]]
  (if out? 
    (-> trace-zip (zip/append-child entry) zip/up ) 
    (-> trace-zip (zip/append-child [entry]) zip/down zip/rightmost))) 

(defn wrap-tracing
  "If the suite is run with tracing on, save the trace in the results."
  [runner]
  (fn [test]
    ; Need to remove :blocked-by key so that traces may be looked up by test map
    (let [pure-test (dissoc test :blocked-by)]
      (binding [trace/tracer (fn [_ value & [out?]]
                         (dosync
                          (alter test-traces update-in [pure-test]
                                 (fn [trace-zip]
                                   (add-trace (or trace-zip (zip/vector-zip []))
                                              [value out?])))))]
        (let [result (runner test)]
          (if (-> (:result result) (= :fail))
            (assoc-in  result [:error :trace] (if-let [t (@test-traces pure-test)]
                                                (zip/root t)))
            result))))))

(defmacro wrap-swank-conn-maybe
  "Produce a wrap-swank function that does nothing, if swank is not
   available."
  []
  (let [runnersym (gensym "runner")]
    `(defn wrap-swank
       "Allows you to place (swank.core/break) statements anywhere, and the
  swank debugger will stop there, no matter which thread hits that
  line."
       [~runnersym]
       ~(if (resolve 'swank.core.connection/*current-connection*)
          `(fn [test#]
             (let [conn# swank.core.connection/*current-connection*]
               (binding [swank.core.connection/*current-connection* conn#]
                 (~runnersym test#))))
          `(fn [test#]
             (~runnersym test#))))))

(wrap-swank-conn-maybe)

(defn debug
  "Run the given test tree, tracing all the functions in trace-list.
   The trace will be stored along with the rest of the test results.
   Accepts an optional reference for referring to the test results
   when running asynchronously.
   See also test.tree/run"
  [tree trace-list & [results-ref]]
  (with-redefs [test.tree/runner (-> test.tree/execute
                                    wrap-tracing
                                    test.tree/wrap-blockers
                                    test.tree/wrap-timer
                                    test.tree/wrap-data-driven
                                    wrap-swank)]
    (trace/dotrace trace-list
      (let [results (test.tree/run tree)]
        (when results-ref (reset! results-ref results))
        (doall (->> results second deref vals (map (comp deref :promise))))
        results))))

(defn quick-report
  "Print a quick summary of test results given result
  data returned by test.tree/run."
  [result]
  (let [reports (-> result second)]
    (binding [test.tree.reporter/*reports* reports]
      (let [fails (into {}
                        (filter (fn [[k v]] (test.tree.reporter/failed? k)) @reports))]
        {:failed-tests fails
         :counts {:failed (count fails)
                  :passed (count (test.tree.reporter/passed-tests))
                  :skipped (count (test.tree.reporter/skipped-tests))}}))))
