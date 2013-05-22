(ns test.tree.debug
  (:require test.tree
            test.tree.reporter
            [fn.trace :as trace]
            [clojure.zip :as zip]))

(defn add-trace
  "Add trace calls and results into a zipper tree structure, so that
   it can be printed out at any time."
  [trace-zip [entry out?]]
  (if out? 
    (-> trace-zip (zip/append-child entry) zip/up ) 
    (-> trace-zip (zip/append-child [entry]) zip/down zip/rightmost))) 

(defn tree-trace "Turn a list of traces into a pretty printable tree"
  [trace-list]
  (->> trace-list
       (reduce add-trace (zip/vector-zip []))
       zip/root))

(deftype TestTrace [trace-list])

(defmethod clojure.pprint/simple-dispatch TestTrace [o]
  (println "TestTrace simple-dispatch")
  (-> o .trace-list tree-trace clojure.pprint/simple-dispatch))

(defmethod print-method TestTrace [o w]
  (pr (.trace-list o)))

(defn wrap-tracing
  "If the suite is run with tracing on, save the trace in the results."
  [runner]
  (fn [{:keys [test] :as req}]
    (let [test-trace (atom (vector))]
      (binding [trace/tracer (fn [_ value & [out?]]
                               (swap! test-trace conj (vector value out?)))]
        (assoc-in (runner req) [:error :trace]
                  (TestTrace. @test-trace))))))

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
          `(fn [req#]
             (let [conn# swank.core.connection/*current-connection*]
               (binding [swank.core.connection/*current-connection* conn#]
                 (~runnersym req#))))
          `(fn [req#]
             (~runnersym req#))))))

(wrap-swank-conn-maybe)

(defn debug
  "Run the given test tree, tracing all the functions given by
   trace-list. The trace will be stored along with the rest of the
   test results. Accepts an optional reference for referring to the
   test results when running asynchronously.  See also test.tree/run"
  [tree & [{:keys [trace-list reports-ref]
            :or {trace-list (list)}
            :as opts}]]
  (with-redefs [test.tree/runner (-> test.tree/execute
                                    wrap-tracing
                                    test.tree/wrap-blockers
                                    test.tree/wrap-timer
                                    test.tree/wrap-data-driven
                                    wrap-swank)]
    (trace/dotrace trace-list
      (let [results (test.tree/run tree opts)]
        (when reports-ref (dosync (ref-set reports-ref results)))
        (doall (->> results second deref vals (map (comp deref :lock))))
        results))))

(defn quick-report
  "Print a quick summary of test results given result
  data returned by test.tree/run."
  [result]
  (let [by-result (->> result second deref
                     (group-by test.tree.reporter/result))]
    {:failed-tests (into {} (by-result :fail))
     :counts
     (let [res-type [:fail :pass :skip]]
       (zipmap res-type (map (comp count by-result) res-type)))}))
