(ns test.tree.jenkins
  (:require [test.tree :as tree]
            [test.tree.reporter :as report]
            [clojure.zip :as zip])
  (:use clojure.pprint
        fn.trace))

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
    (binding [tracer (fn [_ value & [out?]]
                       (dosync
                        (alter test-traces update-in [test]
                               (fn [trace-zip]
                                 (add-trace (or trace-zip (zip/vector-zip []))
                                            [value out?])))))]
      (let [result (runner test)]
        (if (-> (:result result) (= :fail))
          (assoc-in  result [:error :trace] (if-let [t (@test-traces test)]
                                              (zip/root t)))
          result)))))

(defn run-suite
  "Run the suite with tracing, and output a testng result file with
   syntaxhighlighted trace. Also print a report of blockers."
  [suite & [{:keys [to-trace do-not-trace syntax-highlight-url]
             :or {to-trace []
                  do-not-trace #{}
                  syntax-highlight-url "/shared/syntaxhighlighter/"}}]]
  (with-redefs [tree/runner (-> tree/execute
                               wrap-tracing
                               tree/wrap-blockers
                               tree/wrap-timer
                               tree/wrap-data-driven)]
    (binding [tracer (per-thread-tracer)
              *print-level* 10
              *print-length* 30
              *print-right-margin* 150
              *print-miser-width* 120 
              report/syntax-highlight (report/syntax-highlighter syntax-highlight-url)]
      (dotrace (remove do-not-trace (all-fns to-trace)) 
        (let [reports (tree/run-suite suite)]
          (println "----- Blockers -----\n ")
          (let [blockers (->> reports
                            vals
                            (mapcat #(get-in % [:report :blocked-by]))
                            (filter identity)
                            (map #(select-keys % [:name :parameters]))
                            frequencies)]
            (pprint blockers)))))))


