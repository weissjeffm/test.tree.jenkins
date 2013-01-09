(ns test.tree.jenkins
  (:require [test.tree :as tree]
            [test.tree.debug :as debug]
            [test.tree.reporter :as report]
            [clojure.zip :as zip]
            [clojure.pprint :refer :all]
            [fn.trace :as trace]))

(defn run-suite
  "Run the suite with tracing, and output a testng result file with
   syntaxhighlighted trace. Also print a report of blockers. You can
   specify a list of namespaces and functions to trace (or not trace).
   Alternatively a function that generates a list of functions to
   trace (if both the list and function are specified, the list
   wins)."
  [suite & [{:keys [to-trace to-trace-fn do-not-trace
                    syntax-highlight-url]
             :or {do-not-trace #{}
                  syntax-highlight-url "/shared/syntaxhighlighter/"}}]]
  (with-redefs [tree/runner (-> tree/execute
                               debug/wrap-tracing
                               tree/wrap-blockers
                               tree/wrap-timer
                               tree/wrap-data-driven)
                trace/tracer trace/thread-tracer]
    (binding [*print-level* 20
              *print-length* 40
              *print-right-margin* 150
              *print-miser-width* 120 
              report/syntax-highlight (report/syntax-highlighter syntax-highlight-url)]
      (trace/dotrace (cond to-trace (remove do-not-trace (trace/all-fns to-trace))
                     to-trace-fn (to-trace-fn)
                     :else []) 
        (let [reports (tree/run-suite suite)]
          (println "----- Blockers -----\n ")
          (pprint (reporter/blocker-report reports)))))))


