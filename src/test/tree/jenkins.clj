(ns test.tree.jenkins
  (:require [test.tree :as tree]
            [test.tree.debug :as debug]
            [test.tree.webdriver :as wd]
            [test.tree.sauce :as sauce]
            [test.tree.reporter :as report]
            [clojure.pprint :refer :all]
            [fn.trace :as trace]))

(def ^{:doc "add tracing to default test.tree middleware"}
  debug-middleware (-> tree/execute
                       debug/wrap-tracing
                       tree/wrap-blockers
                       tree/wrap-timer
                       tree/wrap-data-driven))

(defn debug+sauce-middleware
  "see sauce/wrap-sauce for options"
  [opts]
  (-> tree/execute
      (sauce/wrap-sauce opts)
      (wd/wrap-webdriver opts)
      debug/wrap-tracing
      tree/wrap-blockers
      tree/wrap-timer
      tree/wrap-data-driven))

(defn debug+webdriver-middleware
  "see wd/wrap-webdriver for options"
  [opts]
  (-> tree/execute
      (wd/wrap-webdriver opts)
      debug/wrap-tracing
      tree/wrap-blockers
      tree/wrap-timer
      tree/wrap-data-driven))

(defn run-suite
  "Run the suite with tracing, and output a testng result file with
   syntaxhighlighted trace. Also print a report of blockers. You can
   specify a list of namespaces and functions to trace (or not trace).
   Alternatively a function that generates a map of functions to trace
   and how deep to trace them (if both the list and function are
   specified, the list wins)."
  [suite & [{:keys [middleware to-trace do-not-trace trace-depths-fn
                    syntax-highlight-url]
             :or {do-not-trace #{}
                  syntax-highlight-url "/shared/syntaxhighlighter/"
                  middleware debug-middleware}
             :as opts}]]
  (with-redefs [tree/runner middleware
                trace/tracer trace/thread-tracer]
    (binding [*print-level* 20
              *print-length* 40
              *print-right-margin* 150
              *print-miser-width* 120
              report/syntax-highlight (report/syntax-highlighter syntax-highlight-url)]
      (trace/dotrace-depth (cond to-trace (zipmap (->> to-trace
                                                       trace/all-fns
                                                       (remove do-not-trace))
                                                  (repeat nil))
                                 trace-depths-fn (trace-depths-fn)
                                 :else [])
                           (let [reports (tree/run-suite suite opts)]
                             (println "----- Blockers -----\n ")
                             (pprint (report/blocker-report reports)))))))
