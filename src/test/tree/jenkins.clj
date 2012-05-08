(ns test.tree.jenkins
  (:require [test.tree :as tree]
            [test.tree.reporter :as report])
  (:use clojure.pprint
        fn.trace))

(def test-traces (ref {}))

(defn wrap-tracing [runner]
  (fn [test]
    (binding [tracer (fn [_ value & [out?]]
                       (dosync
                        (alter test-traces update-in [test]
                               (fn [v]
                                 (conj (or v (with-meta [] {:log true})) [value out?])))))]
      (let [result (runner test)]
        (if (-> (:result result) (= :fail))
          (assoc-in  result [:error :trace] (@test-traces test))
          result)))))

(defn run-suite [suite & [{:keys [to-trace do-not-trace syntax-highlight-url]
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
              *print-pprint-dispatch* log-dispatch
              report/syntax-highlight (report/syntax-highlighter syntax-highlight-url)]
      (dotrace (remove do-not-trace (all-fns to-trace)) 
        (let [reports (tree/run-suite suite)]
          (println "----- Blockers -----\n ")
          (let [blockers (->> reports
                            vals
                            (mapcat #(get-in % [:report :blocked-by]))
                            (filter #(not (nil? %)))
                            frequencies)]
            (pprint blockers)))))))

