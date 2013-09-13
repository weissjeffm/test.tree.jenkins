(ns test.tree.webdriver
  (:require [clj-webdriver.taxi :as browser]
            [clj-webdriver.remote.server :as rs]))

(defn wrap-webdriver
  "Run the tests on a selenium webdriver server.  selenium-server is a
  remote webdriver instance.  'capabilities-chooser-fn' is a function
  that takes the test as an argument and returns browser capabilities
  associated with that test.  'finder-fn' is a finder function to bind
  to taxi's *finder-fn*.  If test has key :headless true, webdriver
  won't be used at all."
  [runner {:keys [selenium-server capabilities-chooser-fn finder-fn]}]
  (fn [{:keys [test] :as req}]
    (if (:headless test)
      (runner req) 
      (binding [browser/*driver* (rs/new-remote-driver selenium-server
                                                       {:capabilities (capabilities-chooser-fn test)})
                browser/*finder-fn* finder-fn]
        (let [res (-> req
                      (assoc :webdriver browser/*driver*)
                      runner)]
          (try (browser/quit)
               res
               (catch Exception e
                 (assoc res :webdriver-error ))))))))
