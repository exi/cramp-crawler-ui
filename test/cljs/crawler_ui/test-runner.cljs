(ns crawler_ui.test-runner
  (:require
   [cljs.test :refer-macros [run-tests]]
   [crawler_ui.core-test]))

(enable-console-print!)

(defn runner []
  (if (cljs.test/successful?
       (run-tests
        'crawler_ui.core-test))
    0
    1))
