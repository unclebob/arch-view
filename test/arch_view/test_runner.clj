(ns arch-view.test-runner
  (:require [clojure.test :as t]
            [arch-view.input.dependency-checker-test]
            [arch-view.input.dependency-extract-test]))

(defn -main [& _]
  (let [{:keys [fail error]}
        (t/run-tests 'arch-view.input.dependency-checker-test
                     'arch-view.input.dependency-extract-test)]
    (when (pos? (+ fail error))
      (System/exit 1))))
