(ns arch-view.input.dependency-checker-test
  (:require [clojure.test :refer [deftest is]]
            [arch-view.input.dependency-checker :as sut]))

(deftest read-guidance-parses-edn
  (let [tmp-file (doto (java.io.File/createTempFile "dependency-checker" ".edn")
                   (.deleteOnExit))]
    (spit tmp-file
          "{:source-paths [\"src\"] :component-rules [{:component :core :kind :concrete :match \"my.app.*\"}]}")
    (is (= {:source-paths ["src"]
            :component-rules [{:component :core :kind :concrete :match "my.app.*"}]}
           (sut/read-guidance (.getAbsolutePath tmp-file))))))
