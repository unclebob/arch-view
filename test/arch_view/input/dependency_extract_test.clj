(ns arch-view.input.dependency-extract-test
  (:require [clojure.test :refer [deftest is]]
            [arch-view.input.dependency-extract :as sut]))

(deftest build-module-graph-reads-ns-requires
  (let [root (.toFile (java.nio.file.Files/createTempDirectory "arch-view-src" (make-array java.nio.file.attribute.FileAttribute 0)))
        src-dir (doto (java.io.File. root "src") .mkdirs)
        a-file (java.io.File. src-dir "my/app/a.clj")
        b-file (java.io.File. src-dir "my/app/b.clj")]
    (.mkdirs (.getParentFile a-file))
    (spit a-file "(ns my.app.a (:require [my.app.b :as b] [clojure.string :as str]))")
    (spit b-file "(ns my.app.b)")
    (let [graph (sut/build-module-graph (.getAbsolutePath root) ["src"]) ]
      (is (= #{"my.app.a" "my.app.b"}
             (:nodes graph)))
      (is (= #{{:from "my.app.a" :to "my.app.b"}}
             (:edges graph))))))
