(ns arch-view.input.dependency-extract-spec
  (:require [arch-view.input.dependency-extract :as sut]
            [speclj.core :refer :all]))

(describe "dependency extraction"
  (it "builds a module graph from ns requires"
    (let [root (.toFile (java.nio.file.Files/createTempDirectory "arch-view-src" (make-array java.nio.file.attribute.FileAttribute 0)))
          src-dir (doto (java.io.File. root "src") .mkdirs)
          a-file (java.io.File. src-dir "my/app/a.clj")
          b-file (java.io.File. src-dir "my/app/b.clj")]
      (.mkdirs (.getParentFile a-file))
      (spit a-file "(ns my.app.a (:require [my.app.b :as b] [clojure.string :as str]))")
      (spit b-file "(ns my.app.b)")
      (let [graph (sut/build-module-graph (.getAbsolutePath root) ["src"])]
        (should= #{"my.app.a" "my.app.b"}
                 (:nodes graph))
        (should= #{{:from "my.app.a" :to "my.app.b"}}
                 (:edges graph))
        (should= (.getAbsolutePath a-file)
                 (get (:module->source-file graph) "my.app.a"))
        (should= (.getAbsolutePath b-file)
                 (get (:module->source-file graph) "my.app.b")))))

  (it "supports reader conditionals in namespace forms"
    (let [root (.toFile (java.nio.file.Files/createTempDirectory "arch-view-src-rc" (make-array java.nio.file.attribute.FileAttribute 0)))
          src-dir (doto (java.io.File. root "src") .mkdirs)
          a-file (java.io.File. src-dir "my/app/a.cljc")
          b-file (java.io.File. src-dir "my/app/b.clj")]
      (.mkdirs (.getParentFile a-file))
      (spit a-file "(ns my.app.a #?(:clj (:require [my.app.b :as b])))")
      (spit b-file "(ns my.app.b)")
      (let [graph (sut/build-module-graph (.getAbsolutePath root) ["src"])]
        (should= #{"my.app.a" "my.app.b"}
                 (:nodes graph))
        (should= #{{:from "my.app.a" :to "my.app.b"}}
                 (:edges graph)))))

  (it "marks modules with polymorphic definitions as abstract"
    (let [root (.toFile (java.nio.file.Files/createTempDirectory "arch-view-src-abstract" (make-array java.nio.file.attribute.FileAttribute 0)))
          src-dir (doto (java.io.File. root "src") .mkdirs)
          api-file (java.io.File. src-dir "my/app/api.clj")
          service-file (java.io.File. src-dir "my/app/service.clj")]
      (.mkdirs (.getParentFile api-file))
      (spit api-file "(ns my.app.api) (defprotocol ServicePort (run [this]))")
      (spit service-file "(ns my.app.service) (defmulti perform (fn [m] (:type m)))")
      (let [graph (sut/build-module-graph (.getAbsolutePath root) ["src"])]
        (should= #{"my.app.api" "my.app.service"}
                 (:nodes graph))
        (should= #{"my.app.api" "my.app.service"}
                 (:abstract-modules graph)))))

  (it "does not fail when non-ns forms are unreadable for polymorphic scanning"
    (let [root (.toFile (java.nio.file.Files/createTempDirectory "arch-view-src-invalid" (make-array java.nio.file.attribute.FileAttribute 0)))
          src-dir (doto (java.io.File. root "src") .mkdirs)
          a-file (java.io.File. src-dir "my/app/a.clj")
          b-file (java.io.File. src-dir "my/app/b.clj")]
      (.mkdirs (.getParentFile a-file))
      (spit a-file "(ns my.app.a (:require [my.app.b :as b])) ::contracts/givens")
      (spit b-file "(ns my.app.b)")
      (let [graph (sut/build-module-graph (.getAbsolutePath root) ["src"])]
        (should= #{"my.app.a" "my.app.b"}
                 (:nodes graph))
        (should= #{{:from "my.app.a" :to "my.app.b"}}
                 (:edges graph))))))
