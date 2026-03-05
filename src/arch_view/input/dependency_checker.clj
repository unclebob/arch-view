(ns arch-view.input.dependency-checker
  (:require [clojure.edn :as edn]))

(defn read-guidance
  "Reads dependency-checker.edn and returns its EDN map."
  [path]
  (-> path slurp edn/read-string))
