(ns arch-view.input.source-scan
  (:require [clojure.java.io :as io]))

(def extensions #{".clj" ".cljc" ".cljs"})

(defn source-file?
  [^java.io.File file]
  (let [name (.getName file)]
    (and (.isFile file)
         (some #(.endsWith name %) extensions))))

(defn discover-source-files
  [project-path source-paths]
  (->> source-paths
       (map #(io/file project-path %))
       (filter #(.exists ^java.io.File %))
       (mapcat file-seq)
       (filter source-file?)
       (map #(.getAbsolutePath ^java.io.File %))))
