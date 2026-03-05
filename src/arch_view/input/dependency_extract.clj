(ns arch-view.input.dependency-extract
  (:require [clojure.java.io :as io]
            [arch-view.input.source-scan :as scan]
            [arch-view.model.graph :as graph]))

(defn- read-first-form
  [path]
  (with-open [r (java.io.PushbackReader. (io/reader path))]
    (read r)))

(defn- ns-form?
  [form]
  (and (seq? form)
       (= 'ns (first form))))

(defn- dependency-symbols
  [ns-form]
  (let [clauses (drop 2 ns-form)
        requires (->> clauses
                      (filter #(and (seq? %)
                                    (= :require (first %))))
                      (mapcat rest))]
    (->> requires
         (map (fn [req]
                (cond
                  (symbol? req) req
                  (vector? req) (first req)
                  :else nil)))
         (filter symbol?)
         (map str)
         set)))

(defn- namespace-in-file
  [path]
  (let [form (read-first-form path)]
    (when (ns-form? form)
      (str (second form)))))

(defn build-module-graph
  [project-path source-paths]
  (let [files (scan/discover-source-files project-path source-paths)
        module-by-file (into {}
                             (for [f files
                                   :let [module (namespace-in-file f)]
                                   :when module]
                               [f module]))
        nodes (set (vals module-by-file))
        edges (set
               (mapcat (fn [[file from]]
                         (let [deps (dependency-symbols (read-first-form file))]
                           (for [to deps
                                 :when (contains? nodes to)]
                             {:from from :to to})))
                       module-by-file))]
    (graph/make-graph nodes edges)))
