(ns arch-view.input.dependency-extract
  (:require [clojure.java.io :as io]
            [arch-view.input.source-scan :as scan]
            [arch-view.model.graph :as graph]))

(defn- read-first-form
  [path]
  (with-open [r (java.io.PushbackReader. (io/reader path))]
    (read {:read-cond :allow :features #{:clj}} r)))

(defn- read-all-forms
  [path]
  (with-open [r (java.io.PushbackReader. (io/reader path))]
    (loop [forms []]
      (let [form (try
                   (read {:read-cond :allow :features #{:clj}} r)
                   (catch java.io.EOFException _
                     ::eof)
                   (catch RuntimeException ex
                     (if (.startsWith (or (.getMessage ex) "") "EOF while reading")
                       ::eof
                       (throw ex))))]
        (if (= ::eof form)
          forms
          (recur (conj forms form)))))))

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

(def polymorphic-defs
  #{"defprotocol" "defmulti" "definterface"})

(defn- polymorphic-module?
  [path]
  (try
    (some (fn [form]
            (and (seq? form)
                 (symbol? (first form))
                 (contains? polymorphic-defs (name (first form)))))
          (read-all-forms path))
    (catch RuntimeException _
      false)))

(defn build-module-graph
  [project-path source-paths]
  (let [files (scan/discover-source-files project-path source-paths)
        module-by-file (into {}
                             (for [f files
                                   :let [module (namespace-in-file f)]
                                   :when module]
                               [f module]))
        files-by-module (reduce (fn [acc [file module]]
                                  (update acc module (fnil conj []) file))
                                {}
                                module-by-file)
        module->source-file (into {}
                                  (for [[module module-files] files-by-module
                                        :let [best-file (->> module-files
                                                             sort
                                                             first)]]
                                    [module best-file]))
        nodes (set (vals module-by-file))
        abstract-modules (set
                          (for [[file module] module-by-file
                                :when (polymorphic-module? file)]
                            module))
        edges (set
               (mapcat (fn [[file from]]
                         (let [deps (dependency-symbols (read-first-form file))]
                           (for [to deps
                                 :when (contains? nodes to)]
                             {:from from :to to})))
                       module-by-file))]
    (merge (graph/make-graph nodes edges)
           {:abstract-modules abstract-modules
            :module->source-file module->source-file})))
