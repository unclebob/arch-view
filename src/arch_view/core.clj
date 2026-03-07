(ns arch-view.core
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [arch-view.input.dependency-extract :as extract]
            [arch-view.layout.layers :as layers]
            [arch-view.model.classify :as classify]
            [arch-view.model.components :as components]
            [arch-view.render.ui.quil.view :as render]))

(def default-guidance
  {:source-paths ["src"]
   :component-rules []})

(defn load-architecture
  [project-path]
  (let [guidance default-guidance
        source-paths (or (:source-paths guidance) ["src"])
        graph (extract/build-module-graph project-path source-paths)
        module->component (components/assign-components guidance (:nodes graph))
        layout (layers/assign-layers graph)
        classified-edges (classify/classify-edges guidance graph)
        scene (render/build-scene {:layout layout
                                   :classified-edges classified-edges
                                   :module->component module->component})]
    {:guidance guidance
     :graph graph
     :module->component module->component
     :layout layout
     :classified-edges classified-edges
     :scene scene}))

(defn load-architecture-edn
  [path]
  (let [architecture (-> path slurp edn/read-string)]
    (if (:scene architecture)
      architecture
      (assoc architecture
             :scene (render/build-scene architecture)))))

(defn parse-args
  [args]
  (loop [remaining args
         opts {:project-path "." :in-edn nil :no-gui false :skip-routing false :out nil}]
    (if (empty? remaining)
      opts
      (let [arg (first remaining)]
        (cond
          (= "--project-path" arg) (recur (nnext remaining)
                                          (assoc opts :project-path (second remaining)))
          (= "--in-edn" arg) (recur (nnext remaining)
                                    (assoc opts :in-edn (second remaining)))
          (= "--out" arg) (recur (nnext remaining)
                                 (assoc opts :out (second remaining)))
          (= "--no-gui" arg) (recur (next remaining)
                                    (assoc opts :no-gui true))
          (= "--skip-routing" arg) (recur (next remaining)
                                          (assoc opts :skip-routing true))
          :else (recur (next remaining) opts))))))

(defn exit-program!
  []
  (shutdown-agents)
  (System/exit 0))

(defn -main [& args]
  (let [{:keys [project-path in-edn no-gui skip-routing out]} (parse-args args)
        architecture (if in-edn
                       (load-architecture-edn in-edn)
                       (load-architecture project-path))
        source-label (or in-edn project-path)
        {:keys [graph scene]} architecture]
    (println "Architecture loaded")
    (println "Nodes:" (count (:nodes graph)))
    (println "Edges:" (count (:edges graph)))
    (when out
      (spit out (pr-str architecture)))
    (when-not no-gui
      (-> (render/show! scene {:title (str "architecture-viewer: " (str/trim source-label))
                               :architecture architecture
                               :skip-routing? skip-routing})
          (render/wait-until-closed!))
      (exit-program!))))
