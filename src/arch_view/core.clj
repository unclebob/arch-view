(ns arch-view.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [arch-view.input.dependency-checker :as checker]
            [arch-view.input.dependency-extract :as extract]
            [arch-view.layout.layers :as layers]
            [arch-view.model.classify :as classify]
            [arch-view.model.components :as components]
            [arch-view.render.quil-view :as render]))

(defn load-architecture
  [project-path]
  (let [guidance-file (io/file project-path "dependency-checker.edn")
        guidance (if (.exists guidance-file)
                   (checker/read-guidance (.getAbsolutePath guidance-file))
                   {:source-paths ["src"] :component-rules []})
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

(defn parse-args
  [args]
  (loop [remaining args
         opts {:project-path "." :no-gui false}]
    (if (empty? remaining)
      opts
      (let [arg (first remaining)]
        (cond
          (= "--project-path" arg) (recur (nnext remaining)
                                          (assoc opts :project-path (second remaining)))
          (= "--no-gui" arg) (recur (next remaining)
                                    (assoc opts :no-gui true))
          :else (recur (next remaining) opts))))))

(defn -main [& args]
  (let [{:keys [project-path no-gui]} (parse-args args)
        {:keys [graph scene]} (load-architecture project-path)]
    (println "Architecture loaded")
    (println "Nodes:" (count (:nodes graph)))
    (println "Edges:" (count (:edges graph)))
    (when-not no-gui
      (render/show! scene {:title (str "architecture-viewer: " (str/trim project-path))}))))
