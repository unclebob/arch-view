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

(defn usage-summary
  []
  (str
   "Usage: clj -M:run [options]\n"
   "\n"
   "Options:\n"
   "  --help                     Print this usage summary and exit.\n"
   "  --project-path <path>      Project root to scan (default: current directory).\n"
   "  --in-edn <file>            Load architecture from an EDN file instead of scanning source.\n"
   "  --out <file>               Write architecture EDN output to file.\n"
   "  --no-gui                   Run headless (do not open the interactive viewer).\n"))

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
  (let [flag-handlers {"--help" (fn [remaining opts]
                                  [(next remaining) (assoc opts :help true)])
                       "--no-gui" (fn [remaining opts]
                                    [(next remaining) (assoc opts :no-gui true)])}
        value-handlers {"--project-path" :project-path
                        "--in-edn" :in-edn
                        "--out" :out}]
    (loop [remaining args
           opts {:project-path "." :in-edn nil :no-gui false :out nil :help false}]
      (if (empty? remaining)
        opts
        (let [arg (first remaining)]
          (if-let [handle-flag (get flag-handlers arg)]
            (let [[next-remaining next-opts] (handle-flag remaining opts)]
              (recur next-remaining next-opts))
            (if-let [key-name (get value-handlers arg)]
              (recur (nnext remaining)
                     (assoc opts key-name (second remaining)))
              (recur (next remaining) opts))))))))

(defn exit-program!
  []
  (shutdown-agents)
  (System/exit 0))

(defn -main [& args]
  (let [{:keys [project-path in-edn no-gui out help]} (parse-args args)]
    (if help
      (println (usage-summary))
      (let [architecture (if in-edn
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
                                   :architecture architecture})
              (render/wait-until-closed!))
          (exit-program!))))))
