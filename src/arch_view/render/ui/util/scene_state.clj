;; mutation-tested: 2026-03-08
(ns arch-view.render.ui.util.scene-state)

(defn drillable?
  [architecture namespace-path module view-architecture]
  (let [candidate (conj (or namespace-path []) module)
        child-view (view-architecture architecture candidate)]
    (seq (get-in child-view [:graph :nodes]))))

(defn attach-drillable-markers
  [scene architecture namespace-path view-architecture]
  (if-not architecture
    scene
    (update scene :module-positions
            (fn [positions]
              (mapv (fn [position]
                      (let [drillable (boolean (drillable? architecture
                                                           namespace-path
                                                           (:module position)
                                                           view-architecture))
                            label (:label position)]
                        (assoc position
                               :drillable? drillable
                               :display-label label)))
                    positions)))))

(defn drilldown-scene
  [state path scroll-x scroll-y {:keys [view-architecture build-scene attach-drillable-markers]}]
  (let [view (view-architecture (:architecture state) path)
        scene (-> (build-scene view)
                  (attach-drillable-markers (:architecture state) path))]
    (assoc state
           :namespace-path path
           :scroll-x (double (or scroll-x 0.0))
           :scroll-y (double (or scroll-y 0.0))
           :scene scene
           :routed-edges nil)))

(defn push-nav-state
  [{:keys [namespace-path scroll-x scroll-y nav-stack scene zoom zoom-stack] :as state}]
  (assoc state :nav-stack (conj (vec (or nav-stack []))
                                {:path (vec (or namespace-path []))
                                 :scene scene
                                 :scroll-x (double (or scroll-x 0.0))
                                 :scroll-y (double (or scroll-y 0.0))
                                 :zoom (double (or zoom 1.0))
                                 :zoom-stack (vec (or zoom-stack []))})))

(defn initial-scene-for-show
  [scene architecture {:keys [view-architecture build-scene attach-drillable-markers]}]
  (if architecture
    (let [initial-view (view-architecture architecture [])]
      (-> (build-scene initial-view)
          (attach-drillable-markers architecture [])))
    scene))
