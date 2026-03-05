(ns arch-view.render.quil-view)

(defn- layer-y
  [index layer-height layer-gap]
  (* index (+ layer-height layer-gap)))

(defn- module-positions-for-layer
  [layer-index modules canvas-width layer-height layer-gap]
  (let [count-modules (count modules)
        spacing (/ canvas-width (inc count-modules))
        y (+ (layer-y layer-index layer-height layer-gap) (/ layer-height 2))]
    (map-indexed (fn [idx module]
                   {:module module
                    :layer layer-index
                    :x (* (inc idx) spacing)
                    :y y})
                 modules)))

(defn- arrowhead-for
  [edge-type]
  (if (= :abstract edge-type)
    :closed-triangle
    :standard))

(defn build-scene
  [architecture {:keys [canvas-width layer-height layer-gap]
                 :or {canvas-width 1200 layer-height 140 layer-gap 24}}]
  (let [layers (get-in architecture [:layout :layers])
        layer-rects (mapv (fn [{:keys [index]}]
                            {:index index
                             :x 0
                             :y (layer-y index layer-height layer-gap)
                             :width canvas-width
                             :height layer-height})
                          layers)
        module-positions (->> layers
                              (mapcat (fn [{:keys [index modules]}]
                                        (module-positions-for-layer index modules canvas-width layer-height layer-gap)))
                              vec)
        edge-drawables (->> (:classified-edges architecture)
                            (map (fn [{:keys [from to type]}]
                                   {:from from
                                    :to to
                                    :arrowhead (arrowhead-for type)}))
                            vec)]
    {:layer-rects layer-rects
     :module-positions module-positions
     :edge-drawables edge-drawables}))
