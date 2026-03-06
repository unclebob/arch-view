(ns arch-view.render.route-engine)

(defn route-edges
  [{:keys [spaced-edges
           resolve-edge-path
           normalize-route-endpoints
           place-non-overlapping-path
           path-segments]}]
  (loop [remaining spaced-edges
         placed []
         placed-segments []]
    (if (empty? remaining)
      placed
      (let [edge (first remaining)
            routed (resolve-edge-path edge)
            edge+ (merge edge routed)
            base-path (normalize-route-endpoints (or (:points routed) []) edge+)
            route-points (place-non-overlapping-path base-path edge+ placed-segments)
            final-path (if (seq route-points) route-points base-path)
            edge' (assoc edge
                         :route-points final-path
                         :anchored? (boolean (or (:from-rect edge) (:to-rect edge))))]
        (if (seq final-path)
          (recur (rest remaining)
                 (conj placed edge')
                 (into placed-segments (path-segments final-path)))
          (recur (rest remaining)
                 placed
                 placed-segments))))))
