(ns arch-view.layout.layers)

(defn- outgoing
  [edges node]
  (->> edges
       (filter #(= node (:from %)))
       (map :to)
       set))

(defn- depth-fn
  [edges]
  (let [memo (atom {})]
    (letfn [(depth [node]
              (if-some [cached (get @memo node)]
                cached
                (let [deps (outgoing edges node)
                      value (if (empty? deps)
                              0
                              (inc (apply max (map depth deps))))]
                  (swap! memo assoc node value)
                  value))) ]
      depth)))

(defn assign-layers
  [{:keys [nodes edges]}]
  (let [depth (depth-fn edges)
        module->layer (into {}
                            (for [n nodes]
                              [n (depth n)]))
        layers (->> module->layer
                    (group-by val)
                    (sort-by key)
                    (mapv (fn [[idx pairs]]
                            {:index idx
                             :modules (->> pairs (map key) sort vec)})))]
    {:layers layers
     :module->layer module->layer}))
