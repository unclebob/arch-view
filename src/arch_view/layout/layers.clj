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
    (letfn [(depth [node visiting]
              (if-some [cached (get @memo node)]
                cached
                (if (contains? visiting node)
                  0
                  (let [deps (outgoing edges node)
                        next-visiting (conj visiting node)
                      value (if (empty? deps)
                              0
                              (inc (apply max (map #(depth % next-visiting) deps))))]
                    (swap! memo assoc node value)
                    value))))]
      #(depth % #{}))))

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
