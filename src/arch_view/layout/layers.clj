(ns arch-view.layout.layers
  (:require [clojure.set :as set]))

(def ^:private exact-feedback-max-nodes 8)
(def ^:private exact-feedback-max-edges 12)

(defn- normalize-edges
  [nodes edges]
  (->> edges
       (keep (fn [{:keys [from to]}]
               (when (and from to
                          (contains? nodes from)
                          (contains? nodes to))
                 {:from from :to to})))
       set
       vec))

(defn- outgoing-map
  [nodes edges]
  (reduce (fn [acc {:keys [from to]}]
            (update acc from (fnil conj #{}) to))
          (zipmap nodes (repeat #{}))
          edges))

(defn- incoming-map
  [nodes edges]
  (reduce (fn [acc {:keys [from to]}]
            (update acc to (fnil conj #{}) from))
          (zipmap nodes (repeat #{}))
          edges))

(defn- indegree-map
  [nodes edges]
  (reduce (fn [acc {:keys [to]}]
            (update acc to (fnil inc 0)))
          (zipmap nodes (repeat 0))
          edges))

(defn- topological-order
  [nodes edges]
  (let [outgoing (outgoing-map nodes edges)
        indegree (indegree-map nodes edges)]
    (loop [queue (into (sorted-set) (for [[n d] indegree :when (zero? d)] n))
           indegree indegree
           ordered []]
      (if (empty? queue)
        ordered
        (let [node (first queue)
              queue (disj queue node)
              deps (get outgoing node #{})
              [indegree queue]
              (reduce (fn [[ind q] dep]
                        (let [next-count (dec (get ind dep 0))
                              ind (assoc ind dep next-count)
                              q (if (zero? next-count)
                                  (conj q dep)
                                  q)]
                          [ind q]))
                      [indegree queue]
                      deps)]
          (recur queue indegree (conj ordered node)))))))

(defn- dag?
  [nodes edges]
  (= (count nodes) (count (topological-order nodes edges))))

(defn- strongly-connected-components
  [nodes edges]
  (let [adj (outgoing-map nodes edges)
        idx* (atom 0)
        stack* (atom [])
        on-stack* (atom #{})
        index* (atom {})
        low* (atom {})
        components* (atom [])]
    (letfn [(strong-connect [v]
              (let [idx @idx*]
                (swap! idx* inc)
                (swap! index* assoc v idx)
                (swap! low* assoc v idx)
                (swap! stack* conj v)
                (swap! on-stack* conj v)
                (doseq [w (sort (get adj v #{}))]
                  (if-not (contains? @index* w)
                    (do
                      (strong-connect w)
                      (swap! low* assoc v (min (get @low* v) (get @low* w))))
                    (when (contains? @on-stack* w)
                      (swap! low* assoc v (min (get @low* v) (get @index* w))))))
                (when (= (get @low* v) (get @index* v))
                  (loop [component #{}]
                    (let [w (peek @stack*)]
                      (swap! stack* pop)
                      (swap! on-stack* disj w)
                      (let [component (conj component w)]
                        (if (= w v)
                          (swap! components* conj component)
                          (recur component))))))))]
      (doseq [node (sort nodes)]
        (when-not (contains? @index* node)
          (strong-connect node))))
    @components*))

(defn- self-loop?
  [component edges]
  (some (fn [{:keys [from to]}]
          (and (= from to) (contains? component from)))
        edges))

(defn- cyclic-component?
  [component edges]
  (or (> (count component) 1)
      (self-loop? component edges)))

(defn- choose-k
  [v k]
  (cond
    (zero? k) [[]]
    (> k (count v)) []
    :else
    (let [head (first v)
          tail (subvec v 1)]
      (concat (map (fn [rest-choice] (cons head rest-choice))
                   (choose-k tail (dec k)))
              (choose-k tail k)))))

(defn- exact-feedback-edges
  [nodes edges]
  (let [edge-vec (vec edges)]
    (some (fn [k]
            (some (fn [subset]
                    (let [removed (set subset)
                          remaining (remove removed edge-vec)]
                      (when (dag? nodes remaining)
                        removed)))
                  (choose-k edge-vec k)))
          (range 0 (inc (count edge-vec))))))

(defn- remove-node
  [edges node]
  (->> edges
       (remove (fn [{:keys [from to]}]
                 (or (= from node) (= to node))))
       set))

(defn- source-node
  [remaining in-map]
  (->> remaining
       (filter #(empty? (get in-map % #{})))
       sort
       first))

(defn- sink-node
  [remaining out-map]
  (->> remaining
       (filter #(empty? (get out-map % #{})))
       sort
       first))

(defn- cycle-break-node
  [remaining in-map out-map]
  (->> remaining
       (sort-by (fn [n]
                  [(- (count (get out-map n #{}))
                      (count (get in-map n #{})))
                   (str n)]))
       last))

(defn- node-choice
  [node side]
  (when node
    {:node node :side side}))

(defn- choose-next-node
  [remaining in-map out-map]
  (or (node-choice (source-node remaining in-map) :left)
      (node-choice (sink-node remaining out-map) :right)
      {:node (cycle-break-node remaining in-map out-map)
       :side :left}))

(defn- place-node
  [left right side node]
  (get {:right [left (conj right node)]
        :left [(conj left node) right]}
       side
       [(conj left node) right]))

(defn- greedy-step
  [{:keys [remaining active-edges left right] :as state}]
  (if (empty? remaining)
    state
    (let [in-map (incoming-map remaining active-edges)
          out-map (outgoing-map remaining active-edges)
          {:keys [node side]} (choose-next-node remaining in-map out-map)
          [next-left next-right] (place-node left right side node)]
      {:remaining (disj remaining node)
       :active-edges (remove-node active-edges node)
       :left next-left
       :right next-right})))

(defn- greedy-order
  [nodes edges]
  (let [{:keys [left right]}
        (->> {:remaining (set nodes)
              :active-edges (set edges)
              :left []
              :right []}
             (iterate greedy-step)
             (drop-while (comp seq :remaining))
             first)]
    (vec (concat left (reverse right)))))

(defn- heuristic-feedback-edges
  [nodes edges]
  (let [order (greedy-order nodes edges)
        idx (into {} (map-indexed (fn [i n] [n i]) order))]
    (->> edges
         (filter (fn [{:keys [from to]}]
                   (>= (get idx from -1)
                       (get idx to -1))))
         set)))

(defn- component-internal-edges
  [component edges]
  (->> edges
       (filter (fn [{:keys [from to]}]
                 (and (contains? component from)
                      (contains? component to))))
       set))

(defn- component-feedback-edges
  [component edges]
  (let [internal (component-internal-edges component edges)]
    (if-not (cyclic-component? component internal)
      #{}
      (let [exact? (and (<= (count component) exact-feedback-max-nodes)
                        (<= (count internal) exact-feedback-max-edges))]
        (if exact?
          (or (exact-feedback-edges component internal) #{})
          (heuristic-feedback-edges component internal))))))

(defn- feedback-edge-set
  [nodes edges]
  (let [components (strongly-connected-components nodes edges)]
    (reduce set/union #{}
            (map #(component-feedback-edges % edges) components))))

(defn- topological-levels
  [nodes edges]
  (let [outgoing (outgoing-map nodes edges)
        incoming (incoming-map nodes edges)
        indegree (indegree-map nodes edges)]
    (loop [queue (into (sorted-set) (for [[n d] indegree :when (zero? d)] n))
           indegree indegree
           levels (zipmap nodes (repeat 1))]
      (if (empty? queue)
        levels
        (let [node (first queue)
              queue (disj queue node)
              node-level (get levels node 1)
              deps (get outgoing node #{})
              [indegree levels queue]
              (reduce (fn [[ind lvl q] dep]
                        (let [next-level (inc node-level)
                              lvl (update lvl dep (fn [existing]
                                                    (max (or existing 1) next-level)))
                              next-count (dec (get ind dep 0))
                              ind (assoc ind dep next-count)
                              q (if (zero? next-count)
                                  (conj q dep)
                                  q)]
                          [ind lvl q]))
                      [indegree levels queue]
                      deps)
              roots (get incoming node #{})
              levels (if (seq roots)
                       (assoc levels node (max (get levels node 1)
                                               (inc (apply max (map #(get levels % 1) roots)))))
                       levels)]
          (recur queue indegree levels))))))

(defn assign-layers
  [{:keys [nodes edges]}]
  (let [nodes (set nodes)
        edges (normalize-edges nodes edges)
        feedback-edges (feedback-edge-set nodes edges)
        acyclic-edges (->> edges (remove feedback-edges) set)
        module->level (topological-levels nodes acyclic-edges)
        module->layer (into {}
                            (for [n (sort nodes)]
                              [n (dec (get module->level n 1))]))
        layers (->> module->layer
                    (group-by val)
                    (sort-by key)
                    (mapv (fn [[idx pairs]]
                            {:index idx
                             :modules (->> pairs (map key) sort vec)})))]
    {:layers layers
     :module->layer module->layer
     :module->level module->level
     :feedback-edges feedback-edges
     :acyclic-edges acyclic-edges}))
