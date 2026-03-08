(ns arch-view.domain.architecture-projection
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [arch-view.layout.layers :as layers]))

(def ^:private mixed-leaf-suffix "|file")

(defn namespace-segments
  [module]
  (vec (rest (str/split module #"\."))))

(defn source-filename
  [path]
  (when path
    (.getName (io/file path))))

(defn- source-filename-base
  [path]
  (some-> (source-filename path)
          (str/replace #"\.[^.]+$" "")))

(defn mixed-leaf-node-id
  [child]
  (str child mixed-leaf-suffix))

(defn mixed-leaf-node?
  [node]
  (and (string? node)
       (str/ends-with? node mixed-leaf-suffix)))

(defn node-child-name
  [node]
  (if (mixed-leaf-node? node)
    (subs node 0 (- (count node) (count mixed-leaf-suffix)))
    node))

(defn prefix?
  [prefix parts]
  (and (<= (count prefix) (count parts))
       (= prefix (subvec parts 0 (count prefix)))))

(defn sink-first-order
  [nodes edges]
  (let [incoming (reduce (fn [acc {:keys [from to]}]
                           (update acc to (fnil conj #{}) from))
                         {}
                         edges)
        outdeg (merge (zipmap nodes (repeat 0))
                      (frequencies (map :from edges)))]
    (loop [remaining (set nodes)
           degree outdeg
           available (into (sorted-set)
                           (filter #(zero? (get degree % 0)) nodes))
           ordered []]
      (if (empty? remaining)
        ordered
        (let [n (if (seq available) (first available) (first (sort remaining)))
              next-remaining (disj remaining n)
              next-available (disj available n)
              [next-degree next-available]
              (reduce (fn [[deg avail] m]
                        (let [new-count (dec (get deg m 0))
                              next-deg (assoc deg m (max 0 new-count))
                              next-avail (if (and (contains? next-remaining m)
                                                  (zero? new-count))
                                           (conj avail m)
                                           avail)]
                          [next-deg next-avail]))
                      [degree next-available]
                      (get incoming n #{}))]
          (recur next-remaining
                 next-degree
                 next-available
                 (conj ordered n)))))))

(defn edge-layout-cost
  [order edges]
  (let [idx (into {} (map-indexed vector order))
        up-penalty 6.0]
    (reduce (fn [cost {:keys [from to]}]
              (let [from-idx (double (get idx from 0))
                    to-idx (double (get idx to 0))
                    distance (Math/abs (- to-idx from-idx))
                    upward? (<= to-idx from-idx)]
                (+ cost distance (if upward? up-penalty 0.0))))
            0.0
            edges)))

(defn optimize-order
  [initial-order edges]
  (loop [order (vec initial-order)]
    (let [base-cost (edge-layout-cost order edges)
          swaps (for [i (range (dec (count order)))]
                  (let [swapped (-> order
                                    (assoc i (nth order (inc i)))
                                    (assoc (inc i) (nth order i)))
                        swapped-cost (edge-layout-cost swapped edges)]
                    {:order swapped :cost swapped-cost}))]
      (if-let [{:keys [order]}
               (->> swaps
                    (filter #(< (:cost %) base-cost))
                    first)]
        (recur order)
        order))))

(defn namespace-layout
  [nodes edges]
  (layers/assign-layers {:nodes nodes :edges edges}))

(defn- scoped-modules
  [all-modules namespace-path]
  (->> all-modules
       (filter (fn [m]
                 (let [parts (namespace-segments m)]
                   (and (prefix? namespace-path parts)
                        (> (count parts) (count namespace-path))))))
       set))

(defn- module->child
  [scoped namespace-path]
  (into {}
        (for [m scoped
              :let [parts (namespace-segments m)]]
          [m (nth parts (count namespace-path))])))

(defn- modules-by-child
  [scoped module->child-map]
  (reduce (fn [acc module]
            (update acc (get module->child-map module) (fnil conj #{}) module))
          {}
          scoped))

(defn- child->info
  [modules-by-child namespace-path]
  (into {}
        (for [[child modules] modules-by-child
              :let [exact-module (some (fn [m]
                                         (when (= (namespace-segments m)
                                                  (conj (vec namespace-path) child))
                                           m))
                                       modules)
                    descendant-modules (if exact-module (disj modules exact-module) modules)]]
          [child {:exact-module exact-module
                  :descendant-modules descendant-modules}])))

(defn- module->node
  [child->info-map]
  (reduce (fn [acc [child {:keys [exact-module descendant-modules]}]]
            (let [leaf-node (if (seq descendant-modules)
                              (mixed-leaf-node-id child)
                              child)
                  with-descendants (reduce (fn [inner module]
                                             (assoc inner module child))
                                           acc
                                           descendant-modules)]
              (if exact-module
                (assoc with-descendants exact-module leaf-node)
                with-descendants)))
          {}
          child->info-map))

(defn- node->modules
  [module->node-map]
  (reduce (fn [acc [module node]]
            (update acc node (fnil conj #{}) module))
          {}
          module->node-map))

(defn- node-kind-map
  [nodes node->modules-map abstract-source]
  (into {}
        (for [n nodes]
          [n (if (some #(contains? abstract-source %)
                       (get node->modules-map n))
               :abstract
               :concrete)])))

(defn- node-cycle-map
  [nodes node->modules-map cycle-modules]
  (into {}
        (for [n nodes]
          [n (boolean (some #(contains? cycle-modules %)
                            (get node->modules-map n)))])))

(defn- node-leaf-map
  [nodes node->modules-map namespace-path]
  (into {}
        (for [n nodes
              :let [modules (get node->modules-map n)
                    leaf? (every? (fn [m]
                                    (= (count (namespace-segments m))
                                       (inc (count namespace-path))))
                                  modules)]]
          [n leaf?])))

(defn- node-source-file-map
  [nodes node->modules-map module->source-file-all namespace-path]
  (into {}
        (for [n nodes
              :let [modules (get node->modules-map n)
                    exact-module (some (fn [m]
                                         (when (= (count (namespace-segments m))
                                                  (inc (count namespace-path)))
                                           m))
                                       modules)
                    source-file (get module->source-file-all exact-module)]]
          [n source-file])))

(defn- node-display-label-map
  [nodes node-leaf-map node-source-file-map]
  (into {}
        (for [n nodes
              :let [leaf? (true? (get node-leaf-map n))
                    source-file (get node-source-file-map n)]]
          [n (if leaf?
               (or (source-filename-base source-file)
                   (node-child-name n))
               (node-child-name n))])))

(defn- node-full-name-map
  [nodes node-leaf-map node-source-file-map namespace-path]
  (into {}
        (for [n nodes]
          [n (if-let [source-file (and (true? (get node-leaf-map n))
                                       (get node-source-file-map n))]
               (str/join "." (concat namespace-path [(source-filename source-file)]))
               (str/join "." (concat namespace-path [(node-child-name n)])))])))

(defn- aggregate-edge
  [old type]
  (if old
    {:type (if (or (= :abstract (:type old))
                   (= type :abstract))
             :abstract
             :direct)
     :count (inc (long (:count old)))}
    {:type type :count 1}))

(defn- edges-by-pair
  [classified module->node-map]
  (reduce (fn [acc {:keys [from to type]}]
            (let [f (get module->node-map from)
                  t (get module->node-map to)]
              (if (and f t (not= (node-child-name f) (node-child-name t)))
                (update acc [f t] #(aggregate-edge % type))
                acc)))
          {}
          classified))

(defn- display-edges-by-pair
  [classified scoped module->node-map]
  (reduce (fn [acc {:keys [from to type]}]
            (let [from-in? (contains? scoped from)
                  to-in? (contains? scoped to)]
              (if (or from-in? to-in?)
                (let [f (if from-in? (get module->node-map from) from)
                      t (if to-in? (get module->node-map to) to)]
                  (if (or (nil? f) (nil? t) (= f t))
                    acc
                    (update acc [f t] #(aggregate-edge % type))))
                acc)))
          {}
          classified))

(defn view-architecture
  [architecture namespace-path]
  (let [all-modules (or (get-in architecture [:graph :nodes]) #{})
        module->source-file-all (or (get-in architecture [:graph :module->source-file]) {})
        cycle-modules (->> (or (get-in architecture [:layout :feedback-edges]) #{})
                           (mapcat (juxt :from :to))
                           set)
        scoped (scoped-modules all-modules namespace-path)
        module->child-map (module->child scoped namespace-path)
        by-child (modules-by-child scoped module->child-map)
        child->info-map (child->info by-child namespace-path)
        module->node-map (module->node child->info-map)
        node->modules-map (node->modules module->node-map)
        nodes (set (keys node->modules-map))
        abstract-source (or (get-in architecture [:graph :abstract-modules]) #{})
        module->kind (node-kind-map nodes node->modules-map abstract-source)
        module->cycle? (node-cycle-map nodes node->modules-map cycle-modules)
        module->leaf? (node-leaf-map nodes node->modules-map namespace-path)
        module->source-file (node-source-file-map nodes node->modules-map module->source-file-all namespace-path)
        module->display-label (node-display-label-map nodes module->leaf? module->source-file)
        module->full-name (node-full-name-map nodes module->leaf? module->source-file namespace-path)
        pairs (edges-by-pair (:classified-edges architecture) module->node-map)
        display-pairs (display-edges-by-pair (:classified-edges architecture) scoped module->node-map)
        classified-edges (set (for [[[f t] {:keys [type count]}] pairs]
                                {:from f :to t :type type :count count}))
        display-edges (set (for [[[f t] {:keys [type count]}] display-pairs]
                             {:from f :to t :type type :count count}))
        graph {:nodes nodes
               :edges (set (for [{:keys [from to]} classified-edges] {:from from :to to}))
               :abstract-modules (set (for [[n k] module->kind :when (= k :abstract)] n))}
        layout (namespace-layout nodes classified-edges)
        layer->label (into {}
                           (for [[module idx] (:module->layer layout)]
                             [idx (or (get module->display-label module) module)]))]
    {:namespace-path namespace-path
     :graph graph
     :layout layout
     :layer->label layer->label
     :classified-edges classified-edges
     :display-edges display-edges
     :module->kind module->kind
     :module->cycle? module->cycle?
     :module->leaf? module->leaf?
     :module->source-file module->source-file
     :module->display-label module->display-label
     :module->full-name module->full-name}))
