;; mutation-tested: 2026-03-08
(ns arch-view.domain.architecture-projection
  (:require [clojure.string :as str]
            [arch-view.layout.layers :as layers]))

(def ^:private mixed-leaf-suffix "|file")

(defn namespace-segments
  [module]
  (vec (rest (str/split module #"\."))))

(defn source-filename
  [path]
  (when path
    (let [normalized (-> (str path)
                         (str/replace #"[\\]+" "/")
                         (str/replace #"/+$" ""))]
      (when-not (str/blank? normalized)
        (last (str/split normalized #"/"))))))

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

(defn- graph-has-cycle?
  [nodes edges]
  (let [nodes (set (or nodes #{}))
        indegree (reduce (fn [acc {:keys [to]}]
                           (update acc to (fnil inc 0)))
                         (zipmap nodes (repeat 0))
                         edges)
        outgoing (reduce (fn [acc {:keys [from to]}]
                           (update acc from (fnil conj #{}) to))
                         {}
                         edges)]
    (loop [queue (into clojure.lang.PersistentQueue/EMPTY
                       (filter #(zero? (get indegree % 0)) nodes))
           indeg indegree
           seen 0]
      (if (empty? queue)
        (< seen (count nodes))
        (let [n (peek queue)
              queue' (pop queue)
              nbrs (get outgoing n #{})
              [indeg' queue'']
              (reduce (fn [[m q] nbr]
                        (let [next (dec (get m nbr 0))
                              m' (assoc m nbr next)]
                          (if (zero? next)
                            [m' (conj q nbr)]
                            [m' q])))
                      [indeg queue']
                      nbrs)]
          (recur queue'' indeg' (inc seen)))))))

(defn- subtree-cycle-detector
  [all-modules classified]
  (let [memo (atom {})]
    (letfn [(subtree-cycle? [namespace-path]
              (if (contains? @memo namespace-path)
                (get @memo namespace-path)
                (let [scoped (scoped-modules all-modules namespace-path)
                      module->child-map (module->child scoped namespace-path)
                      children (set (vals module->child-map))
                      level-edges (->> classified
                                       (keep (fn [{:keys [from to]}]
                                               (let [from-child (get module->child-map from)
                                                     to-child (get module->child-map to)]
                                                 (when (and from-child to-child
                                                            (not= from-child to-child))
                                                   {:from from-child :to to-child}))))
                                       set)
                      level-cycle? (graph-has-cycle? children level-edges)
                      descendant-cycle? (some true?
                                              (for [child children
                                                    :let [child-path (conj (vec namespace-path) child)
                                                          deeper? (some (fn [m]
                                                                          (> (count (namespace-segments m))
                                                                             (count child-path)))
                                                                        scoped)]
                                                    :when deeper?]
                                                (subtree-cycle? child-path)))
                      result (boolean (or level-cycle? descendant-cycle?))]
                  (swap! memo assoc namespace-path result)
                  result)))]
      subtree-cycle?)))

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

(defn- format-cycle-line
  [namespace-path cycle]
  (->> cycle
       (map (fn [node]
              (str/join "." (concat namespace-path [(node-child-name node)]))))
       (str/join "->")))

(defn- subtree-cycle-lines
  [all-modules classified namespace-path]
  (let [scoped (scoped-modules all-modules namespace-path)
        module->child-map (module->child scoped namespace-path)
        children (set (vals module->child-map))
        level-edges (->> classified
                         (keep (fn [{:keys [from to]}]
                                 (let [from-child (get module->child-map from)
                                       to-child (get module->child-map to)]
                                   (when (and from-child to-child
                                              (not= from-child to-child))
                                     {:from from-child :to to-child}))))
                         set)
        level-cycles (get (layers/assign-layers {:nodes children :edges level-edges}) :cycles [])
        descendant-cycles (mapcat (fn [child]
                                    (let [child-path (conj (vec namespace-path) child)
                                          deeper? (some (fn [m]
                                                          (> (count (namespace-segments m))
                                                             (count child-path)))
                                                        scoped)]
                                      (when deeper?
                                        (subtree-cycle-lines all-modules classified child-path))))
                                  (sort children))]
    (->> (concat (map #(format-cycle-line namespace-path %) level-cycles)
                 descendant-cycles)
         distinct
         vec)))

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
        subtree-cycle? (subtree-cycle-detector all-modules (:classified-edges architecture))
        scoped (scoped-modules all-modules namespace-path)
        module->child-map (module->child scoped namespace-path)
        by-child (modules-by-child scoped module->child-map)
        child->info-map (child->info by-child namespace-path)
        module->node-map (module->node child->info-map)
        node->modules-map (node->modules module->node-map)
        nodes (set (keys node->modules-map))
        abstract-source (or (get-in architecture [:graph :abstract-modules]) #{})
        module->kind (node-kind-map nodes node->modules-map abstract-source)
        module->cycle? (into {}
                             (for [n nodes]
                               [n (subtree-cycle? (conj (vec namespace-path) (node-child-name n)))]))
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
        cycle-lines (subtree-cycle-lines all-modules (:classified-edges architecture) namespace-path)
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
     :module->full-name module->full-name
     :cycle-lines cycle-lines}))
