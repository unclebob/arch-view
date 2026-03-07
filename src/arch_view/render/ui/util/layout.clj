(ns arch-view.render.ui.util.layout
  (:require [arch-view.render.ui.util.labels :as labels]))

(def ^:private scene-top-padding 42.0)
(def ^:private racetrack-count 5)
(def ^:private racetrack-margin 24.0)
(def ^:private racetrack-gap 24.0)

(def default-slot-scoring
  {:height-weight 100.0
   :horizontal-weight 9.0
   :vertical-weight 2.0
   :upward-shape-penalty 18.0
   :upward-count-penalty 140.0
   :crossing-penalty 180.0
   :center-penalty 30.0})

(defn layer-y
  [index layer-height layer-gap]
  (+ scene-top-padding
     (* index (+ layer-height layer-gap))))

(defn dominant-component
  [modules module->component]
  (->> modules
       (map module->component)
       (remove nil?)
       frequencies
       (sort-by (juxt (comp - val) (comp str key)))
       ffirst))

(defn module-positions-for-layer
  [layer-index modules layer-rect module->kind module->full-name]
  (let [count-modules (count modules)
        spacing (/ (:width layer-rect) (inc count-modules))
        y (+ (:y layer-rect) (/ (:height layer-rect) 2))]
    (map-indexed (fn [idx module]
                   {:module module
                    :layer layer-index
                    :x (+ (:x layer-rect) (* (inc idx) spacing))
                    :y y
                    :kind (or (get module->kind module) :concrete)
                    :full-name (or (get module->full-name module) module)})
                 modules)))

(defn arrowhead-for
  [edge-type]
  (if (= :abstract edge-type)
    :closed-triangle
    :standard))

(defn track-width-for
  [canvas-width]
  (/ (- (double canvas-width)
        (* 2.0 racetrack-margin)
        (* (dec racetrack-count) racetrack-gap))
     racetrack-count))

(defn track-x-for
  [track canvas-width]
  (+ racetrack-margin
     (* track (+ (track-width-for canvas-width) racetrack-gap))))

(defn dependency-pairs-by-layer
  [classified-edges module->layer]
  (->> classified-edges
       (keep (fn [{:keys [from to]}]
               (let [from-layer (get module->layer from)
                     to-layer (get module->layer to)]
                 (when (and (number? from-layer)
                            (number? to-layer))
                   [from-layer to-layer]))))
       vec))

(defn incoming-counts-by-layer
  [layer-indexes layer-pairs]
  (reduce (fn [acc [_ to-layer]]
            (if (contains? acc to-layer)
              (update acc to-layer inc)
              acc))
          (zipmap layer-indexes (repeat 0))
          (filter (fn [[from-layer to-layer]]
                    (not= from-layer to-layer))
                  layer-pairs)))

(defn edge-point
  [placement node]
  (when-let [{:keys [track row]} (get placement node)]
    [(double track) (double row)]))

(defn orientation
  [[ax ay] [bx by] [cx cy]]
  (- (* (- bx ax) (- cy ay))
     (* (- by ay) (- cx ax))))

(defn segment-crosses?
  [p1 p2 q1 q2]
  (let [o1 (orientation p1 p2 q1)
        o2 (orientation p1 p2 q2)
        o3 (orientation q1 q2 p1)
        o4 (orientation q1 q2 p2)
        eps 1.0e-9]
    (and (< (* o1 o2) (- eps))
         (< (* o3 o4) (- eps)))))

(defn edge-cross?
  [placement [a b] [c d]]
  (and (not-any? #{a b} [c d])
       (let [p1 (edge-point placement a)
             p2 (edge-point placement b)
             q1 (edge-point placement c)
             q2 (edge-point placement d)]
         (and p1 p2 q1 q2
              (segment-crosses? p1 p2 q1 q2)))))

(defn- count->min-row-map
  [ordered-layer-indexes incoming-counts]
  (let [unique-counts (->> ordered-layer-indexes
                           (map #(get incoming-counts % 0))
                           distinct
                           sort
                           vec)]
    (into {} (map-indexed (fn [idx c] [c idx]) unique-counts))))

(defn- slot-candidates
  [placement min-row row-limit]
  (for [row-candidate (range min-row row-limit)
        track-candidate (range racetrack-count)
        :let [occupied? (some (fn [{:keys [row track]}]
                                (and (= row row-candidate)
                                     (= track track-candidate)))
                              (vals placement))]
        :when (not occupied?)]
    {:row row-candidate :track track-candidate}))

(defn- connected-placement
  [pair-index reverse-index placement idx]
  (let [outgoing (for [[_ to] (get pair-index idx)
                       :let [p (get placement to)]
                       :when p]
                   [p true])
        incoming (for [[from _] (get reverse-index idx)
                       :let [p (get placement from)]
                       :when p]
                   [p false])]
    (concat outgoing incoming)))

(defn- edge-shape-cost
  [connections row track {:keys [horizontal-weight vertical-weight upward-shape-penalty]}]
  (reduce (fn [cost [{other-row :row other-track :track} as-from?]]
            (let [horizontal (Math/abs (double (- track other-track)))
                  vertical (Math/abs (double (- row other-row)))
                  upward? (if as-from?
                            (< other-row row)
                            (< row other-row))]
              (+ cost
                 (* horizontal-weight horizontal)
                 (* vertical-weight vertical)
                 (if upward? upward-shape-penalty 0.0))))
          0.0
          connections))

(defn- upward-cost
  [connections row {:keys [upward-count-penalty]}]
  (let [upward-count (count (filter true?
                                    (map (fn [[{other-row :row} as-from?]]
                                           (if as-from?
                                             (< other-row row)
                                             (< row other-row)))
                                         connections)))]
    (* upward-count-penalty (double upward-count))))

(defn- placed-edges
  [pairs placement]
  (->> pairs
       (filter (fn [[from to]]
                 (and (contains? placement from)
                      (contains? placement to))))
       vec))

(defn- new-edges
  [pairs placement idx]
  (->> pairs
       (filter (fn [[from to]]
                 (or (and (= from idx) (contains? placement to))
                     (and (= to idx) (contains? placement from)))))
       vec))

(defn- crossing-cost
  [pairs placement idx row track {:keys [crossing-penalty]}]
  (let [candidate-placement {:row row :track track}
        placement* (assoc placement idx candidate-placement)
        placed (placed-edges pairs placement)
        new (new-edges pairs placement idx)
        crossing-count (count (for [new-edge new
                                    placed-edge placed
                                    :when (edge-cross? placement* new-edge placed-edge)]
                                true))]
    (* crossing-penalty (double crossing-count))))

(defn- center-cost
  [incoming-counts idx track {:keys [center-penalty]}]
  (let [fan-in (double (get incoming-counts idx 0))
        center-track (/ (double (dec racetrack-count)) 2.0)
        center-distance (Math/abs (- (double track) center-track))]
    (* fan-in center-penalty center-distance)))

(defn- slot-score
  [pair-index reverse-index pairs placement incoming-counts idx {:keys [row track]} scoring]
  (let [connections (connected-placement pair-index reverse-index placement idx)
        height-cost (* (:height-weight scoring) row)]
    (+ height-cost
       (edge-shape-cost connections row track scoring)
       (upward-cost connections row scoring)
       (crossing-cost pairs placement idx row track scoring)
       (center-cost incoming-counts idx track scoring))))

(defn- best-slot
  [pair-index reverse-index pairs placement incoming-counts idx candidates scoring]
  (reduce (fn [best candidate]
            (let [score (slot-score pair-index reverse-index pairs placement incoming-counts idx candidate scoring)]
              (if (or (nil? best) (< score (:score best)))
                (assoc candidate :score score)
                best)))
          nil
          candidates))

(defn assign-layer-slots
  ([ordered-layer-indexes pairs incoming-counts]
   (assign-layer-slots ordered-layer-indexes pairs incoming-counts default-slot-scoring))
  ([ordered-layer-indexes pairs incoming-counts scoring]
   (let [pair-index (group-by first pairs)
         reverse-index (group-by second pairs)
         count->min-row (count->min-row-map ordered-layer-indexes incoming-counts)]
     (loop [remaining ordered-layer-indexes
            placement {}
            max-row -1]
       (if (empty? remaining)
         placement
         (let [idx (first remaining)
               min-row (get count->min-row (get incoming-counts idx 0) 0)
               row-limit (inc (max (+ 2 max-row)
                                   (+ 2 min-row)))
               candidates (slot-candidates placement min-row row-limit)
               best (best-slot pair-index reverse-index pairs placement incoming-counts idx candidates scoring)
               next-placement (assoc placement idx {:row (:row best) :track (:track best)})]
           (recur (rest remaining)
                  next-placement
                  (max max-row (:row best)))))))))

(def ^:private architecture-map-keys
  [:module->component
   :layer->label
   :module->kind
   :module->leaf?
   :module->source-file
   :module->full-name
   :module->display-label])

(defn- extract-architecture-maps
  [architecture]
  (reduce (fn [acc k]
            (assoc acc k (get architecture k {})))
          {}
          architecture-map-keys))

(defn- compute-layer-placement
  [architecture]
  (let [raw-layers (get-in architecture [:layout :layers])
        ordered-levels (->> raw-layers (map :index) distinct sort vec)
        level->row (into {}
                         (map-indexed (fn [row idx] [idx row]) ordered-levels))
        expanded-layers
        (->> raw-layers
             (sort-by :index)
             (mapcat (fn [{:keys [index modules]}]
                       (let [ordered-modules (->> modules sort vec)
                             peer-count (count ordered-modules)]
                         (map-indexed (fn [peer-idx module]
                                        {:index [index module]
                                         :level index
                                         :modules [module]
                                         :peer-idx peer-idx
                                         :peer-count (max 1 peer-count)})
                                      ordered-modules))))
             vec)
        row-by-layer-index (into {}
                                 (map (fn [{:keys [index level]}]
                                        [index (get level->row level 0)])
                                      expanded-layers))
        peer-by-layer-index (into {}
                                  (map (fn [{:keys [index peer-idx peer-count]}]
                                         [index {:peer-idx peer-idx
                                                 :peer-count peer-count}])
                                       expanded-layers))]
    {:layers expanded-layers
     :layer-by-index (into {} (map (juxt :index identity) expanded-layers))
     :row-by-layer-index row-by-layer-index
     :peer-by-layer-index peer-by-layer-index}))

(defn- scene-inputs
  [architecture]
  (merge (compute-layer-placement architecture)
         (extract-architecture-maps architecture)))

(defn- layer-rect-geometry
  [canvas-width layer-height]
  (let [rect-scale 0.5
        rect-height (* rect-scale layer-height)
        track-width (track-width-for canvas-width)
        rect-width (* rect-scale track-width)]
    {:rect-width rect-width
     :rect-height rect-height
     :rect-x-offset (/ (- track-width rect-width) 2.0)
     :rect-y-offset (/ (- layer-height rect-height) 2.0)}))

(defn- centered-peer-x
  [canvas-width rect-width peer-idx peer-count]
  (let [center-x (/ (double canvas-width) 2.0)
        spacing (* rect-width 1.5)
        group-width (+ rect-width (* (double (dec (max 1 peer-count))) spacing))
        group-start (- center-x (/ group-width 2.0))]
    (+ group-start (* (double peer-idx) spacing))))

(defn- layer-rect-entry
  [layer-by-index layer row-by-layer-index peer-by-layer-index module->component module->kind module->full-name layer->label
   canvas-width geometry]
  (let [{:keys [index]} layer
        modules (:modules (get layer-by-index index))
        component (dominant-component modules module->component)
        abstract-layer? (boolean (some #(= :abstract (get module->kind %)) modules))
        {:keys [rect-width rect-height]} geometry
        row (double (get row-by-layer-index index 0))
        {:keys [peer-idx peer-count]} (get peer-by-layer-index index {:peer-idx 0 :peer-count 1})]
    {:index index
     :x (centered-peer-x canvas-width rect-width peer-idx peer-count)
     :y (+ scene-top-padding (* row (* rect-height 1.5)))
     :width rect-width
     :height rect-height
     :abstract? abstract-layer?
     :full-name (some-> modules first module->full-name labels/strip-top-namespace)
     :label (or (get layer->label index)
                (some-> modules first labels/abbreviate-module-name)
                (if component (name component) (str "layer-" index)))}))

(defn- decorate-module-position
  [m module->leaf? module->source-file module->display-label]
  (assoc m
         :leaf? (boolean (get module->leaf? (:module m)))
         :source-file (get module->source-file (:module m))
         :label (or (get module->display-label (:module m))
                    (labels/abbreviate-module-name (:module m)))
         :full-name (labels/strip-top-namespace (:full-name m))))

(defn- scene-module-positions
  [layers rect-by-layer module->kind module->full-name module->leaf? module->source-file module->display-label]
  (->> layers
       (mapcat (fn [{:keys [index modules]}]
                 (->> (module-positions-for-layer index modules (get rect-by-layer index) module->kind module->full-name)
                      (map (fn [m]
                             (decorate-module-position m module->leaf? module->source-file module->display-label)))
                      labels/apply-layer-stagger)))
       vec))

(defn- scene-edge-drawables
  [architecture]
  (let [feedback-pairs (->> (get-in architecture [:layout :feedback-edges] #{})
                            (map (juxt :from :to))
                            set)]
    (->> (:classified-edges architecture)
       (map (fn [{:keys [from to type count]}]
              {:from from
               :to to
               :count (long (or count 1))
               :arrowhead (arrowhead-for type)
               :type type
               :cycle-break? (contains? feedback-pairs [from to])}))
       vec)))

(defn build-scene
  ([architecture]
   (build-scene architecture {}))
  ([architecture {:keys [canvas-width layer-height layer-gap]
                  :or {canvas-width 1200 layer-height 140 layer-gap 24}}]
   (let [{:keys [layers layer-by-index row-by-layer-index peer-by-layer-index module->component layer->label module->kind
                 module->leaf? module->source-file module->full-name module->display-label]}
         (scene-inputs architecture)
         geometry (layer-rect-geometry canvas-width layer-height)
         layer-rects (mapv (fn [layer]
                             (layer-rect-entry layer-by-index layer row-by-layer-index peer-by-layer-index
                                               module->component module->kind module->full-name layer->label
                                               canvas-width geometry))
                           layers)
         rect-by-layer (into {} (map (juxt :index identity) layer-rects))]
     {:layer-rects layer-rects
      :module-positions (scene-module-positions layers rect-by-layer module->kind module->full-name
                                                module->leaf? module->source-file module->display-label)
      :edge-drawables (scene-edge-drawables architecture)})))
