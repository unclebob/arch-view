(ns arch-view.render.ui.util.routing
  (:require [arch-view.render.ui.util.layout :as layout]))

(defn clamp-between
  [v lo hi]
  (-> v double (max lo) (min hi)))

(defn rect-center
  [{:keys [x y width height]}]
  [(+ x (/ width 2.0))
   (+ y (/ height 2.0))])

(defn rect-edge-anchor
  [{:keys [x y width height] :as rect} tx ty]
  (let [[cx cy] (rect-center rect)
        dx (- tx cx)
        dy (- ty cy)
        corner-inset 10.0
        x-lo (+ x corner-inset)
        x-hi (- (+ x width) corner-inset)
        y-lo (+ y corner-inset)
        y-hi (- (+ y height) corner-inset)
        right (+ x width)
        bottom (+ y height)]
    (if (>= (Math/abs dx) (Math/abs dy))
      (if (>= dx 0.0)
        {:point [right (clamp-between ty y-lo y-hi)]
         :side :right}
        {:point [x (clamp-between ty y-lo y-hi)]
         :side :left})
      (if (>= dy 0.0)
        {:point [(clamp-between tx x-lo x-hi) bottom]
         :side :bottom}
        {:point [(clamp-between tx x-lo x-hi) y]
         :side :top}))))

(defn layer-center-map
  [module-positions]
  (reduce (fn [acc {:keys [layer x y]}]
            (update acc layer (fnil conj []) [x y]))
          {}
          module-positions))

(defn layer-center-at
  [layer->centers layer]
  (let [points (get layer->centers layer)]
    (when (seq points)
      (let [count-points (double (count points))
            sum-x (reduce + (map first points))
            sum-y (reduce + (map second points))]
        [(/ sum-x count-points) (/ sum-y count-points)]))))

(defn aggregate-layer-edges
  [scene module->layer]
  (reduce (fn [acc {:keys [from to type]}]
            (let [from-layer (get module->layer from)
                  to-layer (get module->layer to)]
              (if (and (number? from-layer)
                       (number? to-layer)
                       (not= from-layer to-layer))
                (update acc [from-layer to-layer]
                        (fn [existing]
                          (if (or (= existing :abstract) (= type :abstract))
                            :abstract
                            :direct)))
                acc)))
          {}
          (:edge-drawables scene)))

(defn make-layer-base-edge
  [layer-rects layer->centers [from-layer to-layer] type]
  (let [{fx :x fy :y fw :width fh :height} (get layer-rects from-layer)
        {tx :x ty :y tw :width th :height} (get layer-rects to-layer)
        [from-x from-y] (or (layer-center-at layer->centers from-layer)
                            [(+ fx (/ fw 2.0))
                             (+ fy (/ fh 2.0))])
        [to-x to-y] (or (layer-center-at layer->centers to-layer)
                        [(+ tx (/ tw 2.0))
                         (+ ty (/ th 2.0))])]
    {:from from-layer
     :to to-layer
     :from-point [from-x from-y]
     :to-point [to-x to-y]
     :type type
     :arrowhead (layout/arrowhead-for type)}))

(defn- layer-base-points
  [{:keys [from-point to-point]}]
  (let [[x1 y1] from-point
        [x2 y2] to-point]
    [x1 y1 x2 y2]))

(defn edge-bbox
  [edge]
  (let [[x1 y1 x2 y2] (layer-base-points edge)]
    {:min-x (min x1 x2)
     :max-x (max x1 x2)
     :min-y (min y1 y2)
     :max-y (max y1 y2)}))

(defn bbox-overlap?
  [a b]
  (and (<= (max (:min-x a) (:min-x b))
           (min (:max-x a) (:max-x b)))
       (<= (max (:min-y a) (:min-y b))
           (min (:max-y a) (:max-y b)))))

(defn lane-available?
  [lane-boxes box]
  (not-any? #(bbox-overlap? % box) lane-boxes))

(defn assign-lane-by-box
  [lanes box]
  (let [lane-idx (or (first (keep-indexed (fn [idx lane-boxes]
                                            (when (lane-available? lane-boxes box) idx))
                                          lanes))
                     (count lanes))]
    (if (< lane-idx (count lanes))
      {:lane lane-idx :lanes (update lanes lane-idx conj box)}
      {:lane lane-idx :lanes (conj lanes [box])})))

(defn assign-lane
  [lanes edge]
  (let [box (edge-bbox edge)
        {:keys [lane lanes]} (assign-lane-by-box lanes box)]
    {:lane-idx lane
     :lanes lanes}))

(defn offset-for-lane
  [lane lane-count]
  (* 15.0 (- lane (/ (dec lane-count) 2.0))))

(defn normal-unit
  [x1 y1 x2 y2]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        len (max 0.001 (Math/sqrt (+ (* dx dx) (* dy dy))))]
    [(/ (- dy) len) (/ dx len)]))

(defn- place-layer-lanes
  [base-edges]
  (reduce (fn [{:keys [lanes edges max-lane]} edge]
            (let [{:keys [lane-idx lanes]} (assign-lane lanes edge)]
              {:lanes lanes
               :edges (conj edges (assoc edge :lane lane-idx))
               :max-lane (max max-lane lane-idx)}))
          {:lanes [] :edges [] :max-lane -1}
          base-edges))

(defn layer-edge-drawables
  [scene]
  (let [module->layer (into {}
                           (map (fn [{:keys [module layer]}] [module layer])
                                (:module-positions scene)))
        layer->centers (layer-center-map (:module-positions scene))
        layer-rects (into {}
                         (map (fn [r] [(:index r) r]) (:layer-rects scene)))
        grouped (aggregate-layer-edges scene module->layer)
        base-edges (->> grouped
                        (map (fn [[[from-layer to-layer] type]]
                               (make-layer-base-edge layer-rects layer->centers [from-layer to-layer] type)))
                        (sort-by (juxt :from :to))
                        vec)
        {:keys [edges max-lane]} (place-layer-lanes base-edges)
        lane-count (inc (max 0 max-lane))]
    (mapv (fn [{:keys [lane from-point to-point] :as edge}]
            (let [offset (offset-for-lane lane lane-count)
                  [x1 y1] from-point
                  [x2 y2] to-point
                  [nx ny] (normal-unit x1 y1 x2 y2)]
              (assoc edge
                     :parallel-offset-x (* nx offset)
                     :parallel-offset-y (* ny offset))))
          edges)))

(defn declutter-edge-drawables
  [scene mode]
  (case mode
    :concrete (->> (:edge-drawables scene)
                   (filter #(= :direct (:type %)))
                   vec)
    :abstract (->> (:edge-drawables scene)
                   (filter #(= :abstract (:type %)))
                   vec)
    :layer (layer-edge-drawables scene)
    (:edge-drawables scene)))

(defn- spacing-base-points
  [points {:keys [from to from-point to-point from-rect to-rect]}]
  (let [[sx sy] (or from-point (let [{x :x y :y} (get points from)] [x y]))
        [tx ty] (or to-point (let [{x :x y :y} (get points to)] [x y]))
        [x1 y1] (if from-rect
                  (:point (rect-edge-anchor from-rect tx ty))
                  [sx sy])
        [x2 y2] (if to-rect
                  (:point (rect-edge-anchor to-rect x1 y1))
                  [tx ty])]
    [x1 y1 x2 y2]))

(defn- spacing-bbox
  [points edge]
  (let [[x1 y1 x2 y2] (spacing-base-points points edge)]
    {:min-x (min x1 x2)
     :max-x (max x1 x2)
     :min-y (min y1 y2)
     :max-y (max y1 y2)}))

(defn- spacing-unit-dir
  [points edge]
  (let [[x1 y1 x2 y2] (spacing-base-points points edge)
        dx (- x2 x1)
        dy (- y2 y1)
        len (max 0.001 (Math/sqrt (+ (* dx dx) (* dy dy))))]
    [(/ dx len) (/ dy len)]))

(defn- parallel-ish?
  [[ux uy] [vx vy]]
  (>= (Math/abs (+ (* ux vx) (* uy vy))) 0.985))

(defn- spacing-overlap-indexes
  [indexed points]
  (let [size (count indexed)
        boxes (mapv (comp #(spacing-bbox points %) :edge) indexed)
        dirs (mapv (comp #(spacing-unit-dir points %) :edge) indexed)
        edges (mapv :edge indexed)]
    (reduce (fn [acc [i j]]
              (let [ei (nth edges i)
                    ej (nth edges j)
                    same-endpoint? (or (= (:from ei) (:from ej))
                                       (= (:to ei) (:to ej)))
                    overlap? (bbox-overlap? (nth boxes i) (nth boxes j))
                    parallel? (parallel-ish? (nth dirs i) (nth dirs j))]
                (if (or same-endpoint?
                        (and overlap? parallel?))
                (-> acc
                    (update i (fnil conj #{}) j)
                    (update j (fnil conj #{}) i))
                  acc)))
            {}
            (for [i (range size)
                  j (range (inc i) size)]
              [i j]))))

(defn- connected-components
  [adjacency size]
  (loop [unvisited (set (range size))
         components []]
    (if (empty? unvisited)
      components
      (let [start (first unvisited)
            visited (loop [stack [start]
                           visited #{start}]
                      (if (empty? stack)
                        visited
                        (let [node (peek stack)
                              next-stack (pop stack)
                              neighbors (get adjacency node #{})
                              fresh (remove visited neighbors)]
                          (recur (into next-stack fresh)
                                 (into visited fresh)))))]
        (recur (apply disj unvisited visited)
               (conj components visited))))))

(defn- spacing-assign-lane
  [points lanes edge]
  (let [box (spacing-bbox points edge)
        {:keys [lane lanes]} (assign-lane-by-box lanes box)]
    {:lane lane :lanes lanes}))

(defn- spacing-component-lanes
  [points component-edges]
  (reduce (fn [{:keys [lanes edges max-lane]} edge]
            (let [{:keys [lane lanes]} (spacing-assign-lane points lanes edge)]
              {:lanes lanes
               :edges (conj edges (assoc edge :lane lane))
               :max-lane (max max-lane lane)}))
          {:lanes [] :edges [] :max-lane -1}
          component-edges))

(defn- spacing-offset-edge
  [points base-normal lane-count {:keys [lane] :as edge}]
  (let [[x1 y1 x2 y2] (spacing-base-points points edge)
        offset (offset-for-lane lane lane-count)
        [nx ny] (normal-unit x1 y1 x2 y2)
        dot (+ (* nx (first base-normal)) (* ny (second base-normal)))
        [nx ny] (if (neg? dot) [(- nx) (- ny)] [nx ny])]
    (assoc edge
           :parallel-offset-x (* nx offset)
           :parallel-offset-y (* ny offset))))

(defn- spacing-component->idx-edges
  [points indexed component]
  (let [component-indexes (->> component sort vec)
        component-edges (mapv (fn [idx] (:edge (nth indexed idx))) component-indexes)
        {:keys [edges max-lane]} (spacing-component-lanes points component-edges)
        lane-count (inc (max 0 max-lane))
        [bx1 by1 bx2 by2] (spacing-base-points points (first component-edges))
        base-normal (normal-unit bx1 by1 bx2 by2)]
    (into {}
          (map-indexed (fn [local-idx edge]
                         [(nth component-indexes local-idx)
                          (spacing-offset-edge points base-normal lane-count edge)])
                       edges))))

(defn apply-parallel-arrow-spacing
  [edge-drawables points]
  (let [indexed (mapv (fn [idx edge] {:idx idx :edge edge}) (range) edge-drawables)
        adjacency (spacing-overlap-indexes indexed points)
        components (connected-components adjacency (count indexed))
        idx->edge (reduce (fn [acc component]
                            (merge acc (spacing-component->idx-edges points indexed component)))
                          {}
                          components)]
    (mapv (fn [idx]
            (get idx->edge idx (nth edge-drawables idx)))
          (range (count edge-drawables)))))
