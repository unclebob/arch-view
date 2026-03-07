(ns arch-view.render.ui.quil.view
  (:require [clojure.string :as str]
            [arch-view.domain.architecture-projection :as projection]
            [arch-view.render.ui.quil.canvas :as canvas]
            [arch-view.render.ui.quil.events :as events]
            [arch-view.render.ui.util.functional :as functional]
            [arch-view.render.ui.swing.source-window :as source-window]
            [arch-view.render.route-engine :as route-engine]
            [quil.core :as q]
            [quil.middleware :as m]))

(def ^:private scene-top-padding 42.0)

(defn- layer-y
  [index layer-height layer-gap]
  (functional/layer-y index layer-height layer-gap))

(defn- dominant-component
  [modules module->component]
  (functional/dominant-component modules module->component))

(defn- module-positions-for-layer
  [layer-index modules layer-rect module->kind module->full-name]
  (functional/module-positions-for-layer layer-index modules layer-rect module->kind module->full-name))

(defn abbreviate-module-name
  [module]
  (functional/abbreviate-module-name module))

(defn strip-top-namespace
  [module]
  (functional/strip-top-namespace module))

(defn- label-width
  [label]
  (functional/label-width label))

(defn- rendered-label
  [{:keys [display-label label]}]
  (functional/rendered-label {:display-label display-label :label label}))

(def ^:private toolbar-height 38.0)
(def ^:private back-button-width 360.0)
(def ^:private declutter-button-width 240.0)
(def ^:private button-height 26.0)

(def declutter-modes functional/declutter-modes)

(defn next-declutter-mode
  [mode]
  (functional/next-declutter-mode mode))

(defn declutter-label
  [mode]
  (functional/declutter-label mode))

(defn- back-button-label
  [{:keys [namespace-path nav-stack]}]
  (if-not (seq namespace-path)
    "Back"
    (let [target-path (or (:path (peek (vec (or nav-stack []))))
                          (vec (drop-last (vec (or namespace-path [])))))
          target-label (if (seq target-path)
                         (str/join "." target-path)
                         "root")]
      (str "Back: " target-label))))

(defn- back-button-rect
  []
  {:x 10.0 :y 6.0 :width back-button-width :height button-height})

(defn- declutter-button-rect
  []
  (let [{:keys [x width]} (back-button-rect)]
    {:x (+ x width 10.0) :y 6.0 :width declutter-button-width :height button-height}))

(defn- overlap?
  [a b]
  (< (Math/abs (double (- (:x a) (:x b))))
     (/ (+ (label-width (rendered-label a))
           (label-width (rendered-label b)))
        2.0)))

(defn- needs-stagger?
  [modules]
  (functional/needs-stagger? modules))

(defn- stagger-offset
  [idx]
  (functional/stagger-offset idx))

(defn- apply-layer-stagger
  [modules]
  (functional/apply-layer-stagger modules))

(defn- arrowhead-for
  [edge-type]
  (functional/arrowhead-for edge-type))

(def ^:private racetrack-count 5)
(def ^:private racetrack-margin 24.0)
(def ^:private racetrack-gap 24.0)

(defn- track-width-for
  [canvas-width]
  (functional/track-width-for canvas-width))

(defn- track-x-for
  [track canvas-width]
  (functional/track-x-for track canvas-width))

(defn- dependency-pairs-by-layer
  [classified-edges module->layer]
  (functional/dependency-pairs-by-layer classified-edges module->layer))

(defn- incoming-counts-by-layer
  [layer-indexes layer-pairs]
  (functional/incoming-counts-by-layer layer-indexes layer-pairs))

(defn- edge-point
  [placement node]
  (functional/edge-point placement node))

(defn- orientation
  [[ax ay] [bx by] [cx cy]]
  (functional/orientation [ax ay] [bx by] [cx cy]))

(defn- segment-crosses?
  [p1 p2 q1 q2]
  (functional/segment-crosses? p1 p2 q1 q2))

(defn- edge-cross?
  [placement [a b] [c d]]
  (functional/edge-cross? placement [a b] [c d]))

(defn- edge-crossing-count
  [placement edge-pairs]
  (let [pairs (vec edge-pairs)]
    (count
      (for [i (range (count pairs))
            j (range (inc i) (count pairs))
            :when (edge-cross? placement (nth pairs i) (nth pairs j))]
        true))))

(defn- assign-layer-slots
  [ordered-layer-indexes pairs incoming-counts]
  (functional/assign-layer-slots ordered-layer-indexes pairs incoming-counts))

(defn build-scene
  ([architecture]
   (functional/build-scene architecture))
  ([architecture {:keys [canvas-width layer-height layer-gap]
                  :or {canvas-width 1200 layer-height 140 layer-gap 24}}]
   (functional/build-scene architecture {:canvas-width canvas-width
                                         :layer-height layer-height
                                         :layer-gap layer-gap})))

(defn- module-point-map
  [scene]
  (into {}
        (map (fn [{:keys [module x y]}]
               [module {:x x :y y}])
             (:module-positions scene))))

(defn arrowhead-points
  [x1 y1 x2 y2 arrowhead]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        len (max 0.001 (Math/sqrt (+ (* dx dx) (* dy dy))))
        ux (/ dx len)
        uy (/ dy len)
        back 10.0
        half 5.0
        bx (- x2 (* ux back))
        by (- y2 (* uy back))
        px (- uy)
        py ux]
    {:tip [x2 y2]
     :center [bx by]
     :left [(+ bx (* px half)) (+ by (* py half))]
     :right [(- bx (* px half)) (- by (* py half))]
     :closed? (= :closed-triangle arrowhead)}))

(defn- draw-arrowhead
  [x1 y1 x2 y2 arrowhead]
  (let [{:keys [tip left right closed?]} (arrowhead-points x1 y1 x2 y2 arrowhead)
        [tx ty] tip
        [lx ly] left
        [rx ry] right]
    (if closed?
      (do
        (q/no-fill)
        (q/triangle tx ty lx ly rx ry))
      (do
        (q/no-fill)
        (q/line tx ty lx ly)
        (q/line tx ty rx ry)))))

(defn edge-line-endpoint
  [x1 y1 x2 y2 arrowhead]
  (let [{:keys [tip center closed?]} (arrowhead-points x1 y1 x2 y2 arrowhead)]
    (if closed?
      center
      tip)))

(def ^:private label-clearance 12.0)

(defn dependency-tip-point
  [x1 y1 x2 y2]
  (let [dy (- y2 y1)]
    (cond
      (> dy 0.1) [(double x2) (- (double y2) label-clearance)]
      (< dy -0.1) [(double x2) (+ (double y2) label-clearance)]
      :else [(double x2) (double y2)])))

(defn dependency-start-point
  [x1 y1 x2 y2]
  (let [dy (- y2 y1)]
    (cond
      (> dy 0.1) [(double x1) (+ (double y1) label-clearance)]
      (< dy -0.1) [(double x1) (- (double y1) label-clearance)]
      :else [(double x1) (double y1)])))

(defn- draw-arrow-between-points
  [x1 y1 x2 y2 arrowhead use-label-clearance?]
  (let [[sx sy] (if use-label-clearance?
                  (dependency-start-point x1 y1 x2 y2)
                  [(double x1) (double y1)])
        [tx ty] (if use-label-clearance?
                  (dependency-tip-point x1 y1 x2 y2)
                  [(double x2) (double y2)])
        [ex ey] (edge-line-endpoint sx sy tx ty arrowhead)]
    (if (= :closed-triangle arrowhead)
      (q/stroke 0 128 0)
      (q/stroke 0 0 0))
    (q/line sx sy ex ey)
    (draw-arrowhead sx sy tx ty arrowhead)))

(defn- clamp-offset
  [offset p1 p2 min-v max-v]
  (let [lo (max (- min-v p1) (- min-v p2))
        hi (min (- max-v p1) (- max-v p2))]
    (-> offset double (max lo) (min hi))))

(defn- rect-center
  [{:keys [x y width height]}]
  (functional/rect-center {:x x :y y :width width :height height}))

(defn- clamp-between
  [v lo hi]
  (functional/clamp-between v lo hi))

(defn- rect-edge-anchor
  [{:keys [x y width height] :as rect} tx ty]
  (functional/rect-edge-anchor {:x x :y y :width width :height height} tx ty))

(defn- apply-edge-constrained-offset
  [[x y] side rect offset-x offset-y]
  (let [{rx :x ry :y rw :width rh :height} rect
        corner-inset 10.0
        rx-lo (+ rx corner-inset)
        rx-hi (- (+ rx rw) corner-inset)
        ry-lo (+ ry corner-inset)
        ry-hi (- (+ ry rh) corner-inset)
        right (+ rx rw)
        bottom (+ ry rh)
        side-parallel-offset (if (#{:top :bottom} side)
                               (if (>= (Math/abs (double offset-x))
                                       (Math/abs (double offset-y)))
                                 offset-x
                                 offset-y)
                               (if (>= (Math/abs (double offset-y))
                                       (Math/abs (double offset-x)))
                                 offset-y
                                 offset-x))]
    (case side
      :left [rx (clamp-between (+ y side-parallel-offset) ry-lo ry-hi)]
      :right [right (clamp-between (+ y side-parallel-offset) ry-lo ry-hi)]
      :top [(clamp-between (+ x side-parallel-offset) rx-lo rx-hi) ry]
      :bottom [(clamp-between (+ x side-parallel-offset) rx-lo rx-hi) bottom]
      [(+ x offset-x) (+ y offset-y)])))

(defn- edge-point-for
  [points node explicit]
  (if explicit
    explicit
    (let [{x :x y :y} (get points node)]
      [x y])))

(defn- edge-anchor
  [rect tx ty]
  (when rect
    (rect-edge-anchor rect tx ty)))

(defn- edge-anchor-point
  [anchor x y]
  (or (:point anchor) [x y]))

(defn- edge-offset-point
  [[x y] anchor rect offset-x offset-y]
  (if anchor
    (apply-edge-constrained-offset [x y] (:side anchor) rect offset-x offset-y)
    [(+ (double x) offset-x) (+ (double y) offset-y)]))

(defn- resolved-edge-segment
  [points bounds {:keys [from to from-point to-point from-rect to-rect parallel-offset-x parallel-offset-y]}]
  (let [raw-offset-x (double (or parallel-offset-x 0.0))
        raw-offset-y (double (or parallel-offset-y 0.0))
        [base-x1 base-y1] (edge-point-for points from from-point)
        [base-x2 base-y2] (edge-point-for points to to-point)
        from-anchor (edge-anchor from-rect base-x2 base-y2)
        [x1 y1] (edge-anchor-point from-anchor base-x1 base-y1)
        to-anchor (edge-anchor to-rect x1 y1)
        [x2 y2] (edge-anchor-point to-anchor base-x2 base-y2)
        offset-x (clamp-offset raw-offset-x x1 x2 (:min-x bounds) (:max-x bounds))
        offset-y (clamp-offset raw-offset-y y1 y2 (:min-y bounds) (:max-y bounds))
        [x1 y1] (edge-offset-point [x1 y1] from-anchor from-rect offset-x offset-y)
        [x2 y2] (edge-offset-point [x2 y2] to-anchor to-rect offset-x offset-y)]
    (when (and x1 y1 x2 y2)
      {:x1 x1
       :y1 y1
       :x2 x2
       :y2 y2
       :anchored? (boolean (or from-rect to-rect))
       :from-side (:side from-anchor)
       :to-side (:side to-anchor)})))

(defn- point-in-rect?
  [{:keys [x y width height]} px py]
  (and (<= x px (+ x width))
       (<= y py (+ y height))))

(defn- turn-orientation
  [ax ay bx by cx cy]
  (let [v (- (* (- by ay) (- cx bx))
             (* (- bx ax) (- cy by)))]
    (cond
      (> v 0.0) 1
      (< v 0.0) -1
      :else 0)))

(defn- on-segment?
  [ax ay bx by px py]
  (and (<= (min ax bx) px (max ax bx))
       (<= (min ay by) py (max ay by))))

(defn- segments-intersect?
  [ax ay bx by cx cy dx dy]
  (let [o1 (turn-orientation ax ay bx by cx cy)
        o2 (turn-orientation ax ay bx by dx dy)
        o3 (turn-orientation cx cy dx dy ax ay)
        o4 (turn-orientation cx cy dx dy bx by)]
    (or (and (not= o1 o2) (not= o3 o4))
        (and (zero? o1) (on-segment? ax ay bx by cx cy))
        (and (zero? o2) (on-segment? ax ay bx by dx dy))
        (and (zero? o3) (on-segment? cx cy dx dy ax ay))
        (and (zero? o4) (on-segment? cx cy dx dy bx by)))))

(defn- segment-intersects-rect?
  [x1 y1 x2 y2 {:keys [x y width height] :as rect}]
  (let [right (+ x width)
        bottom (+ y height)]
    (or (point-in-rect? rect x1 y1)
        (point-in-rect? rect x2 y2)
        (segments-intersect? x1 y1 x2 y2 x y right y)
        (segments-intersect? x1 y1 x2 y2 right y right bottom)
        (segments-intersect? x1 y1 x2 y2 right bottom x bottom)
        (segments-intersect? x1 y1 x2 y2 x bottom x y))))

(defn- segment-intersects-any-rect?
  [x1 y1 x2 y2 rects ignored]
  (some (fn [rect]
          (and (not (contains? ignored rect))
               (segment-intersects-rect? x1 y1 x2 y2 rect)))
        rects))

(defn- point-on-rect-edge?
  [{:keys [x y width height]} px py]
  (let [right (+ x width)
        bottom (+ y height)
        on-h (and (<= x px right)
                  (or (< (Math/abs (- (double py) (double y))) 0.1)
                      (< (Math/abs (- (double py) (double bottom))) 0.1)))
        on-v (and (<= y py bottom)
                  (or (< (Math/abs (- (double px) (double x))) 0.1)
                      (< (Math/abs (- (double px) (double right))) 0.1)))]
    (or on-h on-v)))

(defn- ranges-strict-overlap?
  [a1 a2 b1 b2]
  (> (- (min (max a1 a2) (max b1 b2))
        (max (min a1 a2) (min b1 b2)))
     0.1))

(defn- horizontal-overlap-on-rect-edge?
  [x1 y1 x2 y2 x right y bottom]
  (and (< (Math/abs (- (double y1) (double y2))) 0.1)
       (or (and (< (Math/abs (- (double y1) (double y))) 0.1)
                (ranges-strict-overlap? x1 x2 x right))
           (and (< (Math/abs (- (double y1) (double bottom))) 0.1)
                (ranges-strict-overlap? x1 x2 x right)))))

(defn- vertical-overlap-on-rect-edge?
  [x1 y1 x2 y2 x right y bottom]
  (and (< (Math/abs (- (double x1) (double x2))) 0.1)
       (or (and (< (Math/abs (- (double x1) (double x))) 0.1)
                (ranges-strict-overlap? y1 y2 y bottom))
           (and (< (Math/abs (- (double x1) (double right))) 0.1)
                (ranges-strict-overlap? y1 y2 y bottom)))))

(defn- segment-overlaps-rect-edge?
  [[[x1 y1] [x2 y2]] {:keys [x y width height]}]
  (let [right (+ x width)
        bottom (+ y height)]
    (or (horizontal-overlap-on-rect-edge? x1 y1 x2 y2 x right y bottom)
        (vertical-overlap-on-rect-edge? x1 y1 x2 y2 x right y bottom))))

(defn- source-touch?
  [idx rect edge]
  (and (= idx 0) (= rect (:from-rect edge))))

(defn- target-touch?
  [idx last-idx rect edge]
  (and (= idx last-idx) (= rect (:to-rect edge))))

(defn- allowed-touch?
  [idx last-idx rect edge]
  (or (source-touch? idx rect edge)
      (target-touch? idx last-idx rect edge)))

(defn- invalid-touch?
  [idx last-idx rect edge x1 y1 x2 y2]
  (cond
    (source-touch? idx rect edge) (not (point-on-rect-edge? rect x1 y1))
    (target-touch? idx last-idx rect edge) (not (point-on-rect-edge? rect x2 y2))
    :else true))

(defn- segment-invalid-for-rect?
  [idx last-idx segment rect edge]
  (let [[[x1 y1] [x2 y2]] segment
        intersects? (segment-intersects-rect? x1 y1 x2 y2 rect)
        overlaps? (segment-overlaps-rect-edge? segment rect)]
    (cond
      (not intersects?) false
      overlaps? true
      (not (allowed-touch? idx last-idx rect edge)) true
      :else (invalid-touch? idx last-idx rect edge x1 y1 x2 y2))))

(defn- clear-vertical-column?
  [x y1 y2 rects ignored]
  (not (segment-intersects-any-rect? x y1 x y2 rects ignored)))

(defn- choose-route-column
  [x1 y1 x2 y2 {:keys [all-rects from-rect to-rect]} bounds]
  (let [ignored (cond-> #{}
                  from-rect (conj from-rect)
                  to-rect (conj to-rect))
        candidates (->> (concat
                          [x1 x2 (/ (+ x1 x2) 2.0)]
                          (mapcat (fn [{:keys [x width]}]
                                    [(double (- x 12.0))
                                     (double (+ x width 12.0))])
                                  all-rects))
                        (map #(clamp-between % (:min-x bounds) (:max-x bounds)))
                        distinct
                        vec)
        clear (filter #(clear-vertical-column? % y1 y2 all-rects ignored) candidates)]
    (or (first (sort-by #(Math/abs (- (double %) (/ (+ x1 x2) 2.0))) clear))
        (clamp-between (/ (+ x1 x2) 2.0) (:min-x bounds) (:max-x bounds)))))

(defn- source-exit-point
  [x1 y1 x2 y2 from-side bounds]
  (let [step 20.0
        dx (- (double x2) (double x1))
        dy (- (double y2) (double y1))]
    (case from-side
      :top [x1 (clamp-between (- y1 step) (:min-y bounds) (:max-y bounds))]
      :bottom [x1 (clamp-between (+ y1 step) (:min-y bounds) (:max-y bounds))]
      :left [(clamp-between (- x1 step) (:min-x bounds) (:max-x bounds)) y1]
      :right [(clamp-between (+ x1 step) (:min-x bounds) (:max-x bounds)) y1]
      [(clamp-between (+ x1 (* 0.22 dx)) (:min-x bounds) (:max-x bounds))
       (clamp-between (+ y1 (* 0.22 dy)) (:min-y bounds) (:max-y bounds))])))

(defn- needs-rectilinear-route?
  [x1 y1 x2 y2 {:keys [all-rects from-rect to-rect]}]
  (some (fn [rect]
          (and (not= rect from-rect)
               (not= rect to-rect)
               (segment-intersects-rect? x1 y1 x2 y2 rect)))
        all-rects))

(defn- compact-points
  [points]
  (reduce (fn [acc [x y :as p]]
            (if-let [[px py] (peek acc)]
              (if (< (+ (Math/abs (- (double x) (double px)))
                        (Math/abs (- (double y) (double py))))
                     0.1)
                acc
                (conj acc p))
              (conj acc p)))
          []
          points))

(defn- point-close?
  [[ax ay] [bx by]]
  (< (+ (Math/abs (- (double ax) (double bx)))
        (Math/abs (- (double ay) (double by))))
     0.1))

(defn- remove-retraces
  [points]
  (reduce (fn [acc p]
            (let [n (count acc)]
              (if (and (>= n 2)
                       (point-close? p (nth acc (- n 2))))
                (pop acc)
                (conj acc p))))
          []
          points))

(defn- orthogonalize-path
  [path-points]
  (if (< (count path-points) 2)
    (vec path-points)
    (->> (partition 2 1 path-points)
         (reduce (fn [acc [[x1 y1] [x2 y2]]]
                   (let [last-point (peek acc)]
                     (if (or (< (Math/abs (- (double x1) (double x2))) 0.1)
                             (< (Math/abs (- (double y1) (double y2))) 0.1))
                       (conj acc [x2 y2])
                       (let [mid [x2 y1]]
                         (if (= last-point mid)
                           (conj acc [x2 y2])
                           (conj acc mid [x2 y2]))))))
                 [(first path-points)])
         compact-points)))

(declare adjust-target-perpendicular
         adjust-source-perpendicular
         path-clear-of-rectangles?
         normalize-route-endpoints)

(defn- enforce-target-perpendicular
  [path-points to-side]
  (if (or (nil? to-side)
          (< (count path-points) 2))
    path-points
    (-> path-points
        (adjust-target-perpendicular to-side)
        vec
        compact-points
        orthogonalize-path)))

(defn- adjust-target-perpendicular
  [path-points to-side]
  (let [[tx ty] (last path-points)
        [px py] (nth path-points (- (count path-points) 2))]
    (case to-side
      (:top :bottom)
      (if (< (Math/abs (- (double px) (double tx))) 0.1)
        path-points
        (concat (butlast (butlast path-points))
                [[px py] [tx py] [tx ty]]))

      (:left :right)
      (if (< (Math/abs (- (double py) (double ty))) 0.1)
        path-points
        (concat (butlast (butlast path-points))
                [[px py] [px ty] [tx ty]]))

      path-points)))

(defn- enforce-source-perpendicular
  [path-points from-side]
  (if (or (nil? from-side)
          (< (count path-points) 2))
    path-points
    (-> path-points
        (adjust-source-perpendicular from-side)
        vec
        compact-points
        orthogonalize-path)))

(defn- adjust-source-perpendicular
  [path-points from-side]
  (let [[sx sy] (first path-points)
        [nx ny] (second path-points)]
    (case from-side
      (:top :bottom)
      (if (< (Math/abs (- (double sx) (double nx))) 0.1)
        path-points
        (concat [[sx sy] [sx ny]] (drop 2 path-points)))

      (:left :right)
      (if (< (Math/abs (- (double sy) (double ny))) 0.1)
        path-points
        (concat [[sx sy] [nx sy]] (drop 2 path-points)))

      path-points)))

(defn- resolved-direct-edge-path
  [{:keys [x1 y1 x2 y2 anchored? from-side to-side]}]
  {:points (-> [[x1 y1] [x2 y2]]
               (enforce-source-perpendicular from-side)
               (enforce-target-perpendicular to-side)
               orthogonalize-path)
   :anchored? anchored?
   :from-side from-side
   :to-side to-side})

(defn- edge-ignored-rects
  [edge]
  (cond-> #{}
    (:from-rect edge) (conj (:from-rect edge))
    (:to-rect edge) (conj (:to-rect edge))))

(defn- adjusted-source-exit-point
  [x1 y1 x2 y2 from-side bounds edge]
  (let [ignored (edge-ignored-rects edge)
        [p1x p1y :as p1] (source-exit-point x1 y1 x2 y2 from-side bounds)]
    (if (segment-intersects-any-rect? x1 y1 p1x p1y (:all-rects edge) ignored)
      [x1 (clamp-between (+ y1 (if (<= y2 y1) -20.0 20.0)) (:min-y bounds) (:max-y bounds))]
      p1)))

(defn- resolved-rectilinear-edge-path
  [bounds edge {:keys [x1 y1 x2 y2 anchored? from-side to-side]}]
  (let [p1 (adjusted-source-exit-point x1 y1 x2 y2 from-side bounds edge)
        route-x (choose-route-column (first p1) (second p1) x2 y2 edge bounds)
        p2 [route-x (second p1)]
        p3 [route-x y2]
        p4 [x2 y2]]
    {:points (-> (compact-points [[x1 y1] p1 p2 p3 p4])
                 (enforce-source-perpendicular from-side)
                 (enforce-target-perpendicular to-side)
                 orthogonalize-path)
     :anchored? anchored?
     :from-side from-side
     :to-side to-side}))

(defn- resolved-edge-path
  [points bounds edge]
  (when-let [{:keys [x1 y1 x2 y2] :as segment} (resolved-edge-segment points bounds edge)]
    (let [direct (resolved-direct-edge-path segment)]
      (if (or (needs-rectilinear-route? x1 y1 x2 y2 edge)
              (not (path-clear-of-rectangles? (:points direct) edge)))
        (resolved-rectilinear-edge-path bounds edge segment)
        direct))))

(def ^:private min-sidestep 10.0)
(def ^:private route-grid-step 10.0)
(def ^:private a-star-max-iterations 3000)
(def ^:private global-optimize-max-iterations 2)
(def ^:private global-optimize-max-conflicted 12)
(def ^:private max-route-segments 3)
(def ^:private route-quality-mode :fast)
(def ^:private balanced-reroute-limit 8)

(defn- path-segments
  [path-points]
  (map vector path-points (rest path-points)))

(defn- horizontal-segment?
  [[[x1 y1] [x2 y2]]]
  (and (< (Math/abs (- (double y1) (double y2))) 0.1)
       (>= (Math/abs (- (double x1) (double x2))) 0.1)))

(defn- vertical-segment?
  [[[x1 y1] [x2 y2]]]
  (and (< (Math/abs (- (double x1) (double x2))) 0.1)
       (>= (Math/abs (- (double y1) (double y2))) 0.1)))

(defn- ranges-overlap?
  [a1 a2 b1 b2]
  (let [lo (max (min a1 a2) (min b1 b2))
        hi (min (max a1 a2) (max b1 b2))]
    (> (- hi lo) 0.1)))

(defn- collinear-overlap?
  [s1 s2]
  (let [[[ax1 ay1] [ax2 ay2]] s1
        [[bx1 by1] [bx2 by2]] s2]
    (cond
      (and (horizontal-segment? s1)
           (horizontal-segment? s2)
           (< (Math/abs (- (double ay1) (double by1))) 0.1))
      (ranges-overlap? ax1 ax2 bx1 bx2)

      (and (vertical-segment? s1)
           (vertical-segment? s2)
           (< (Math/abs (- (double ax1) (double bx1))) 0.1))
      (ranges-overlap? ay1 ay2 by1 by2)

      :else false)))

(defn- path-clear-of-rectangles?
  [path-points edge]
  (let [segments (vec (path-segments path-points))
        last-idx (dec (count segments))]
    (not-any? true?
              (for [idx (range (count segments))
                    rect (:all-rects edge)]
                (segment-invalid-for-rect? idx last-idx (nth segments idx) rect edge)))))

(defn- path-overlaps-existing?
  [path-points placed-segments]
  (some (fn [segment]
          (some #(collinear-overlap? segment %) placed-segments))
        (path-segments path-points)))

(defn- nudge-path
  [path-points dx dy]
  (let [count-points (count path-points)]
    (if (<= count-points 0)
      (vec path-points)
      (->> path-points
           (map-indexed (fn [idx [x y]]
                          (if (and (> count-points 2)
                                   (or (zero? idx) (= idx (dec count-points))))
                            [x y]
                            [(+ (double x) (double dx))
                             (+ (double y) (double dy))])))
           vec
           orthogonalize-path))))

(defn- sidestep-candidates
  [path-points]
  (let [steps (mapcat (fn [n]
                        [(* min-sidestep n)
                         (* -1.0 min-sidestep n)])
                      (range 1 40))
        base [path-points]
        y-nudges (for [step steps]
                   (nudge-path path-points 0.0 step))
        x-nudges (for [step steps]
                   (nudge-path path-points step 0.0))]
    (vec (concat base y-nudges x-nudges))))

(defn- perimeter-candidates
  [path-points {:keys [min-x max-x min-y max-y]}]
  (if (or (nil? min-x) (nil? max-x) (nil? min-y) (nil? max-y) (< (count path-points) 2))
    []
    (let [[sx sy] (first path-points)
          [tx ty] (last path-points)
          lane-step 12.0
          lanes (range 0 30)
          left-xs (map #(clamp-between (+ (double min-x) (* lane-step %)) min-x max-x) lanes)
          right-xs (map #(clamp-between (- (double max-x) (* lane-step %)) min-x max-x) lanes)
          top-ys (map #(clamp-between (+ (double min-y) (* lane-step %)) min-y max-y) lanes)
          bottom-ys (map #(clamp-between (- (double max-y) (* lane-step %)) min-y max-y) lanes)
          via-left (map (fn [x] [[sx sy] [x sy] [x ty] [tx ty]]) left-xs)
          via-right (map (fn [x] [[sx sy] [x sy] [x ty] [tx ty]]) right-xs)
          via-top (map (fn [y] [[sx sy] [sx y] [tx y] [tx ty]]) top-ys)
          via-bottom (map (fn [y] [[sx sy] [sx y] [tx y] [tx ty]]) bottom-ys)]
      (vec (concat via-left via-right via-top via-bottom)))))

(defn- snap-grid
  [v]
  (* route-grid-step
     (double (Math/round (/ (double v) route-grid-step)))))

(defn- unique-sorted
  [values]
  (->> values
       (filter number?)
       (map snap-grid)
       distinct
       sort
       vec))

(defn- route-grid-axes
  [path-points {:keys [route-bounds all-rects]} placed-segments]
  (let [{min-x0 :min-x max-x0 :max-x min-y0 :min-y max-y0 :max-y} route-bounds
        [sx sy] (first path-points)
        [tx ty] (last path-points)
        edge-pad 10.0
        x-from-rects (mapcat (fn [{:keys [x width]}]
                               [(- x edge-pad)
                                x
                                (+ x width)
                                (+ x width edge-pad)])
                             all-rects)
        y-from-rects (mapcat (fn [{:keys [y height]}]
                               [(- y edge-pad)
                                y
                                (+ y height)
                                (+ y height edge-pad)])
                             all-rects)
        x-from-segments (mapcat (fn [[[x1 _] [x2 _]]] [x1 x2]) placed-segments)
        y-from-segments (mapcat (fn [[[_ y1] [_ y2]]] [y1 y2]) placed-segments)
        raw-xs (concat [sx tx]
                       (mapcat (fn [[[x1 _] [x2 _]]] [x1 x2]) placed-segments)
                       (mapcat (fn [{:keys [x width]}] [x (+ x width)]) all-rects))
        raw-ys (concat [sy ty]
                       (mapcat (fn [[[_ y1] [_ y2]]] [y1 y2]) placed-segments)
                       (mapcat (fn [{:keys [y height]}] [y (+ y height)]) all-rects))
        xs-num (seq (map double (filter number? raw-xs)))
        ys-num (seq (map double (filter number? raw-ys)))
        min-x (or min-x0 (if xs-num (- (apply min xs-num) 40.0) 14.0))
        max-x (or max-x0 (if xs-num (+ (apply max xs-num) 40.0) 1200.0))
        min-y (or min-y0 (if ys-num (- (apply min ys-num) 40.0) 14.0))
        max-y (or max-y0 (if ys-num (+ (apply max ys-num) 40.0) 1200.0))
        x-candidates (concat [min-x max-x sx tx]
                             x-from-rects
                             x-from-segments)
        y-candidates (concat [min-y max-y sy ty]
                             y-from-rects
                             y-from-segments)
        xs (->> x-candidates (filter #(<= min-x % max-x)) unique-sorted)
        ys (->> y-candidates (filter #(<= min-y % max-y)) unique-sorted)
        min-x* (snap-grid min-x)
        max-x* (snap-grid max-x)
        min-y* (snap-grid min-y)
        max-y* (snap-grid max-y)
        full-xs (->> (range (long min-x*) (inc (long max-x*)) (long route-grid-step))
                     (map double)
                     (concat xs)
                     unique-sorted)
        full-ys (->> (range (long min-y*) (inc (long max-y*)) (long route-grid-step))
                     (map double)
                     (concat ys)
                     unique-sorted)]
    {:xs full-xs :ys full-ys}))

(defn- path-overlap-count
  [path-points placed-segments]
  (count
   (for [segment (path-segments path-points)
         :when (some #(collinear-overlap? segment %) placed-segments)]
     true)))

(defn- axis-neighbors
  [vals v]
  (let [idx (.indexOf ^java.util.List vals v)
        prev-v (when (> idx 0) (nth vals (dec idx)))
        next-v (when (and (>= idx 0) (< idx (dec (count vals)))) (nth vals (inc idx)))]
    (cond-> []
      prev-v (conj prev-v)
      next-v (conj next-v))))

(defn- segment-direction
  [[[x1 y1] [x2 y2]]]
  (if (< (Math/abs (- (double x1) (double x2))) 0.1) :v :h))

(defn- segment-cost
  [segment edge placed-segments mode prev-dir]
  (let [[[x1 y1] [x2 y2]] segment
        len (+ (Math/abs (- (double x2) (double x1)))
               (Math/abs (- (double y2) (double y1))))
        ignored (edge-ignored-rects edge)
        rect-hit? (segment-intersects-any-rect? x1 y1 x2 y2 (:all-rects edge) ignored)
        overlap? (some #(collinear-overlap? segment %) placed-segments)
        block-rect? (contains? #{:strict :no-rect} mode)
        block-overlap? (= :strict mode)
        dir (segment-direction segment)
        turn? (and prev-dir (not= prev-dir dir))
        impossible? (or (< len 0.1)
                        (and block-rect? rect-hit?)
                        (and block-overlap? overlap?))
        penalty (+ (if turn? 20.0 0.0)
                   (if rect-hit? 1000000.0 0.0)
                   (if overlap? 200000.0 0.0))]
    (when-not impossible?
      (+ len penalty))))

(defn- manhattan
  [[x y] [tx ty]]
  (+ (Math/abs (- (double tx) (double x)))
     (Math/abs (- (double ty) (double y)))))

(defn- reconstruct-path
  [came state]
  (loop [s state
         rev []]
    (let [[node _] s
          rev' (conj rev node)]
      (if-let [p (get came s)]
        (recur p rev')
        (->> rev' reverse vec)))))

(defn- a*-orthogonal-path
  [path-points edge placed-segments mode]
  (when (>= (count path-points) 2)
    (let [{:keys [xs ys]} (route-grid-axes path-points edge placed-segments)
          start [(snap-grid (first (first path-points)))
                 (snap-grid (second (first path-points)))]
          goal [(snap-grid (first (last path-points)))
                (snap-grid (second (last path-points)))]
          xs (if (some #{(first start)} xs) xs (unique-sorted (conj xs (first start))))
          xs (if (some #{(first goal)} xs) xs (unique-sorted (conj xs (first goal))))
          ys (if (some #{(second start)} ys) ys (unique-sorted (conj ys (second start))))
          ys (if (some #{(second goal)} ys) ys (unique-sorted (conj ys (second goal))))
          start-state [start nil]]
      (loop [open [start-state]
             came {}
             g {start-state 0.0}
             iterations 0]
        (if (or (empty? open) (> iterations a-star-max-iterations))
          nil
          (let [state (apply min-key (fn [s] (+ (get g s Double/POSITIVE_INFINITY)
                                                (manhattan (first s) goal)))
                             open)
                open' (vec (remove #(= % state) open))
                [node prev-dir] state]
            (if (= node goal)
              (reconstruct-path came state)
              (let [[x y] node
                    neighbors (concat
                               (for [nx (axis-neighbors xs x)] [nx y])
                               (for [ny (axis-neighbors ys y)] [x ny]))]
                (let [[open-next came-next g-next]
                      (reduce (fn [[o c gmap] n]
                                (let [segment [node n]
                                      step-cost (segment-cost segment edge placed-segments mode prev-dir)
                                      dir (segment-direction segment)]
                                  (if (nil? step-cost)
                                    [o c gmap]
                                    (let [next-state [n dir]
                                          tentative (+ (get gmap state Double/POSITIVE_INFINITY) step-cost)]
                                      (if (< tentative (get gmap next-state Double/POSITIVE_INFINITY))
                                        [(if (some #(= % next-state) o) o (conj o next-state))
                                         (assoc c next-state state)
                                         (assoc gmap next-state tentative)]
                                        [o c gmap])))))
                              [open' came g]
                              neighbors)]
                  (recur open-next came-next g-next (inc iterations)))))))))))

(defn- path-length
  [path-points]
  (reduce + 0.0
          (for [[[x1 y1] [x2 y2]] (path-segments path-points)]
            (+ (Math/abs (- (double x2) (double x1)))
               (Math/abs (- (double y2) (double y1)))))))

(defn- bend-count
  [path-points]
  (let [segments (vec (path-segments path-points))]
    (count (for [i (range 1 (count segments))
                 :let [d1 (segment-direction (nth segments (dec i)))
                       d2 (segment-direction (nth segments i))]
                 :when (not= d1 d2)]
             true))))

(defn- path-violation-counts
  [path-points edge placed-segments]
  (let [segments (vec (path-segments path-points))
        last-idx (dec (count segments))
        rect-viol (count
                   (for [idx (range (count segments))
                         rect (:all-rects edge)
                         :when (segment-invalid-for-rect? idx last-idx (nth segments idx) rect edge)]
                     true))
        overlap-viol (count
                      (for [segment segments
                            :when (some #(collinear-overlap? segment %) placed-segments)]
                        true))]
    {:rect rect-viol :overlap overlap-viol}))

(defn- path-score
  [path-points edge placed-segments]
  (let [{:keys [rect overlap]} (path-violation-counts path-points edge placed-segments)]
    (+ (path-length path-points)
       (* 45.0 (bend-count path-points))
       (* 1000000.0 rect)
       (* 200000.0 overlap))))

(defn- cap-segment-count
  [path-points edge placed-segments]
  (let [segments (vec (path-segments path-points))]
    (if (<= (count segments) max-route-segments)
      path-points
      (let [[sx sy] (first path-points)
            [tx ty] (last path-points)
            {:keys [min-x max-x min-y max-y]} (:route-bounds edge)
            sx (snap-grid sx)
            sy (snap-grid sy)
            tx (snap-grid tx)
            ty (snap-grid ty)
            base-xs (concat [sx tx]
                            (map first path-points)
                            (when (number? min-x) [min-x])
                            (when (number? max-x) [max-x]))
            base-ys (concat [sy ty]
                            (map second path-points)
                            (when (number? min-y) [min-y])
                            (when (number? max-y) [max-y]))
            x-options (->> base-xs (filter number?) unique-sorted (take 8))
            y-options (->> base-ys (filter number?) unique-sorted (take 8))
            hv [[sx sy] [tx sy] [tx ty]]
            vh [[sx sy] [sx ty] [tx ty]]
            via-x (for [x x-options]
                    [[sx sy] [x sy] [x ty] [tx ty]])
            via-y (for [y y-options]
                    [[sx sy] [sx y] [tx y] [tx ty]])
            candidates (->> (concat [[[sx sy] [tx ty]]]
                                     [hv vh]
                                     via-x
                                     via-y)
                            (map (fn [candidate]
                                   (-> candidate
                                       vec
                                       orthogonalize-path
                                       remove-retraces
                                       compact-points)))
                            (filter #(>= (count %) 2))
                            (filter #(<= (count (path-segments %)) max-route-segments))
                            distinct
                            vec)]
        (if (seq candidates)
          (apply min-key #(path-score % edge placed-segments) candidates)
          (->> path-points
               (take (inc max-route-segments))
               vec
               orthogonalize-path
               remove-retraces
               compact-points))))))

(defn- template-candidates
  [path-points edge]
  (let [normalized-base (normalize-route-endpoints path-points edge)]
    (if (< (count normalized-base) 2)
      [normalized-base]
      (let [[sx sy] (first normalized-base)
            [tx ty] (last normalized-base)
            {:keys [min-x max-x min-y max-y]} (:route-bounds edge)
            sx (snap-grid sx)
            sy (snap-grid sy)
            tx (snap-grid tx)
            ty (snap-grid ty)
            x-options (->> (concat [sx tx min-x max-x]
                                    (map first normalized-base)
                                    (mapcat (fn [{:keys [x width]}] [x (+ x width)])
                                            (:all-rects edge)))
                           (filter number?)
                           unique-sorted
                           (take 10))
            y-options (->> (concat [sy ty min-y max-y]
                                    (map second normalized-base)
                                    (mapcat (fn [{:keys [y height]}] [y (+ y height)])
                                            (:all-rects edge)))
                           (filter number?)
                           unique-sorted
                           (take 10))
            direct [[sx sy] [tx ty]]
            hv [[sx sy] [tx sy] [tx ty]]
            vh [[sx sy] [sx ty] [tx ty]]
            via-x (for [x x-options] [[sx sy] [x sy] [x ty] [tx ty]])
            via-y (for [y y-options] [[sx sy] [sx y] [tx y] [tx ty]])]
        (->> (concat [direct hv vh] via-x via-y)
             (map (fn [candidate]
                    (-> candidate
                        vec
                        orthogonalize-path
                        remove-retraces
                        compact-points)))
             (filter #(>= (count %) 2))
             distinct
             vec)))))

(defn- non-zero-segments
  [path-points]
  (->> (path-segments path-points)
       (filter (fn [[[x1 y1] [x2 y2]]]
                 (> (+ (Math/abs (- (double x2) (double x1)))
                       (Math/abs (- (double y2) (double y1))))
                    0.1)))
       vec))

(defn- normalize-route-endpoints
  [path-points {:keys [from-rect to-rect from-side to-side]}]
  (if (< (count path-points) 2)
    (vec path-points)
    (let [with-source (if from-rect
                        (let [current-source (first path-points)]
                          (if (point-on-rect-edge? from-rect (first current-source) (second current-source))
                            (vec path-points)
                            (let [[nx ny] (second path-points)
                                  source-anchor (:point (rect-edge-anchor from-rect nx ny))]
                              (assoc (vec path-points) 0 source-anchor))))
                        (vec path-points))
          with-target (if to-rect
                        (let [count-points (count with-source)
                              current-target (nth with-source (dec count-points))]
                          (if (point-on-rect-edge? to-rect (first current-target) (second current-target))
                            with-source
                            (let [[px py] (nth with-source (- count-points 2))
                                  target-anchor (:point (rect-edge-anchor to-rect px py))]
                              (assoc with-source (dec count-points) target-anchor))))
                        with-source)]
      (-> with-target
          (enforce-source-perpendicular from-side)
          (enforce-target-perpendicular to-side)
          ((fn [pts]
             (mapv (fn [[x y]] [(snap-grid x) (snap-grid y)]) pts)))
          orthogonalize-path
          remove-retraces
          compact-points))))

(defn- place-non-overlapping-path
  [path-points edge placed-segments]
  (let [normalized-base (normalize-route-endpoints path-points edge)
        template-first (->> (template-candidates path-points edge)
                            (map #(cap-segment-count % edge placed-segments))
                            vec)
        strict-template (->> template-first
                             (filter #(and (path-clear-of-rectangles? % edge)
                                           (not (path-overlaps-existing? % placed-segments))))
                             vec)
        candidates (->> (concat template-first
                                [normalized-base])
                        (map #(normalize-route-endpoints % edge))
                        (map #(cap-segment-count % edge placed-segments))
                        (filter #(>= (count %) 2))
                        distinct
                        vec)
        strict (->> candidates
                    (filter #(and (path-clear-of-rectangles? % edge)
                                  (not (path-overlaps-existing? % placed-segments))))
                    vec)]
    (cond
      (seq strict)
      (apply min-key #(path-score % edge placed-segments) strict)
      (seq candidates)
      (apply min-key #(path-score % edge placed-segments) candidates)
      :else
      normalized-base)))

(defn- edge-segment-vec
  [edge]
  (vec (path-segments (or (:route-points edge) []))))

(defn- edge-rect-violation-count
  [edge]
  (let [segments (edge-segment-vec edge)
        last-idx (dec (count segments))]
    (count
     (for [idx (range (count segments))
           rect (:all-rects edge)
           :when (segment-invalid-for-rect? idx last-idx (nth segments idx) rect edge)]
       true))))

(defn- edge-overlap?
  [edge-a edge-b]
  (let [sa (edge-segment-vec edge-a)
        sb (edge-segment-vec edge-b)]
    (some (fn [seg-a]
            (some #(collinear-overlap? seg-a %) sb))
          sa)))

(defn- edge-route-bbox
  [edge]
  (let [segments (edge-segment-vec edge)
        xs (mapcat (fn [[[x1 _] [x2 _]]] [x1 x2]) segments)
        ys (mapcat (fn [[[_ y1] [_ y2]]] [y1 y2]) segments)]
    (when (and (seq xs) (seq ys))
      {:min-x (apply min xs)
       :max-x (apply max xs)
       :min-y (apply min ys)
       :max-y (apply max ys)})))

(defn- bbox-overlap?
  [a b]
  (and (<= (max (:min-x a) (:min-x b))
           (min (:max-x a) (:max-x b)))
       (<= (max (:min-y a) (:min-y b))
           (min (:max-y a) (:max-y b)))))

(defn- routing-conflicts
  [edges]
  (let [size (count edges)
        rect-counts (->> edges
                         (pmap edge-rect-violation-count)
                         doall
                         vec)
        bboxes (mapv edge-route-bbox edges)
        base (zipmap (range size) rect-counts)
        pairs (vec (for [i (range size)
                         j (range (inc i) size)]
                     [i j]))
        pair-overlaps (->> pairs
                           (pmap (fn [[i j]]
                                   (let [bi (nth bboxes i)
                                         bj (nth bboxes j)]
                                     (when (and bi bj
                                                (bbox-overlap? bi bj)
                                                (edge-overlap? (nth edges i) (nth edges j)))
                                       [i j]))))
                           doall
                           (remove nil?)
                           vec)
        by-index (reduce (fn [acc [i j]]
                           (-> acc
                               (update i (fnil inc 0))
                               (update j (fnil inc 0))))
                         base
                         pair-overlaps)
        total (+ (reduce + 0 rect-counts)
                 (count pair-overlaps))]
    {:total total
     :by-index by-index}))

(defn- reroute-edge-against
  [edge placed-segments]
  (let [base (vec (or (:route-points edge) []))
        next-path (place-non-overlapping-path base edge placed-segments)]
    (assoc edge :route-points (or next-path base))))

(defn- reroute-conflicted
  [edges conflicted-order]
  (let [conflicted (set conflicted-order)
        fixed-segments (->> (map-indexed vector edges)
                            (remove (fn [[idx _]] (contains? conflicted idx)))
                            (mapcat (fn [[_ edge]] (edge-segment-vec edge)))
                            vec)]
    (loop [remaining conflicted-order
           rerouted {}
           placed fixed-segments]
      (if (empty? remaining)
        (mapv (fn [idx]
                (get rerouted idx (nth edges idx)))
              (range (count edges)))
        (let [idx (first remaining)
              edge' (reroute-edge-against (nth edges idx) placed)
              segments' (edge-segment-vec edge')]
          (recur (rest remaining)
                 (assoc rerouted idx edge')
                 (into placed segments')))))))

(defn- global-optimize-routes
  [routed]
  (case route-quality-mode
    :fast routed
    :balanced
    (let [{:keys [by-index]} (routing-conflicts routed)
          conflicted-order (->> by-index
                                (filter (fn [[_ c]] (pos? c)))
                                (sort-by (fn [[idx c]] [(- c) idx]))
                                (map first)
                                (take balanced-reroute-limit)
                                vec)]
      (if (empty? conflicted-order)
        routed
        (let [candidate-a (reroute-conflicted routed conflicted-order)
              score-a (:total (routing-conflicts candidate-a))
              candidate-b (reroute-conflicted routed (vec (reverse conflicted-order)))
              score-b (:total (routing-conflicts candidate-b))]
          (if (<= score-a score-b) candidate-a candidate-b))))
    routed))

(defn- edge-path-points
  [points bounds edge]
  (normalize-route-endpoints
   (or (:route-points edge)
       (:points (resolved-edge-path points bounds edge)))
   edge))

(defn- edge-anchored?
  [edge]
  (if (contains? edge :anchored?)
    (:anchored? edge)
    (boolean (or (:from-rect edge) (:to-rect edge)))))

(defn- stroke-for-arrowhead!
  [arrowhead]
  (if (= :closed-triangle arrowhead)
    (q/stroke 0 128 0)
    (q/stroke 0 0 0)))

(defn- draw-multi-segment-edge!
  [segments arrowhead]
  (stroke-for-arrowhead! arrowhead)
  (doseq [[[sx sy] [tx ty]] (butlast segments)]
    (q/line sx sy tx ty))
  (let [[[x1 y1] [x2 y2]] (last segments)
        [ex ey] (edge-line-endpoint x1 y1 x2 y2 arrowhead)]
    (q/line x1 y1 ex ey)
    (draw-arrowhead x1 y1 x2 y2 arrowhead)))

(defn- draw-edge
  [points bounds {:keys [arrowhead preserve-endpoints?] :as edge}]
  (let [path-points (edge-path-points points bounds edge)]
    (when (seq path-points)
      (let [segments (non-zero-segments path-points)]
        (when (seq segments)
          (let [[[x1 y1] [x2 y2]] (last segments)]
            (if (and (= 1 (count segments)) (not preserve-endpoints?))
              (draw-arrow-between-points x1 y1 x2 y2 arrowhead (not (edge-anchored? edge)))
              (draw-multi-segment-edge! segments arrowhead))))))))

(defn layer-edge-drawables
  [scene]
  (functional/layer-edge-drawables scene))

(defn declutter-edge-drawables
  [scene mode]
  (functional/declutter-edge-drawables scene mode))

(defn apply-parallel-arrow-spacing
  [edge-drawables points]
  (functional/apply-parallel-arrow-spacing edge-drawables points))

(defn- label-hitbox
  [{:keys [x y] :as module-position}]
  (let [width (label-width (rendered-label module-position))
        pad-x 8.0
        pad-y 6.0
        half-w (+ (/ width 2.0) pad-x)
        half-h (+ 7.0 pad-y)]
    {:left (- x half-w)
     :right (+ x half-w)
     :top (- y half-h)
     :bottom (+ y half-h)}))

(defn hovered-module
  [module-positions mx my]
  (some (fn [{:keys [module] :as m}]
          (let [{:keys [left right top bottom]} (label-hitbox m)]
            (when (and (<= left mx right)
                       (<= top my bottom))
              module)))
        module-positions))

(defn- layer-label-hitbox
  [{:keys [x y label]}]
  (let [width (label-width label)]
    {:left (+ x 6.0)
     :right (+ x 10.0 width)
     :top (+ y 4.0)
     :bottom (+ y 22.0)}))

(defn hovered-layer-label
  [layer-rects mx my]
  (some (fn [{:keys [label] :as layer-rect}]
          (let [{:keys [left right top bottom]} (layer-label-hitbox layer-rect)]
            (when (and (<= left mx right)
                       (<= top my bottom))
              layer-rect)))
        layer-rects))

(defn- hovered-module-position
  [module-positions mx my]
  (some (fn [m]
          (let [{:keys [left right top bottom]} (label-hitbox m)]
            (when (and (<= left mx right)
                       (<= top my bottom))
              m)))
        module-positions))

(declare view-architecture
         drilldown-scene
         push-nav-state
         apply-zoom-click
         world-x-at-screen
         world-y-at-screen
         scroll-for-world-x
         scroll-for-world-y
         point-in-toolbar?)

(defn- drillable?
  [state hovered]
  (let [candidate (conj (or (:namespace-path state) []) (:module hovered))
        child-view (view-architecture (:architecture state) candidate)]
    (seq (get-in child-view [:graph :nodes]))))

(defn attach-drillable-markers
  [scene architecture namespace-path]
  (if-not architecture
    scene
    (update scene :module-positions
            (fn [positions]
              (mapv (fn [position]
                      (let [drillable (boolean (drillable? {:architecture architecture
                                                            :namespace-path namespace-path}
                                                           position))
                            label (:label position)]
                        (assoc position
                               :drillable? drillable
                               :display-label (if drillable
                                                (str "+ " label)
                                                label))))
                    positions)))))

(defn scroll-range
  [content-height viewport-height]
  (max 0.0 (- (double content-height) (double viewport-height))))

(defn clamp-scroll
  [scroll-y content-height viewport-height]
  (let [max-scroll (scroll-range content-height viewport-height)]
    (-> scroll-y double (max 0.0) (min max-scroll))))

(defn clamp-scroll-x
  [scroll-x content-width viewport-width]
  (let [max-scroll (scroll-range content-width viewport-width)]
    (-> scroll-x double (max 0.0) (min max-scroll))))

(defn scrollbar-rect
  [content-height viewport-height scroll-y viewport-width]
  (when (> content-height viewport-height)
    (let [track-height (- (double viewport-height) 24.0)
          ratio (/ (double viewport-height) (double content-height))
          thumb-height (max 30.0 (* track-height ratio))
          max-scroll (scroll-range content-height viewport-height)
          thumb-y (if (zero? max-scroll)
                    12.0
                    (+ 12.0 (* (- track-height thumb-height) (/ scroll-y max-scroll))))]
      {:x (- (double viewport-width) 12.0)
       :y thumb-y
       :width 8.0
       :height thumb-height})))

(defn- content-height-for-scene
  [scene]
  (->> (:layer-rects scene)
       (map (fn [{:keys [y height]}] (+ y height)))
       (apply max 0)
       (+ 40)))

(defn- scaled-content-height
  [scene zoom]
  (* (double (or zoom 1.0))
     (content-height-for-scene scene)))

(defn- content-width-for-scene
  [scene]
  (->> (:layer-rects scene)
       (map (fn [{:keys [x width]}] (+ x width)))
       (apply max 0)
       (+ racetrack-margin 20.0)))

(defn- scaled-content-width
  [scene zoom]
  (* (double (or zoom 1.0))
     (content-width-for-scene scene)))

(defn thumb-y->scroll
  [thumb-y content-height viewport-height]
  (if (<= content-height viewport-height)
    0.0
    (let [track-height (- (double viewport-height) 24.0)
          ratio (/ (double viewport-height) (double content-height))
          thumb-height (max 30.0 (* track-height ratio))
          max-scroll (scroll-range content-height viewport-height)
          max-thumb-travel (max 1.0 (- track-height thumb-height))
          normalized (/ (- thumb-y 12.0) max-thumb-travel)]
      (clamp-scroll (* normalized max-scroll) content-height viewport-height))))

(defn- draw-scene-content
  [scene viewport-width spaced-edges]
  (canvas/draw-scene-content scene viewport-width spaced-edges
                             {:rendered-label rendered-label
                              :module-point-map module-point-map
                              :content-height-for-scene content-height-for-scene
                              :draw-edge draw-edge}))

(defn- prepare-edge-drawables
  [scene declutter-mode]
  (let [points (module-point-map scene)
        layer-rect-by-index (into {} (map (juxt :index identity) (:layer-rects scene)))
        module->layer (into {} (map (juxt :module :layer) (:module-positions scene)))
        route-bounds {:min-x 14.0
                      :max-x (+ 20.0
                                (reduce max 0.0
                                        (map (fn [{:keys [x width]}] (+ x width))
                                             (:layer-rects scene))))
                      :min-y 14.0
                      :max-y (+ 20.0
                                (reduce max 0.0
                                        (map (fn [{:keys [y height]}] (+ y height))
                                             (:layer-rects scene))))}
        edge-drawables (->> (declutter-edge-drawables scene declutter-mode)
                            (mapv (fn [{:keys [from to] :as edge}]
                                    (let [from-layer (if (number? from)
                                                       from
                                                       (get module->layer from))
                                          to-layer (if (number? to)
                                                     to
                                                     (get module->layer to))]
                                      (assoc edge
                                             :from-rect (get layer-rect-by-index from-layer)
                                             :to-rect (get layer-rect-by-index to-layer)
                                             :all-rects (:layer-rects scene)
                                             :route-bounds route-bounds)))))]
    (let [spaced (apply-parallel-arrow-spacing edge-drawables points)]
      (let [routed (route-engine/route-edges
                     {:spaced-edges spaced
                      :resolve-edge-path (fn [edge]
                                           (resolved-edge-path points route-bounds edge))
                      :normalize-route-endpoints normalize-route-endpoints
                      :place-non-overlapping-path place-non-overlapping-path
                      :path-segments path-segments})]
        (if (> (count routed) 1)
          (global-optimize-routes routed)
          routed)))))

(defn- filter-routed-edges
  [routed declutter-mode]
  (case declutter-mode
    :concrete (->> routed (filter #(= :direct (:type %))) vec)
    :abstract (->> routed (filter #(= :abstract (:type %))) vec)
    routed))

(defn- precompute-routed-edges
  [scene]
  (prepare-edge-drawables scene :all))

(defn- unrouted-edges
  [scene]
  (vec (or (:edge-drawables scene) [])))

(defn- draw-toolbar
  [{:keys [namespace-path declutter-mode nav-stack]}]
  (canvas/draw-toolbar {:namespace-path namespace-path
                        :declutter-mode declutter-mode
                        :nav-stack nav-stack}
                       {:back-button-rect back-button-rect
                        :declutter-button-rect declutter-button-rect
                        :back-button-label back-button-label
                        :declutter-label declutter-label
                        :toolbar-height toolbar-height}))

(defn- draw-tooltip
  [full-name mx my]
  (canvas/draw-tooltip full-name mx my))

(defn- point->segment-distance
  [px py x1 y1 x2 y2]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        len2 (+ (* dx dx) (* dy dy))]
    (if (< len2 0.001)
      (Math/sqrt (+ (* (- px x1) (- px x1))
                    (* (- py y1) (- py y1))))
      (let [t (-> (/ (+ (* (- px x1) dx)
                        (* (- py y1) dy))
                     len2)
                  double
                  (max 0.0)
                  (min 1.0))
            proj-x (+ x1 (* t dx))
            proj-y (+ y1 (* t dy))]
        (Math/sqrt (+ (* (- px proj-x) (- px proj-x))
                      (* (- py proj-y) (- py proj-y))))))))

(defn hovered-edge
  ([spaced-edges points bounds mx my]
   (hovered-edge spaced-edges points bounds mx my 8.0))
  ([spaced-edges points bounds mx my tolerance]
   (->> spaced-edges
        (map (fn [edge]
               (let [path-points (or (:route-points edge)
                                     (:points (resolved-edge-path points bounds edge)))]
                 (when (seq path-points)
                    (let [segments (map vector path-points (rest path-points))
                          segment-info (map (fn [[[x1 y1] [x2 y2]]]
                                              (let [dx (Math/abs (double (- x2 x1)))
                                                    dy (Math/abs (double (- y2 y1)))
                                                    diagonal? (and (> dx 1.0) (> dy 1.0))
                                                    tol (* (double tolerance) (if diagonal? 0.6 1.0))
                                                    dist (point->segment-distance mx my x1 y1 x2 y2)]
                                                {:tol tol :dist dist}))
                                            segments)
                          best (first (sort-by :dist segment-info))]
                      (assoc edge
                             :hover-tolerance (:tol best)
                             :distance (:dist best)))))))
        (remove nil?)
        (filter #(<= (double (:distance %)) (double (:hover-tolerance %))))
        (sort-by :distance)
        first)))

(defn edge-hover-label
  [{:keys [from to count]}]
  (str from "->" to "(" (long (or count 1)) ")"))

(defn- html-escape
  [s]
  (source-window/html-escape s))

(defn- colorize-clojure-html
  [source]
  (source-window/colorize-clojure-html source))

(defn- expand-tabs
  [line]
  (source-window/expand-tabs line))

(defn- source-lines->html
  [source]
  (source-window/source-lines->html source))

(defn- source->html
  [title source]
  (source-window/source->html title source))

(defn open-source-file-window!
  [source-file]
  (source-window/open-source-file-window! source-file))

(defn- draw-scrollbar
  [content-height viewport-height scroll-y viewport-width]
  (canvas/draw-scrollbar content-height viewport-height scroll-y viewport-width
                         {:scrollbar-rect scrollbar-rect}))

(defn- draw-scene
  [{:keys [scene declutter-mode scroll-x scroll-y viewport-height viewport-width zoom] :as state}]
  (let [routed (or (:routed-edges state)
                   (if (:skip-routing? state)
                     (unrouted-edges scene)
                     (precompute-routed-edges scene)))]
    (canvas/draw-scene state
                       {:scaled-content-height scaled-content-height
                        :point-in-toolbar? point-in-toolbar?
                        :module-point-map module-point-map
                        :prepare-edge-drawables (fn [_ mode]
                                                  (filter-routed-edges routed mode))
                        :hovered-edge hovered-edge
                        :hovered-module-position hovered-module-position
                        :hovered-layer-label hovered-layer-label
                        :edge-hover-label edge-hover-label
                        :draw-scene-content draw-scene-content
                        :draw-toolbar draw-toolbar
                        :draw-tooltip draw-tooltip
                        :draw-scrollbar draw-scrollbar})))

(defn- plus-key?
  [k]
  (events/plus-key? k))

(defn- minus-key?
  [k]
  (events/minus-key? k))

(defn- zoom-in-at-screen-pos
  [{:keys [zoom zoom-stack scene viewport-height viewport-width] :as state} screen-x screen-y]
  (events/zoom-in-at-screen-pos state screen-x screen-y
                                {:scaled-content-width scaled-content-width
                                 :scaled-content-height scaled-content-height
                                 :clamp-scroll clamp-scroll
                                 :clamp-scroll-x clamp-scroll-x}))

(defn- zoom-out-at-screen-pos
  [{:keys [zoom zoom-stack scene viewport-height viewport-width] :as state} screen-x screen-y]
  (events/zoom-out-at-screen-pos state screen-x screen-y
                                 {:scaled-content-width scaled-content-width
                                  :scaled-content-height scaled-content-height
                                  :clamp-scroll clamp-scroll
                                  :clamp-scroll-x clamp-scroll-x}))

(defn handle-key-pressed
  [state event]
  (events/handle-key-pressed state event
                             {:scaled-content-width scaled-content-width
                              :scaled-content-height scaled-content-height
                              :clamp-scroll clamp-scroll
                              :clamp-scroll-x clamp-scroll-x}))

(defn handle-mouse-wheel
  [{:keys [scene scroll-y viewport-height viewport-width zoom] :as state} event]
  (events/handle-mouse-wheel state event
                             {:scaled-content-height scaled-content-height
                              :clamp-scroll clamp-scroll}))

(defn- mouse-pos
  [event]
  (events/mouse-pos event))

(defn handle-mouse-pressed
  [{:keys [scene scroll-y viewport-height viewport-width zoom] :as state} event]
  (events/handle-mouse-pressed state event
                               {:scaled-content-height scaled-content-height
                                :scrollbar-rect scrollbar-rect
                                :point-in-rect? point-in-rect?}))

(defn handle-mouse-dragged
  [{:keys [scene viewport-height dragging-scrollbar? drag-offset zoom] :as state} event]
  (events/handle-mouse-dragged state event
                               {:scaled-content-height scaled-content-height
                                :thumb-y->scroll thumb-y->scroll}))

(defn- apply-drilldown-click
  [{:keys [scene scroll-x scroll-y namespace-path zoom] :as state} event]
  (events/apply-drilldown-click state event
                                {:hovered-module-position hovered-module-position
                                 :view-architecture view-architecture
                                 :push-nav-state push-nav-state
                                 :drilldown-scene drilldown-scene
                                 :open-source-file-window! open-source-file-window!}))

(defn- navigate-up
  [{:keys [nav-stack] :as state}]
  (events/navigate-up state {:drilldown-scene drilldown-scene}))

(defn- toolbar-click-target
  [state mx my]
  (events/toolbar-click-target state mx my {:point-in-rect? point-in-rect?
                                            :back-button-rect back-button-rect
                                            :declutter-button-rect declutter-button-rect
                                            :toolbar-height toolbar-height}))

(defn- point-in-toolbar?
  [mx my]
  (events/point-in-toolbar? mx my toolbar-height))

(defn- apply-toolbar-click
  [state event]
  (events/apply-toolbar-click state event
                              {:point-in-rect? point-in-rect?
                               :back-button-rect back-button-rect
                               :declutter-button-rect declutter-button-rect
                               :next-declutter-mode next-declutter-mode
                               :drilldown-scene drilldown-scene
                               :toolbar-height toolbar-height}))

(defn- control-down?
  [event]
  (events/control-down? event))

(defn- button-kind
  [event]
  (events/button-kind event))

(defn- scaled-scroll-for-zoom
  [scroll-y old-zoom new-zoom]
  (if (<= old-zoom 0.0)
    scroll-y
    (* (double (or scroll-y 0.0))
       (/ (double new-zoom) (double old-zoom)))))

(defn- world-y-at-screen
  [screen-y scroll-y zoom]
  (events/world-y-at-screen screen-y scroll-y zoom))

(defn- world-x-at-screen
  [screen-x scroll-x zoom]
  (events/world-x-at-screen screen-x scroll-x zoom))

(defn- scroll-for-world-y
  [world-y screen-y zoom]
  (events/scroll-for-world-y world-y screen-y zoom))

(defn- scroll-for-world-x
  [world-x screen-x zoom]
  (events/scroll-for-world-x world-x screen-x zoom))

(defn- apply-zoom-click
  [{:keys [zoom zoom-stack scene viewport-height] :as state} event]
  (events/apply-zoom-click state event
                           {:scaled-content-height scaled-content-height
                            :clamp-scroll clamp-scroll}))

(defn handle-mouse-released
  [{:keys [dragging-scrollbar?] :as state} event]
  (events/handle-mouse-released state event
                                {:point-in-rect? point-in-rect?
                                 :back-button-rect back-button-rect
                                 :declutter-button-rect declutter-button-rect
                                 :next-declutter-mode next-declutter-mode
                                 :drilldown-scene drilldown-scene
                                 :toolbar-height toolbar-height
                                 :hovered-module-position hovered-module-position
                                 :view-architecture view-architecture
                                 :push-nav-state push-nav-state
                                 :open-source-file-window! open-source-file-window!}))

(defn handle-mouse-clicked
  [state event]
  (events/handle-mouse-clicked state event
                               {:point-in-rect? point-in-rect?
                                :back-button-rect back-button-rect
                                :declutter-button-rect declutter-button-rect
                                :next-declutter-mode next-declutter-mode
                                :drilldown-scene drilldown-scene
                                :toolbar-height toolbar-height
                                :hovered-module-position hovered-module-position
                                :view-architecture view-architecture
                                :push-nav-state push-nav-state
                                :open-source-file-window! open-source-file-window!}))

(defn view-architecture
  [architecture namespace-path]
  (projection/view-architecture architecture namespace-path))

(defn- drilldown-scene
  [state path scroll-x scroll-y]
  (let [view (view-architecture (:architecture state) path)
        scene (-> (build-scene view)
                  (attach-drillable-markers (:architecture state) path))
        routed-edges (if (:skip-routing? state)
                       (unrouted-edges scene)
                       (precompute-routed-edges scene))]
    (assoc state
           :namespace-path path
           :scroll-x (double (or scroll-x 0.0))
           :scroll-y (double (or scroll-y 0.0))
           :scene scene
           :routed-edges routed-edges)))

(defn initial-scene-for-show
  [scene architecture]
  (if architecture
    (let [initial-view (view-architecture architecture [])]
      (-> (build-scene initial-view)
          (attach-drillable-markers architecture [])))
    scene))

(defn- push-nav-state
  [{:keys [namespace-path scroll-x scroll-y nav-stack] :as state}]
  (assoc state :nav-stack (conj (vec (or nav-stack []))
                                {:path (vec (or namespace-path []))
                                 :scroll-x (double (or scroll-x 0.0))
                                 :scroll-y (double (or scroll-y 0.0))})))

(defn show!
  ([scene]
   (show! scene {}))
  ([scene {:keys [title architecture skip-routing?]
           :or {title "architecture-viewer"
                skip-routing? false}}]
   (let [effective-architecture (or architecture {:scene scene})
         initial-scene (initial-scene-for-show scene architecture)
         initial-routed-edges (if skip-routing?
                               (unrouted-edges initial-scene)
                               (precompute-routed-edges initial-scene))
         content-height (if (seq (:layer-rects initial-scene))
                          (->> (:layer-rects initial-scene)
                               (map (fn [{:keys [y height]}] (+ y height)))
                               (apply max)
                               (+ 40))
                          400)
         viewport-height (int (min 900 (max 400 content-height)))
         width (int (max 1200 (content-width-for-scene initial-scene)))]
     (q/sketch
       :title title
       :size [width viewport-height]
      :setup (fn []
                {:scene initial-scene
                 :architecture effective-architecture
                 :namespace-path (when architecture [])
                 :nav-stack []
                 :declutter-mode :all
                 :zoom 1.0
                 :zoom-stack []
                 :suppress-next-click? false
                 :scroll-x 0.0
                 :scroll-y 0.0
                 :dragging-scrollbar? false
                 :drag-offset nil
                 :skip-routing? skip-routing?
                 :routed-edges initial-routed-edges
                 :viewport-height viewport-height
                 :viewport-width width})
       :draw draw-scene
       :key-pressed handle-key-pressed
       :mouse-wheel handle-mouse-wheel
       :mouse-pressed handle-mouse-pressed
       :mouse-dragged handle-mouse-dragged
        :mouse-released handle-mouse-released
        :middleware [m/fun-mode]))))

(defn- safe-looping?
  [sketch]
  (try
    (.isLooping sketch)
    (catch Throwable _
      false)))

(defn- safe-displayable?
  [sketch fallback]
  (try
    (let [native (some-> sketch .getSurface .getNative)]
      (if (instance? java.awt.Component native)
        (.isDisplayable ^java.awt.Component native)
        fallback))
    (catch Throwable _
      fallback)))

(defn wait-until-closed!
  [sketch]
  (when sketch
    (loop []
      (let [looping? (safe-looping? sketch)
            displayable? (safe-displayable? sketch looping?)]
        (when (and looping? displayable?)
        (Thread/sleep 100)
        (recur))))))
