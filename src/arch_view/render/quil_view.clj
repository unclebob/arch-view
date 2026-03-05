(ns arch-view.render.quil-view
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [quil.core :as q]
            [quil.middleware :as m])
  (:import [javax.swing JFrame JEditorPane JScrollPane SwingUtilities]
           [java.awt Dimension]
           [java.util.regex Pattern]))

(defn- layer-y
  [index layer-height layer-gap]
  (* index (+ layer-height layer-gap)))

(defn- dominant-component
  [modules module->component]
  (->> modules
       (map module->component)
       (remove nil?)
       frequencies
       (sort-by (juxt (comp - val) (comp str key)))
       ffirst))

(defn- module-positions-for-layer
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

(defn abbreviate-module-name
  [module]
  (let [parts (->> (str/split module #"\.")
                   (drop 1)
                   vec)
        parent (butlast parts)
        last-part (last parts)]
    (if (and (seq parent) last-part)
      (str (str/join "." (map #(subs % 0 1) parent))
           "."
           last-part)
      (or last-part module))))

(defn strip-top-namespace
  [module]
  (let [parts (str/split module #"\.")]
    (if (> (count parts) 1)
      (str/join "." (rest parts))
      module)))

(defn- label-width
  [label]
  (* 7.0 (count (or label ""))))

(defn- rendered-label
  [{:keys [display-label label]}]
  (or display-label label))

(def ^:private toolbar-height 38.0)
(def ^:private back-button-width 80.0)
(def ^:private declutter-button-width 240.0)
(def ^:private button-height 26.0)

(def declutter-modes [:all :concrete :abstract :between-layers])

(defn next-declutter-mode
  [mode]
  (let [idx (.indexOf ^java.util.List declutter-modes (or mode :all))
        current (if (neg? idx) 0 idx)
        next-idx (mod (inc current) (count declutter-modes))]
    (nth declutter-modes next-idx)))

(defn declutter-label
  [mode]
  (case mode
    :concrete "Declutter: Concrete"
    :abstract "Declutter: Abstract"
    :between-layers "Declutter: Layers"
    "Declutter: All"))

(defn- back-button-rect
  []
  {:x 10.0 :y 6.0 :width back-button-width :height button-height})

(defn- declutter-button-rect
  []
  {:x 100.0 :y 6.0 :width declutter-button-width :height button-height})

(defn- overlap?
  [a b]
  (< (Math/abs (double (- (:x a) (:x b))))
     (/ (+ (label-width (rendered-label a))
           (label-width (rendered-label b)))
        2.0)))

(defn- needs-stagger?
  [modules]
  (boolean
    (some true?
          (map overlap?
               modules
               (rest modules)))))

(defn- stagger-offset
  [idx]
  (let [step 10.0]
    (cond
      (even? idx) 0.0
      (odd? idx) step)))

(defn- apply-layer-stagger
  [modules]
  (if (needs-stagger? modules)
    (map-indexed (fn [idx m]
                   (update m :y + (stagger-offset idx)))
                 modules)
    modules))

(defn- arrowhead-for
  [edge-type]
  (if (= :abstract edge-type)
    :closed-triangle
    :standard))

(def ^:private racetrack-count 4)
(def ^:private racetrack-margin 24.0)
(def ^:private racetrack-gap 24.0)

(defn- track-width-for
  [canvas-width]
  (/ (- (double canvas-width)
        (* 2.0 racetrack-margin)
        (* (dec racetrack-count) racetrack-gap))
     racetrack-count))

(defn- track-x-for
  [track canvas-width]
  (+ racetrack-margin
     (* track (+ (track-width-for canvas-width) racetrack-gap))))

(defn- dependency-pairs-by-layer
  [classified-edges module->layer]
  (->> classified-edges
       (keep (fn [{:keys [from to]}]
               (let [from-layer (get module->layer from)
                     to-layer (get module->layer to)]
                 (when (and (number? from-layer)
                            (number? to-layer))
                   [from-layer to-layer]))))
       vec))

(defn- assign-layer-slots
  [ordered-layer-indexes pairs]
  (let [pair-index (group-by first pairs)
        reverse-index (group-by second pairs)]
    (loop [remaining ordered-layer-indexes
           placement {}
           max-row -1]
      (if (empty? remaining)
        placement
        (let [idx (first remaining)
              candidates (for [row-candidate (range (inc (+ 2 max-row)))
                               track-candidate (range racetrack-count)
                               :let [occupied? (some (fn [{:keys [row track]}]
                                                       (and (= row row-candidate)
                                                            (= track track-candidate)))
                                                     (vals placement))]
                               :when (not occupied?)]
                           {:row row-candidate :track track-candidate})
              connection-cost
              (fn [{:keys [row track]}]
                (let [outgoing (for [[_ to] (get pair-index idx)
                                     :let [p (get placement to)]
                                     :when p]
                                 [p true])
                      incoming (for [[from _] (get reverse-index idx)
                                     :let [p (get placement from)]
                                     :when p]
                                 [p false])]
                  (reduce (fn [cost [{other-row :row other-track :track} as-from?]]
                            (let [horizontal (Math/abs (double (- track other-track)))
                                  vertical (Math/abs (double (- row other-row)))
                                  upward? (if as-from?
                                            (< other-row row)
                                            (< row other-row))]
                              (+ cost
                                 (* 9.0 horizontal)
                                 (* 2.0 vertical)
                                 (if upward? 18.0 0.0))))
                          0.0
                          (concat outgoing incoming))))
              best (reduce (fn [best candidate]
                             (let [height-cost (* 100.0 (:row candidate))
                                   score (+ height-cost (connection-cost candidate))]
                               (if (or (nil? best) (< score (:score best)))
                                 (assoc candidate :score score)
                                 best)))
                           nil
                           candidates)
              next-placement (assoc placement idx {:row (:row best) :track (:track best)})]
          (recur (rest remaining)
                 next-placement
                 (max max-row (:row best))))))))

(defn build-scene
  ([architecture]
   (build-scene architecture {}))
  ([architecture {:keys [canvas-width layer-height layer-gap]
                  :or {canvas-width 1200 layer-height 140 layer-gap 24}}]
  (let [layers (get-in architecture [:layout :layers])
         layer-indexes (->> layers (map :index) sort vec)
         module->layer (or (get-in architecture [:layout :module->layer]) {})
         layer-pairs (dependency-pairs-by-layer (:classified-edges architecture) module->layer)
         placement-by-layer-index (assign-layer-slots layer-indexes layer-pairs)
         module->component (or (:module->component architecture) {})
         layer->label (or (:layer->label architecture) {})
         module->kind (or (:module->kind architecture) {})
         module->leaf? (or (:module->leaf? architecture) {})
         module->source-file (or (:module->source-file architecture) {})
         module->full-name (or (:module->full-name architecture) {})
         module->display-label (or (:module->display-label architecture) {})
         layer-rects (mapv (fn [{:keys [index]}]
                             (let [modules (get-in architecture [:layout :layers index :modules])
                                   component (dominant-component modules module->component)
                                   abstract-layer? (boolean (some #(= :abstract (get module->kind %))
                                                                  modules))]
                               {:index index
                                :x (track-x-for (get-in placement-by-layer-index [index :track] 0) canvas-width)
                                :y (layer-y (get-in placement-by-layer-index [index :row] index) layer-height layer-gap)
                                :width (track-width-for canvas-width)
                                :height layer-height
                                :abstract? abstract-layer?
                                :full-name (some-> modules first module->full-name strip-top-namespace)
                                :label (or (get layer->label index)
                                           (if component
                                         (name component)
                                         (str "layer-" index)))}))
                           layers)
         rect-by-layer (into {} (map (juxt :index identity) layer-rects))
         module-positions (->> layers
                               (mapcat (fn [{:keys [index modules]}]
                                         (->> (module-positions-for-layer index modules (get rect-by-layer index) module->kind module->full-name)
                                              (map (fn [m]
                                                     (assoc m
                                                            :leaf? (boolean (get module->leaf? (:module m)))
                                                            :source-file (get module->source-file (:module m))
                                                            :label (or (get module->display-label (:module m))
                                                                       (abbreviate-module-name (:module m)))
                                                            :full-name (strip-top-namespace (:full-name m)))))
                                              apply-layer-stagger)))
                               vec)
         edge-drawables (->> (:classified-edges architecture)
                             (map (fn [{:keys [from to type]}]
                                    {:from from
                                     :to to
                                     :arrowhead (arrowhead-for type)
                                     :type type}))
                             vec)]
     {:layer-rects layer-rects
      :module-positions module-positions
      :edge-drawables edge-drawables})))

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
  [x1 y1 x2 y2 arrowhead]
  (let [[sx sy] (dependency-start-point x1 y1 x2 y2)
        [tx ty] (dependency-tip-point x1 y1 x2 y2)
        [ex ey] (edge-line-endpoint sx sy tx ty arrowhead)]
    (if (= :closed-triangle arrowhead)
      (q/stroke 0 128 0)
      (q/stroke 0 0 0))
    (q/line sx sy ex ey)
    (draw-arrowhead sx sy tx ty arrowhead)))

(defn- clamp-point
  [[x y] {:keys [min-x max-x min-y max-y]}]
  [(-> x double (max min-x) (min max-x))
   (-> y double (max min-y) (min max-y))])

(defn- draw-edge
  [points bounds {:keys [from to from-point to-point arrowhead preserve-endpoints? parallel-offset-x parallel-offset-y]}]
  (let [offset-x (double (or parallel-offset-x 0.0))
        offset-y (double (or parallel-offset-y 0.0))
        [x1 y1] (or from-point (let [{x :x y :y} (get points from)] [x y]))
        [x2 y2] (or to-point (let [{x :x y :y} (get points to)] [x y]))
        x1 (+ (double x1) offset-x)
        x2 (+ (double x2) offset-x)
        y1 (+ (double y1) offset-y)
        y2 (+ (double y2) offset-y)
        [x1 y1] (clamp-point [x1 y1] bounds)
        [x2 y2] (clamp-point [x2 y2] bounds)]
    (when (and x1 y1 x2 y2)
      (if preserve-endpoints?
        (let [[ex ey] (edge-line-endpoint x1 y1 x2 y2 arrowhead)]
          (if (= :closed-triangle arrowhead)
            (q/stroke 0 128 0)
            (q/stroke 0 0 0))
          (q/line x1 y1 ex ey)
          (draw-arrowhead x1 y1 x2 y2 arrowhead))
        (draw-arrow-between-points x1 y1 x2 y2 arrowhead)))))

(defn layer-edge-drawables
  [scene]
  (let [module->layer (into {}
                           (map (fn [{:keys [module layer]}] [module layer])
                                (:module-positions scene)))
        layer->module-centers (reduce (fn [acc {:keys [layer x y]}]
                                        (update acc layer (fnil conj []) [x y]))
                                      {}
                                      (:module-positions scene))
        layer-center (fn [layer]
                       (let [points (get layer->module-centers layer)]
                         (if (seq points)
                           (let [count-points (double (count points))
                                 sum-x (reduce + (map first points))
                                 sum-y (reduce + (map second points))]
                             [(/ sum-x count-points) (/ sum-y count-points)])
                           nil)))
        layer-rects (into {}
                         (map (fn [r] [(:index r) r]) (:layer-rects scene)))
        edges-by-layer (reduce (fn [acc {:keys [from to type]}]
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
                               (:edge-drawables scene))
        base-edges (->> edges-by-layer
                        (map (fn [[[from-layer to-layer] type]]
                               (let [{fx :x fy :y fw :width fh :height} (get layer-rects from-layer)
                                     {tx :x ty :y tw :width th :height} (get layer-rects to-layer)
                                     [from-x from-y] (or (layer-center from-layer)
                                                         [(+ fx (/ fw 2.0))
                                                          (+ fy (/ fh 2.0))])
                                     [to-x to-y] (or (layer-center to-layer)
                                                     [(+ tx (/ tw 2.0))
                                                      (+ ty (/ th 2.0))])]
                                 {:from from-layer
                                  :to to-layer
                                  :from-point [from-x from-y]
                                  :to-point [to-x to-y]
                                  :type type
                                  :arrowhead (arrowhead-for type)})))
                        (sort-by (juxt :from :to))
                        vec)]
    (letfn [(span [{:keys [from-point to-point]}]
              (let [[_ y1] from-point
                    [_ y2] to-point]
                [(min y1 y2) (max y1 y2)]))
            (overlap? [[a0 a1] [b0 b1]]
              (< (max a0 b0) (min a1 b1)))
            (assign-lane [lanes edge]
              (let [[start end] (span edge)
                    lane-idx (or (first (keep-indexed (fn [idx lane-end]
                                                        (when (< lane-end start) idx))
                                                      lanes))
                                 (count lanes))]
                (if (< lane-idx (count lanes))
                  {:lane-idx lane-idx
                   :lanes (assoc lanes lane-idx end)}
                  {:lane-idx lane-idx
                   :lanes (conj lanes end)})))
            (offset-for-lane [lane lane-count]
              (* 15.0 (- lane (/ (dec lane-count) 2.0))))]
      (let [{:keys [edges max-lane]}
            (reduce (fn [{:keys [lanes edges max-lane]} edge]
                      (let [{:keys [lane-idx lanes]} (assign-lane lanes edge)
                            [x1 y1] (:from-point edge)
                            [x2 y2] (:to-point edge)]
                        {:lanes lanes
                         :edges (conj edges (assoc edge :lane lane-idx))
                         :max-lane (max max-lane lane-idx)}))
                    {:lanes [] :edges [] :max-lane -1}
                    base-edges)
            lane-count (inc (max 0 max-lane))]
        (mapv (fn [{:keys [lane from-point to-point] :as edge}]
                (let [offset (offset-for-lane lane lane-count)
                      [x1 y1] from-point
                      [x2 y2] to-point
                      horizontal? (>= (Math/abs (- x2 x1))
                                      (Math/abs (- y2 y1)))]
                  (assoc edge
                         :from-point (if horizontal?
                                       [x1 (+ y1 offset)]
                                       [(+ x1 offset) y1])
                         :to-point (if horizontal?
                                     [x2 (+ y2 offset)]
                                     [(+ x2 offset) y2]))))
              edges)))))

(defn declutter-edge-drawables
  [scene mode]
  (case mode
    :concrete (->> (:edge-drawables scene)
                   (filter #(= :direct (:type %)))
                   vec)
    :abstract (->> (:edge-drawables scene)
                   (filter #(= :abstract (:type %)))
                   vec)
    :between-layers (layer-edge-drawables scene)
    (:edge-drawables scene)))

(defn apply-parallel-arrow-spacing
  [edge-drawables points]
  (letfn [(base-points [{:keys [from to from-point to-point]}]
            (let [[x1 y1] (or from-point (let [{x :x y :y} (get points from)] [x y]))
                  [x2 y2] (or to-point (let [{x :x y :y} (get points to)] [x y]))]
              [x1 y1 x2 y2]))
          (span [edge]
            (let [[_ y1 _ y2] (base-points edge)]
              [(min y1 y2) (max y1 y2)]))
          (assign-lane [lanes edge]
            (let [[start end] (span edge)
                  lane-idx (or (first (keep-indexed (fn [idx lane-end]
                                                      (when (< lane-end start) idx))
                                                    lanes))
                               (count lanes))]
              (if (< lane-idx (count lanes))
                {:lane lane-idx :lanes (assoc lanes lane-idx end)}
                {:lane lane-idx :lanes (conj lanes end)})))
          (offset-for-lane [lane lane-count]
            (* 15.0 (- lane (/ (dec lane-count) 2.0))))]
    (let [{:keys [edges max-lane]}
          (reduce (fn [{:keys [lanes edges max-lane]} edge]
                    (let [{:keys [lane lanes]} (assign-lane lanes edge)]
                      {:lanes lanes
                       :edges (conj edges (assoc edge :lane lane))
                       :max-lane (max max-lane lane)}))
                  {:lanes [] :edges [] :max-lane -1}
                  edge-drawables)
          lane-count (inc (max 0 max-lane))]
      (mapv (fn [{:keys [lane] :as edge}]
              (let [[x1 y1 x2 y2] (base-points edge)
                    offset (offset-for-lane lane lane-count)
                    horizontal? (>= (Math/abs (- x2 x1))
                                    (Math/abs (- y2 y1)))]
                (if horizontal?
                  (assoc edge :parallel-offset-y offset)
                  (assoc edge :parallel-offset-x offset))))
            edges))))

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

(declare view-architecture drilldown-scene push-nav-state)

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

(defn- point-in-rect?
  [{:keys [x y width height]} px py]
  (and (<= x px (+ x width))
       (<= y py (+ y height))))

(defn- content-height-for-scene
  [scene]
  (->> (:layer-rects scene)
       (map (fn [{:keys [y height]}] (+ y height)))
       (apply max 0)
       (+ 40)))

(defn- content-width-for-scene
  [scene]
  (->> (:layer-rects scene)
       (map (fn [{:keys [x width]}] (+ x width)))
       (apply max 0)
       (+ racetrack-margin 20.0)))

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
  [scene declutter-mode viewport-width viewport-height]
  (q/background 250 250 250)
  (doseq [{:keys [x y width height label abstract?]} (:layer-rects scene)]
    (if abstract?
      (q/fill 226 242 226)
      (q/fill 225 233 242))
    (q/stroke 120 140 160)
    (q/rect x y width height)
    (q/fill 45 60 80)
    (q/text-align :left :top)
    (q/text label (+ x 8) (+ y 6)))
  (doseq [{:keys [x y kind] :as module-position} (:module-positions scene)]
    (if (= :abstract kind)
      (q/fill 0 128 0)
      (q/fill 15 20 30))
    (q/no-stroke)
    (q/text-align :center :center)
    (q/text (rendered-label module-position) x y))
  (let [points (module-point-map scene)
        edge-drawables (declutter-edge-drawables scene declutter-mode)
        spaced-edges (if (= :between-layers declutter-mode)
                       edge-drawables
                       (apply-parallel-arrow-spacing edge-drawables points))
        bounds {:min-x 14.0
                :max-x (- (double viewport-width) 20.0)
                :min-y 14.0
                :max-y (- (content-height-for-scene scene) 14.0)}]
    (doseq [edge spaced-edges]
      (draw-edge points bounds edge))))

(defn- draw-toolbar
  [{:keys [namespace-path declutter-mode]}]
  (let [back-rect (back-button-rect)
        declutter-rect (declutter-button-rect)
        can-go-back? (seq namespace-path)]
    (q/no-stroke)
    (q/fill 238 242 246)
    (q/rect 0 0 3000 toolbar-height)
    (q/fill (if can-go-back? 225 205))
    (q/rect (:x back-rect) (:y back-rect) (:width back-rect) (:height back-rect))
    (if can-go-back?
      (q/fill 0 0 0)
      (q/fill 120 120 120))
    (q/text-align :center :center)
    (q/text "Back" (+ (:x back-rect) (/ (:width back-rect) 2.0)) (+ (:y back-rect) (/ (:height back-rect) 2.0)))
    (q/fill 225)
    (q/rect (:x declutter-rect) (:y declutter-rect) (:width declutter-rect) (:height declutter-rect))
    (q/fill 0 0 0)
    (q/text-align :center :center)
    (q/text (declutter-label declutter-mode)
            (+ (:x declutter-rect) (/ (:width declutter-rect) 2.0))
            (+ (:y declutter-rect) (/ (:height declutter-rect) 2.0)))))

(defn- draw-tooltip
  [full-name mx my]
  (q/fill 255 255 225)
  (q/stroke 80 80 80)
  (q/rect (+ mx 12) (+ my 12) (+ 12 (* 7 (count full-name))) 20)
  (q/fill 0 0 0)
  (q/no-stroke)
  (q/text-align :left :center)
  (q/text full-name (+ mx 18) (+ my 22)))

(defn- html-escape
  [s]
  (-> (or s "")
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

(defn- colorize-clojure-html
  [source]
  (let [escaped (html-escape source)
        comment-pattern (Pattern/compile "(?m);.*$")
        string-pattern (Pattern/compile "\"([^\"\\\\]|\\\\.)*\"")
        keyword-pattern (Pattern/compile ":[a-zA-Z0-9\\-\\?!_\\./]+")
        apply-style (fn [text pattern class-name]
                      (let [matcher (.matcher pattern text)
                            sb (StringBuffer.)]
                        (loop []
                          (if (.find matcher)
                            (do
                              (.appendReplacement matcher sb (str "<span class='" class-name "'>$0</span>"))
                              (recur))
                            (do
                              (.appendTail matcher sb)
                              (str sb))))))]
    (-> escaped
        (apply-style string-pattern "str")
        (apply-style keyword-pattern "kw")
        (apply-style comment-pattern "cmt"))))

(defn- expand-tabs
  [line]
  (str/replace (or line "") "\t" "  "))

(defn- source-lines->html
  [source]
  (let [lines (str/split (or source "") #"\r?\n" -1)]
    (->> lines
         (map-indexed (fn [idx line]
                        (let [line-html (colorize-clojure-html (expand-tabs line))
                              visible-line (if (str/blank? line-html) "&nbsp;" line-html)]
                          (str "<tr>"
                               "<td class='ln'>" (inc idx) "</td>"
                               "<td class='code'><pre>" visible-line "</pre></td>"
                               "</tr>"))))
         (apply str))))

(defn- source->html
  [title source]
  (str "<html><head><style>"
       "body{margin:0;padding:0;background:#f8fafc;color:#111827;font-family:Menlo,Monaco,Consolas,monospace;}"
       ".hdr{padding:10px 12px;background:#e5e7eb;border-bottom:1px solid #cbd5e1;font-family:sans-serif;font-size:13px;}"
       ".src{padding:0;line-height:1.35;font-size:13px;}"
       ".src table{border-collapse:collapse;width:100%;}"
       ".ln{width:52px;padding:0 10px;background:#eef2f7;color:#6b7280;text-align:right;vertical-align:top;"
       "border-right:1px solid #d1d5db;user-select:none;}"
       ".code{padding:0 12px;vertical-align:top;}"
       ".code pre{margin:0;white-space:pre;tab-size:2;}"
       ".cmt{color:#6b7280;}"
       ".str{color:#b45309;}"
       ".kw{color:#1d4ed8;}"
       "</style></head><body>"
       "<div class='hdr'>" (html-escape title) "</div>"
       "<div class='src'><table>" (source-lines->html source) "</table></div>"
       "</body></html>"))

(defn open-source-file-window!
  [source-file]
  (when (and source-file (.exists (io/file source-file)))
    (let [content (slurp source-file)
          title (.getName (io/file source-file))]
      (SwingUtilities/invokeLater
        (fn []
          (let [frame (JFrame. (str "Source: " title))
                editor (JEditorPane. "text/html" (source->html source-file content))
                scroll (JScrollPane. editor)]
            (.setEditable editor false)
            (.setCaretPosition editor 0)
            (.setPreferredSize scroll (Dimension. 980 760))
            (.add (.getContentPane frame) scroll)
            (.pack frame)
            (.setLocationByPlatform frame true)
            (.setVisible frame true)))))))

(defn- draw-scrollbar
  [content-height viewport-height scroll-y viewport-width]
  (when-let [{:keys [x y width height]} (scrollbar-rect content-height viewport-height scroll-y viewport-width)]
    (q/no-stroke)
    (q/fill 220 220 220)
    (q/rect (- x 1.0) 10.0 (+ width 2.0) (- viewport-height 20.0))
    (q/fill 120 120 120)
    (q/rect x y width height)))

(defn- draw-scene
  [{:keys [scene declutter-mode scroll-y viewport-height viewport-width] :as state}]
  (let [content-height (content-height-for-scene scene)
        mx (double (q/mouse-x))
        my (double (q/mouse-y))
        world-my (+ my scroll-y)
        hovered (hovered-module-position (:module-positions scene) mx world-my)
        hovered-layer (hovered-layer-label (:layer-rects scene) mx world-my)]
    (q/cursor (if (:drillable? hovered)
                :cross
                :arrow))
    (q/background 250 250 250)
    (q/push-matrix)
    (q/translate 0 (- scroll-y))
    (draw-scene-content scene declutter-mode viewport-width viewport-height)
    (q/pop-matrix)
    (draw-toolbar state)
    (cond
      hovered (draw-tooltip (:full-name hovered) mx my)
      (:full-name hovered-layer) (draw-tooltip (:full-name hovered-layer) mx my))
    (draw-scrollbar content-height viewport-height scroll-y viewport-width)))

(defn handle-key-pressed
  [state event]
  (when (= :escape (:key event))
    (q/exit))
  state)

(defn handle-mouse-wheel
  [{:keys [scene scroll-y viewport-height viewport-width] :as state} event]
  (let [content-height (content-height-for-scene scene)
        units (cond
                (number? event) (double event)
                (map? event) (double (or (:count event)
                                         (:amount event)
                                         (:rotation event)
                                         (:wheel-rotation event)
                                         0))
                :else 0.0)
        delta (* 30.0 units)
        next-scroll (clamp-scroll (+ scroll-y delta) content-height viewport-height)]
    (assoc state
           :scroll-y next-scroll
           :viewport-height viewport-height
           :viewport-width viewport-width)))

(defn- mouse-pos
  [event]
  [(double (or (:x event) (q/mouse-x)))
   (double (or (:y event) (q/mouse-y)))])

(defn handle-mouse-pressed
  [{:keys [scene scroll-y viewport-height viewport-width] :as state} event]
  (let [content-height (content-height-for-scene scene)
        thumb (scrollbar-rect content-height viewport-height scroll-y viewport-width)
        [mx my] (mouse-pos event)]
    (if (and thumb (point-in-rect? thumb mx my))
      (assoc state
             :dragging-scrollbar? true
             :drag-offset (- my (:y thumb)))
      state)))

(defn handle-mouse-dragged
  [{:keys [scene viewport-height dragging-scrollbar? drag-offset] :as state} event]
  (if-not dragging-scrollbar?
    state
    (let [content-height (content-height-for-scene scene)
          [_ mouse-y] (mouse-pos event)
          thumb-y (- mouse-y (double (or drag-offset 0)))
          next-scroll (thumb-y->scroll thumb-y content-height viewport-height)]
      (assoc state :scroll-y next-scroll))))

(defn- apply-drilldown-click
  [{:keys [scene scroll-y namespace-path] :as state} event]
  (let [[mx my] (mouse-pos event)
        world-my (+ my scroll-y)
        hovered (hovered-module-position (:module-positions scene) mx world-my)]
    (if hovered
      (let [candidate (conj (or namespace-path []) (:module hovered))
            child-view (view-architecture (:architecture state) candidate)]
        (if (seq (get-in child-view [:graph :nodes]))
          (-> state
              push-nav-state
              (drilldown-scene candidate 0.0))
          (do
            (when-let [source-file (:source-file hovered)]
              (open-source-file-window! source-file))
            state)))
      state)))

(defn- navigate-up
  [{:keys [nav-stack] :as state}]
  (let [stack (vec (or nav-stack []))]
    (if (seq stack)
      (let [{:keys [path scroll-y]} (peek stack)]
        (-> state
            (assoc :nav-stack (pop stack))
            (drilldown-scene (vec (or path [])) scroll-y)))
      state)))

(defn- toolbar-click-target
  [state mx my]
  (cond
    (and (seq (:namespace-path state))
         (point-in-rect? (back-button-rect) mx my)) :back
    (point-in-rect? (declutter-button-rect) mx my) :declutter
    :else nil))

(defn- point-in-toolbar?
  [mx my]
  (and (>= mx 0.0)
       (<= my toolbar-height)))

(defn- apply-toolbar-click
  [state event]
  (let [[mx my] (mouse-pos event)]
    (if-not (point-in-toolbar? mx my)
      nil
      (case (toolbar-click-target state mx my)
      :back (navigate-up state)
      :declutter (update state :declutter-mode next-declutter-mode)
      state))))

(defn handle-mouse-released
  [{:keys [dragging-scrollbar?] :as state} event]
  (if dragging-scrollbar?
    (assoc state :dragging-scrollbar? false :drag-offset nil)
    (let [base-state (assoc state :dragging-scrollbar? false :drag-offset nil)]
      (or (apply-toolbar-click base-state event)
          (apply-drilldown-click base-state event)))))

(defn handle-mouse-clicked
  [state event]
  (if (:dragging-scrollbar? state)
    state
    (or (apply-toolbar-click state event)
        (apply-drilldown-click state event))))

(defn- namespace-segments
  [module]
  (vec (rest (str/split module #"\."))))

(defn- source-filename
  [path]
  (when path
    (.getName (io/file path))))

(defn- prefix?
  [prefix parts]
  (and (<= (count prefix) (count parts))
       (= prefix (subvec parts 0 (count prefix)))))

(defn- sink-first-order
  [nodes edges]
  (let [outgoing (reduce (fn [acc {:keys [from to]}]
                           (update acc from (fnil conj #{}) to))
                         {}
                         edges)
        incoming (reduce (fn [acc {:keys [from to]}]
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
        (let [n (if (seq available)
                  (first available)
                  (first (sort remaining)))
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

(defn- edge-layout-cost
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

(defn- optimize-order
  [initial-order edges]
  (loop [order (vec initial-order)]
    (let [base-cost (edge-layout-cost order edges)
          swaps (for [i (range (dec (count order)))]
                  (let [swapped (-> order
                                    (assoc i (nth order (inc i)))
                                    (assoc (inc i) (nth order i)))
                        swapped-cost (edge-layout-cost swapped edges)]
                    {:order swapped :cost swapped-cost}))]
      (if-let [{:keys [order cost]}
               (->> swaps
                    (filter #(< (:cost %) base-cost))
                    first)]
        (recur order)
        order))))

(defn- namespace-layout
  [nodes edges]
  (let [ordered (-> (sink-first-order nodes edges)
                    (optimize-order edges)
                    vec)
        layers (mapv (fn [idx module]
                       {:index idx
                        :modules [module]})
                     (range)
                     ordered)
        module->layer (into {}
                            (map (fn [idx module]
                                   [module idx])
                                 (range)
                                 ordered))]
    {:layers layers
     :module->layer module->layer}))

(defn view-architecture
  [architecture namespace-path]
  (let [all-modules (or (get-in architecture [:graph :nodes]) #{})
        module->source-file-all (or (get-in architecture [:graph :module->source-file]) {})
        scoped-modules (->> all-modules
                            (filter (fn [m]
                                      (let [parts (namespace-segments m)]
                                        (and (prefix? namespace-path parts)
                                             (> (count parts) (count namespace-path))))))
                            set)
        module->child (into {}
                           (for [m scoped-modules
                                 :let [parts (namespace-segments m)]]
                             [m (nth parts (count namespace-path))]))
        modules-by-child (reduce (fn [acc module]
                                   (update acc (get module->child module) (fnil conj #{}) module))
                                 {}
                                 scoped-modules)
        nodes (set (vals module->child))
        abstract-source (or (get-in architecture [:graph :abstract-modules]) #{})
        module->kind (into {}
                          (for [n nodes]
                            [n (if (some #(and (= n (get module->child %))
                                               (contains? abstract-source %))
                                         scoped-modules)
                                 :abstract
                                 :concrete)]))
        module->leaf? (into {}
                            (for [n nodes
                                  :let [modules (get modules-by-child n)
                                        leaf? (every? (fn [m]
                                                        (= (count (namespace-segments m))
                                                           (inc (count namespace-path))))
                                                      modules)]]
                              [n leaf?]))
        module->source-file (into {}
                                 (for [n nodes
                                       :let [modules (get modules-by-child n)
                                             exact-module (some (fn [m]
                                                                  (when (= (namespace-segments m)
                                                                           (conj (vec namespace-path) n))
                                                                    m))
                                                                modules)
                                             source-file (get module->source-file-all exact-module)]]
                                   [n source-file]))
        module->display-label (into {}
                                    (for [n nodes
                                          :let [leaf? (true? (get module->leaf? n))
                                                source-file (get module->source-file n)]]
                                      [n (if leaf?
                                           (or (source-filename source-file) n)
                                           n)]))
        module->full-name (into {}
                               (for [n nodes]
                                 [n (str/join "." (concat namespace-path [n]))]))
        edges-by-pair (reduce (fn [acc {:keys [from to type]}]
                                (let [f (get module->child from)
                                      t (get module->child to)]
                                  (if (and f t (not= f t))
                                    (update acc [f t] (fn [old]
                                                        (if (or (= old :abstract) (= type :abstract))
                                                          :abstract
                                                          :direct)))
                                    acc)))
                              {}
                              (:classified-edges architecture))
        classified-edges (set (for [[[f t] type] edges-by-pair]
                                {:from f :to t :type type}))
        graph {:nodes nodes
               :edges (set (for [{:keys [from to]} classified-edges]
                             {:from from :to to}))
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
     :module->kind module->kind
     :module->leaf? module->leaf?
     :module->source-file module->source-file
     :module->display-label module->display-label
     :module->full-name module->full-name}))

(defn- drilldown-scene
  [state path scroll-y]
  (let [view (view-architecture (:architecture state) path)
        scene (-> (build-scene view)
                  (attach-drillable-markers (:architecture state) path))]
    (assoc state
           :namespace-path path
           :scroll-y (double (or scroll-y 0.0))
           :scene scene)))

(defn initial-scene-for-show
  [scene architecture]
  (if architecture
    (let [initial-view (view-architecture architecture [])]
      (-> (build-scene initial-view)
          (attach-drillable-markers architecture [])))
    scene))

(defn- push-nav-state
  [{:keys [namespace-path scroll-y nav-stack] :as state}]
  (assoc state :nav-stack (conj (vec (or nav-stack []))
                                {:path (vec (or namespace-path []))
                                 :scroll-y (double (or scroll-y 0.0))})))

(defn show!
  ([scene]
   (show! scene {}))
  ([scene {:keys [title architecture]
           :or {title "architecture-viewer"}}]
   (let [effective-architecture (or architecture {:scene scene})
         initial-scene (initial-scene-for-show scene architecture)
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
                 :scroll-y 0.0
                 :dragging-scrollbar? false
                 :drag-offset nil
                 :viewport-height viewport-height
                 :viewport-width width})
       :draw draw-scene
       :key-pressed handle-key-pressed
       :mouse-wheel handle-mouse-wheel
       :mouse-pressed handle-mouse-pressed
       :mouse-dragged handle-mouse-dragged
       :mouse-released handle-mouse-released
        :middleware [m/fun-mode]))))

(defn wait-until-closed!
  [sketch]
  (when sketch
    (loop []
      (let [looping? (try
                       (.isLooping sketch)
                       (catch Throwable _
                         false))
            displayable? (try
                           (let [native (some-> sketch .getSurface .getNative)]
                             (if (instance? java.awt.Component native)
                               (.isDisplayable ^java.awt.Component native)
                               looping?))
                           (catch Throwable _
                             looping?))]
        (when (and looping? displayable?)
        (Thread/sleep 100)
        (recur))))))
