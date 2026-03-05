(ns arch-view.render.quil-view
  (:require [clojure.string :as str]
            [quil.core :as q]
            [quil.middleware :as m]))

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
  [layer-index modules canvas-width layer-height layer-gap]
  (let [count-modules (count modules)
        spacing (/ canvas-width (inc count-modules))
        y (+ (layer-y layer-index layer-height layer-gap) (/ layer-height 2))]
    (map-indexed (fn [idx module]
                   {:module module
                    :layer layer-index
                    :x (* (inc idx) spacing)
                   :y y})
                 modules)))

(defn abbreviate-module-name
  [module]
  (let [parts (str/split module #"\.")
        parent (butlast parts)
        last-part (last parts)]
    (if (seq parent)
      (str (str/join "." (map #(subs % 0 1) parent))
           "."
           last-part)
      module)))

(defn- arrowhead-for
  [edge-type]
  (if (= :abstract edge-type)
    :closed-triangle
    :standard))

(defn build-scene
  ([architecture]
   (build-scene architecture {}))
  ([architecture {:keys [canvas-width layer-height layer-gap]
                  :or {canvas-width 1200 layer-height 140 layer-gap 24}}]
   (let [layers (get-in architecture [:layout :layers])
         module->component (or (:module->component architecture) {})
         layer-rects (mapv (fn [{:keys [index]}]
                             (let [modules (get-in architecture [:layout :layers index :modules])
                                   component (dominant-component modules module->component)]
                               {:index index
                                :x 0
                                :y (layer-y index layer-height layer-gap)
                                :width canvas-width
                                :height layer-height
                                :label (if component
                                         (name component)
                                         (str "layer-" index))}))
                           layers)
         module-positions (->> layers
                               (mapcat (fn [{:keys [index modules]}]
                                         (module-positions-for-layer index modules canvas-width layer-height layer-gap)))
                               (map (fn [m]
                                      (assoc m :label (abbreviate-module-name (:module m))))
                               )
                               vec)
         edge-drawables (->> (:classified-edges architecture)
                             (map (fn [{:keys [from to type]}]
                                    {:from from
                                     :to to
                                     :arrowhead (arrowhead-for type)}))
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

(defn- draw-edge
  [points {:keys [from to arrowhead]}]
  (let [{x1 :x y1 :y} (get points from)
        {x2 :x y2 :y} (get points to)]
    (when (and x1 y1 x2 y2)
      (let [[sx sy] (dependency-start-point x1 y1 x2 y2)
            [tx ty] (dependency-tip-point x1 y1 x2 y2)
            [ex ey] (edge-line-endpoint sx sy tx ty arrowhead)]
      (q/stroke 40 40 40)
      (q/line sx sy ex ey)
      (draw-arrowhead sx sy tx ty arrowhead)))))

(defn- label-hitbox
  [{:keys [x y label]}]
  (let [width (* 7.0 (count (or label "")))
        half-w (/ width 2.0)
        half-h 7.0]
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

(defn- draw-scene
  [scene]
  (q/background 250 250 250)
  (doseq [{:keys [x y width height label]} (:layer-rects scene)]
    (q/fill 225 233 242)
    (q/stroke 120 140 160)
    (q/rect x y width height)
    (q/fill 45 60 80)
    (q/text-align :left :top)
    (q/text label (+ x 8) (+ y 6)))
  (doseq [{:keys [x y label]} (:module-positions scene)]
    (q/fill 15 20 30)
    (q/no-stroke)
    (q/text-align :center :center)
    (q/text label x y))
  (let [points (module-point-map scene)
        mx (q/mouse-x)
        my (q/mouse-y)
        hovered (hovered-module (:module-positions scene) mx my)]
    (doseq [edge (:edge-drawables scene)]
      (draw-edge points edge))
    (when hovered
      (q/fill 255 255 225)
      (q/stroke 80 80 80)
      (q/rect (+ mx 12) (+ my 12) (+ 12 (* 7 (count hovered))) 20)
      (q/fill 0 0 0)
      (q/no-stroke)
      (q/text-align :left :center)
      (q/text hovered (+ mx 18) (+ my 22)))))

(defn handle-key-pressed
  [scene event]
  (when (= :escape (:key event))
    (q/exit))
  scene)

(defn show!
  ([scene]
   (show! scene {}))
  ([scene {:keys [title]
           :or {title "architecture-viewer"}}]
   (let [height (if (seq (:layer-rects scene))
                  (->> (:layer-rects scene)
                       (map (fn [{:keys [y height]}] (+ y height)))
                       (apply max)
                       (+ 40))
                  400)
         width (if (seq (:layer-rects scene))
                 (:width (first (:layer-rects scene)))
                 1200)]
     (q/sketch
       :title title
       :size [width height]
       :setup (fn [] scene)
       :draw draw-scene
       :key-pressed handle-key-pressed
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
