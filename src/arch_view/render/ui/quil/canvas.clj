(ns arch-view.render.ui.quil.canvas
  (:require [quil.core :as q]))

(defn draw-scene-content
  [scene viewport-width dependency-indicators {:keys [rendered-label rendered-label-lines draw-dependency-indicators]}]
  (q/background 250 250 250)
  (doseq [{:keys [x y width height abstract? leaf?]} (:layer-rects scene)]
    (if abstract?
      (q/fill 226 242 226)
      (q/fill 225 233 242))
    (if leaf?
      (q/stroke 0 0 0)
      (q/stroke 120 140 160))
    (q/stroke-weight (if leaf? 3.0 1.0))
    (q/rect x y width height)
    (when-not leaf?
      (let [button-width 10.0
            button-height (/ (double height) 5.0)
            button-gap (/ (double height) 5.0)
            first-y (+ (double y) (/ (double height) 5.0))
            second-y (+ first-y button-height button-gap)
            button-x (- (double x) button-width)]
        (q/rect button-x first-y button-width button-height)
        (q/rect button-x second-y button-width button-height))))
  (q/stroke-weight 1.0)
  (doseq [{:keys [x y kind cycle?] :as module-position} (:module-positions scene)]
    (let [lines (vec (or (when rendered-label-lines
                           (rendered-label-lines module-position))
                         [(rendered-label module-position)]))
          line-height 14.0
          mid (/ (dec (max 1 (count lines))) 2.0)]
    (if cycle?
      (q/fill 180 0 0)
      (if (= :abstract kind)
      (q/fill 0 128 0)
        (q/fill 15 20 30)))
    (q/no-stroke)
    (q/text-align :center :center)
      (doseq [[idx line] (map-indexed vector lines)]
        (q/text line x (+ y (* (- idx mid) line-height))))))
  (draw-dependency-indicators dependency-indicators))

(defn draw-toolbar
  [{:keys [namespace-path nav-stack]}
   {:keys [back-button-rect back-button-label toolbar-height]}]
  (let [back-rect (back-button-rect)
        can-go-back? (seq namespace-path)
        back-label (back-button-label {:namespace-path namespace-path
                                       :nav-stack nav-stack})]
    (q/no-stroke)
    (q/fill 238 242 246)
    (q/rect 0 0 3000 toolbar-height)
    (q/fill (if can-go-back? 225 205))
    (q/rect (:x back-rect) (:y back-rect) (:width back-rect) (:height back-rect))
    (if can-go-back?
      (q/fill 0 0 0)
      (q/fill 120 120 120))
    (q/text-align :center :center)
    (q/text back-label (+ (:x back-rect) (/ (:width back-rect) 2.0)) (+ (:y back-rect) (/ (:height back-rect) 2.0)))))

(defn- canvas-dimension
  [getter fallback]
  (try
    (let [v (double (getter))]
      (if (pos? v) v fallback))
    (catch Throwable _
      fallback)))

(defn- clamp-tooltip-origin
  [x y width height]
  (let [margin 6.0
        canvas-w (canvas-dimension q/width 1400.0)
        canvas-h (canvas-dimension q/height 900.0)
        max-x (max margin (- canvas-w width margin))
        max-y (max margin (- canvas-h height margin))]
    [(-> x (max margin) (min max-x))
     (-> y (max margin) (min max-y))]))

(defn draw-tooltip
  [full-name mx my]
  (let [width (+ 12.0 (* 7.0 (count full-name)))
        height 20.0
        [x y] (clamp-tooltip-origin (+ mx 12.0) (+ my 12.0) width height)]
    (q/fill 255 255 225)
    (q/stroke 80 80 80)
    (q/rect x y width height))
  (q/fill 0 0 0)
  (q/no-stroke)
  (q/text-align :left :center)
  (let [[x y] (clamp-tooltip-origin (+ mx 12.0) (+ my 12.0)
                                    (+ 12.0 (* 7.0 (count full-name)))
                                    20.0)]
    (q/text full-name (+ x 6.0) (+ y 10.0))))

(defn draw-tooltip-lines
  [lines mx my]
  (let [line-height 16.0
        line-entries (->> (or lines [])
                          (map (fn [line]
                                 (if (map? line)
                                   line
                                   {:text (str line) :cycle? false})))
                          vec)
        longest (if (seq line-entries) (apply max (map (comp count :text) line-entries)) 0)
        width (+ 18.0 (* 7.0 longest))
        height (+ 10.0 (* line-height (max 1 (count line-entries))))
        [base-x base-y] (clamp-tooltip-origin (+ mx 12.0) (+ my 12.0) width height)]
    (q/fill 255 255 225)
    (q/stroke 80 80 80)
    (q/rect base-x base-y width height)
    (q/no-stroke)
    (q/text-align :left :top)
    (doseq [[idx {:keys [text cycle?]}] (map-indexed vector line-entries)]
      (if cycle?
        (q/fill 180 0 0)
        (q/fill 0 0 0))
      (q/text text (+ base-x 8.0) (+ base-y 6.0 (* idx line-height))))))

(defn draw-scrollbar
  [content-height viewport-height scroll-y viewport-width {:keys [scrollbar-rect]}]
  (when-let [{:keys [x y width height]} (scrollbar-rect content-height viewport-height scroll-y viewport-width)]
    (q/no-stroke)
    (q/fill 220 220 220)
    (q/rect (- x 1.0) 10.0 (+ width 2.0) (- viewport-height 20.0))
    (q/fill 120 120 120)
    (q/rect x y width height)))

(defn- world-bounds
  [sx sy viewport-width viewport-height z]
  (let [world-left (/ sx z)
        world-right (/ (+ sx (double viewport-width)) z)
        world-top (/ sy z)
        world-bottom (/ (+ sy (double viewport-height)) z)]
    {:min-x (+ world-left (/ 14.0 z))
     :max-x (- world-right (/ 20.0 z))
     :min-y (+ world-top (/ 14.0 z))
     :max-y (- world-bottom (/ 14.0 z))}))

(defn- draw-zoomed-scene
  [scene viewport-width dependency-indicators z sx sy {:keys [draw-scene-content]}]
  (q/background 250 250 250)
  (q/push-matrix)
  (q/scale z)
  (q/translate (- (/ sx z)) (- (/ sy z)))
  (draw-scene-content scene viewport-width dependency-indicators)
  (q/pop-matrix))

(defn- draw-hover-tooltip
  [hovered hovered-layer hovered-dependency mx my {:keys [draw-tooltip draw-tooltip-lines]}]
  (cond
    hovered (draw-tooltip (:full-name hovered) mx my)
    (:full-name hovered-layer) (draw-tooltip (:full-name hovered-layer) mx my)
    hovered-dependency (draw-tooltip-lines (:tooltip-lines hovered-dependency) mx my)))

(defn draw-scene
  [{:keys [scene declutter-mode scroll-x scroll-y viewport-height viewport-width zoom] :as state}
   {:keys [scaled-content-height point-in-toolbar? module-point-map dependency-indicators
           hovered-dependency hovered-module-position hovered-layer-label
           draw-scene-content draw-toolbar draw-tooltip draw-tooltip-lines draw-scrollbar]}]
  (let [z (double (or zoom 1.0))
        content-height (scaled-content-height scene z)
        mx (double (q/mouse-x))
        my (double (q/mouse-y))
        interactive-canvas? (not (point-in-toolbar? mx my))
        sx (double (or scroll-x 0.0))
        sy (double (or scroll-y 0.0))
        world-mx (+ (/ mx z) (/ sx z))
        world-my (+ (/ my z) (/ sy z))
        deps (dependency-indicators scene declutter-mode)
        hovered-dep (when interactive-canvas?
                      (hovered-dependency deps world-mx world-my))
        hovered (when interactive-canvas?
                  (hovered-module-position (:module-positions scene) world-mx world-my))
        hovered-layer (when interactive-canvas?
                        (hovered-layer-label (:layer-rects scene) world-mx world-my))]
    (q/cursor (if (:drillable? hovered)
                :cross
                :arrow))
    (draw-zoomed-scene scene viewport-width deps z sx sy
                       {:draw-scene-content draw-scene-content})
    (draw-toolbar state)
    (draw-hover-tooltip hovered hovered-layer hovered-dep mx my
                        {:draw-tooltip draw-tooltip
                         :draw-tooltip-lines draw-tooltip-lines})
    (draw-scrollbar content-height viewport-height scroll-y viewport-width)))
