;; mutation-tested: 2026-03-08
(ns arch-view.render.ui.util.viewport)

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
  ([content-height viewport-height scroll-y viewport-width]
   (scrollbar-rect content-height viewport-height scroll-y viewport-width false))
  ([content-height viewport-height scroll-y viewport-width horizontal-scrollbar?]
   (when (> content-height viewport-height)
    (let [track-height (- (double viewport-height) 24.0 (if horizontal-scrollbar? 14.0 0.0))
          ratio (/ (double viewport-height) (double content-height))
          thumb-height (max 30.0 (* track-height ratio))
          max-scroll (scroll-range content-height viewport-height)
          thumb-y (if (zero? max-scroll)
                    12.0
                    (+ 12.0 (* (- track-height thumb-height) (/ scroll-y max-scroll))))]
      {:x (- (double viewport-width) 12.0)
       :y thumb-y
       :width 8.0
       :height thumb-height}))))

(defn horizontal-scrollbar-rect
  ([content-width viewport-width scroll-x viewport-height]
   (horizontal-scrollbar-rect content-width viewport-width scroll-x viewport-height false))
  ([content-width viewport-width scroll-x viewport-height vertical-scrollbar?]
   (when (> content-width viewport-width)
     (let [track-width (- (double viewport-width) 24.0 (if vertical-scrollbar? 14.0 0.0))
           ratio (/ (double viewport-width) (double content-width))
           thumb-width (max 30.0 (* track-width ratio))
           max-scroll (scroll-range content-width viewport-width)
           thumb-x (if (zero? max-scroll)
                     12.0
                     (+ 12.0 (* (- track-width thumb-width) (/ scroll-x max-scroll))))]
       {:x thumb-x
        :y (- (double viewport-height) 12.0)
        :width thumb-width
        :height 8.0}))))

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

(defn thumb-x->scroll
  [thumb-x content-width viewport-width]
  (if (<= content-width viewport-width)
    0.0
    (let [track-width (- (double viewport-width) 24.0)
          ratio (/ (double viewport-width) (double content-width))
          thumb-width (max 30.0 (* track-width ratio))
          max-scroll (scroll-range content-width viewport-width)
          max-thumb-travel (max 1.0 (- track-width thumb-width))
          normalized (/ (- thumb-x 12.0) max-thumb-travel)]
      (clamp-scroll-x (* normalized max-scroll) content-width viewport-width))))

(defn point-in-rect?
  [{:keys [x y width height]} px py]
  (and (<= x px (+ x width))
       (<= y py (+ y height))))

(def ^:private cycle-list-line-height 16.0)
(def ^:private cycle-list-top-gap 28.0)
(def ^:private cycle-list-bottom-gap 24.0)

(defn- cycle-list-height
  [scene]
  (let [cycle-count (count (or (:cycle-lines scene) []))]
    (if (zero? cycle-count)
      0.0
      (+ cycle-list-top-gap
         20.0
         (* cycle-list-line-height cycle-count)
         cycle-list-bottom-gap))))

(defn content-height-for-scene
  [scene]
  (->> (:layer-rects scene)
       (map (fn [{:keys [y height]}] (+ y height)))
       (apply max 0)
       (+ 40)
       (+ (cycle-list-height scene))))

(defn scaled-content-height
  [scene zoom]
  (* (double (or zoom 1.0))
     (content-height-for-scene scene)))

(defn content-width-for-scene
  [scene racetrack-margin]
  (let [rect-width (->> (:layer-rects scene)
                        (map (fn [{:keys [x width]}] (+ x width)))
                        (apply max 0))
        cycle-width (if (seq (:cycle-lines scene))
                      (+ racetrack-margin
                         20.0
                         (* 7.0 (apply max (map count (:cycle-lines scene)))))
                      0.0)]
    (max (+ rect-width racetrack-margin 20.0)
         cycle-width)))

(defn scaled-content-width
  [scene zoom racetrack-margin]
  (* (double (or zoom 1.0))
     (content-width-for-scene scene racetrack-margin)))
