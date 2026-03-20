;; mutation-tested: 2026-03-08
(ns arch-view.render.ui.quil.events
  (:require [arch-view.render.ui.util.events-logic :as logic]
            [quil.core :as q]))

(defn plus-key?
  [k]
  (logic/plus-key? k))

(defn minus-key?
  [k]
  (logic/minus-key? k))

(defn mouse-pos
  [event]
  [(double (or (:x event) (q/mouse-x)))
   (double (or (:y event) (q/mouse-y)))])

(defn- canvas-dimension
  [getter fallback]
  (try
    (let [value (double (getter))]
      (if (pos? value) value (double (or fallback 0.0))))
    (catch Throwable _
      (double (or fallback 0.0)))))

(defn control-down?
  [event]
  (let [mods (:modifiers event)
        ctrl-mask 128
        legacy-ctrl-mask 2
        mod-set (if (or (set? mods) (sequential? mods))
                  (set mods)
                  #{})
        ctrl-held? (try
                     (and (q/key-pressed?)
                          (contains? #{:control :ctrl}
                                     (q/key-as-keyword)))
                     (catch Throwable _
                       false))
        direct? (some true? [(true? (:control event))
                             (true? (:ctrl event))
                             (true? (:control-key? event))
                             ctrl-held?
                             (contains? mod-set :control)
                             (contains? mod-set :ctrl)
                             (contains? mod-set :control-down)])
        mask? (and (number? mods)
                   (not (zero? (bit-and (int mods) (+ ctrl-mask legacy-ctrl-mask)))))]
    (or direct? mask?)))

(defn button-kind
  [event]
  (let [b (or (:button event)
              (:mouse-button event)
              (:which event)
              (try
                (q/mouse-button)
                (catch Throwable _
                  nil)))]
    (or (when (keyword? b) b)
        ({1 :left
          2 :center
          3 :right} b))))

(defn world-y-at-screen
  [screen-y scroll-y zoom]
  (logic/world-y-at-screen screen-y scroll-y zoom))

(defn world-x-at-screen
  [screen-x scroll-x zoom]
  (logic/world-x-at-screen screen-x scroll-x zoom))

(defn scroll-for-world-y
  [world-y screen-y zoom]
  (logic/scroll-for-world-y world-y screen-y zoom))

(defn scroll-for-world-x
  [world-x screen-x zoom]
  (logic/scroll-for-world-x world-x screen-x zoom))

(defn- event-world-coords
  [event scroll-x scroll-y zoom]
  (let [[mx my] (mouse-pos event)
        z (double (or zoom 1.0))]
    {:mx mx
     :my my
     :world-mx (+ (/ mx z) (/ (double (or scroll-x 0.0)) z))
     :world-my (+ (/ my z) (/ (double (or scroll-y 0.0)) z))}))

(defn zoom-in-at-screen-pos
  [{:keys [zoom zoom-stack scene viewport-height viewport-width] :as state} screen-x screen-y
   {:keys [scaled-content-width scaled-content-height clamp-scroll clamp-scroll-x]}]
  (let [old-z (double (or zoom 1.0))
        center-world-x (world-x-at-screen screen-x (:scroll-x state) old-z)
        center-world-y (world-y-at-screen screen-y (:scroll-y state) old-z)
        new-z (* old-z 1.1)
        content-width (scaled-content-width scene new-z)
        content-height (scaled-content-height scene new-z)
        next-scroll-x (clamp-scroll-x (scroll-for-world-x center-world-x screen-x new-z)
                                      content-width
                                      viewport-width)
        next-scroll-y (clamp-scroll (scroll-for-world-y center-world-y screen-y new-z)
                                    content-height
                                    viewport-height)]
    (assoc state
           :zoom new-z
           :scroll-x next-scroll-x
           :scroll-y next-scroll-y
           :zoom-stack (conj (vec (or zoom-stack []))
                             {:zoom old-z
                              :center-world-x center-world-x
                              :center-world-y center-world-y
                              :screen-x (double screen-x)
                              :screen-y (double screen-y)
                              :scroll-x (double (or (:scroll-x state) 0.0))
                              :scroll-y (double (or (:scroll-y state) 0.0))}))))

(defn- zoom-out-at-screen-pos*
  [{:keys [zoom scene viewport-height viewport-width] :as state} screen-x screen-y
   {:keys [scaled-content-width scaled-content-height clamp-scroll clamp-scroll-x]}
   stack]
  (let [old-z (double (or zoom 1.0))
        {prev-z :zoom
         prev-scroll-x :scroll-x
         prev-scroll-y :scroll-y
         center-world-x :center-world-x
         center-world-y :center-world-y
         screen-x0 :screen-x
         screen-y0 :screen-y} (peek stack)
        target-screen-x (double (or screen-x0 screen-x))
        target-screen-y (double (or screen-y0 screen-y))
        content-width (scaled-content-width scene prev-z)
        content-height (scaled-content-height scene prev-z)
        fallback-world-x (world-x-at-screen screen-x (:scroll-x state) old-z)
        fallback-world-y (world-y-at-screen screen-y (:scroll-y state) old-z)
        centered-scroll-x (scroll-for-world-x (or center-world-x fallback-world-x)
                                              target-screen-x
                                              prev-z)
        centered-scroll-y (scroll-for-world-y (or center-world-y fallback-world-y)
                                              target-screen-y
                                              prev-z)
        next-scroll-x (clamp-scroll-x (or centered-scroll-x prev-scroll-x)
                                      content-width
                                      viewport-width)
        next-scroll-y (clamp-scroll (or centered-scroll-y prev-scroll-y)
                                    content-height
                                    viewport-height)]
    (assoc state
           :zoom prev-z
           :scroll-x next-scroll-x
           :scroll-y next-scroll-y
           :zoom-stack (pop stack))))

(defn- child-view-exists?
  [state candidate view-architecture]
  (seq (get-in (view-architecture (:architecture state) candidate)
               [:graph :nodes])))

(defn- open-source-if-present!
  [hovered open-source-file-window!]
  (when-let [source-file (:source-file hovered)]
    (open-source-file-window! source-file)))

(defn zoom-out-at-screen-pos
  [{:keys [zoom zoom-stack scene viewport-height viewport-width] :as state} screen-x screen-y
   {:keys [scaled-content-width scaled-content-height clamp-scroll clamp-scroll-x]}]
  (let [stack (vec (or zoom-stack []))]
    (when (seq stack)
      (zoom-out-at-screen-pos* state screen-x screen-y
                               {:scaled-content-width scaled-content-width
                                :scaled-content-height scaled-content-height
                                :clamp-scroll clamp-scroll
                                :clamp-scroll-x clamp-scroll-x}
                               stack))))

(defn point-in-toolbar?
  [mx my toolbar-height]
  (logic/point-in-toolbar? mx my toolbar-height))

(defn toolbar-click-target
  [state mx my {:keys [point-in-rect? back-button-rect reanalyze-button-rect toolbar-height]}]
  (cond
    (and (seq (:namespace-path state))
         (point-in-rect? (back-button-rect) mx my)) :back
    (and reanalyze-button-rect
         (point-in-rect? (reanalyze-button-rect) mx my)) :reanalyze
    :else nil))

(defn navigate-up
  [{:keys [nav-stack] :as state} {:keys [drilldown-scene]}]
  (let [stack (vec (or nav-stack []))]
    (if (seq stack)
      (let [{:keys [path scroll-x scroll-y]} (peek stack)]
        (-> state
            (assoc :nav-stack (pop stack))
            (drilldown-scene (vec (or path [])) scroll-x scroll-y)))
      state)))

(defn apply-toolbar-click
  [state event {:keys [toolbar-height] :as deps}]
  (let [[mx my] (mouse-pos event)]
    (if-not (point-in-toolbar? mx my toolbar-height)
      nil
      (case (toolbar-click-target state mx my deps)
        :back (navigate-up state deps)
        :reanalyze ((:reanalyze-state deps) state)
        state))))

(defn apply-drilldown-click
  [{:keys [scene scroll-x scroll-y namespace-path zoom] :as state} event
   {:keys [hovered-module-position view-architecture push-nav-state drilldown-scene open-source-file-window!]}]
  (let [{:keys [world-mx world-my]} (event-world-coords event scroll-x scroll-y zoom)
        hovered (hovered-module-position (:module-positions scene) world-mx world-my)]
    (if-not hovered
      state
      (let [candidate (conj (or namespace-path []) (:module hovered))]
        (if (child-view-exists? state candidate view-architecture)
          (-> state
              push-nav-state
              (drilldown-scene candidate 0.0 0.0))
          (do
            (open-source-if-present! hovered open-source-file-window!)
            state))))))

(defn- zoom-click-in
  [state my {:keys [scaled-content-height clamp-scroll]}]
  (let [old-z (double (or (:zoom state) 1.0))
        center-world-y (world-y-at-screen my (:scroll-y state) old-z)
        new-z (* old-z 1.1)
        content-height (scaled-content-height (:scene state) new-z)
        next-scroll (clamp-scroll (scroll-for-world-y center-world-y my new-z)
                                  content-height
                                  (:viewport-height state))]
    (assoc state
           :zoom new-z
           :scroll-y next-scroll
           :zoom-stack (conj (vec (or (:zoom-stack state) []))
                             {:zoom old-z
                              :center-world-y center-world-y
                              :screen-y (double my)
                              :scroll-y (double (or (:scroll-y state) 0.0))}))))

(defn- zoom-click-out
  [state my {:keys [scaled-content-height clamp-scroll]}]
  (let [stack (vec (or (:zoom-stack state) []))]
    (when (seq stack)
      (let [{prev-z :zoom prev-scroll :scroll-y center-world-y :center-world-y screen-y :screen-y} (peek stack)
            target-screen-y (double (or screen-y my))
            old-z (double (or (:zoom state) 1.0))
            content-height (scaled-content-height (:scene state) prev-z)
            fallback-world-y (world-y-at-screen my (:scroll-y state) old-z)
            centered-scroll (scroll-for-world-y (or center-world-y fallback-world-y)
                                                target-screen-y
                                                prev-z)
            next-scroll (clamp-scroll (or centered-scroll prev-scroll)
                                      content-height
                                      (:viewport-height state))]
        (assoc state
               :zoom prev-z
               :scroll-y next-scroll
               :zoom-stack (pop stack))))))

(defn apply-zoom-click
  [{:keys [zoom zoom-stack scene viewport-height] :as state} event
   {:keys [scaled-content-height clamp-scroll]}]
  (if-not (control-down? event)
    nil
    (let [[_ my] (mouse-pos event)
          b (button-kind event)]
      (case b
        :right (zoom-click-in state my {:scaled-content-height scaled-content-height
                                        :clamp-scroll clamp-scroll})
        :left (zoom-click-out state my {:scaled-content-height scaled-content-height
                                        :clamp-scroll clamp-scroll})
        nil))))

(defn- event-mouse-coord
  [event primary fallback]
  (double (or (get event primary)
              (get event fallback)
              (try
                (if (= primary :x) (q/mouse-x) (q/mouse-y))
                (catch Throwable _
                  0.0)))))

(declare sync-viewport-size)

(def ^:private dependency-tooltip-line-height 16.0)
(def ^:private dependency-tooltip-padding 10.0)

(defn dependency-tooltip-scroll-range
  [tooltip-lines viewport-height]
  (let [line-count (max 1 (count (or tooltip-lines [])))
        content-height (+ dependency-tooltip-padding
                          (* dependency-tooltip-line-height line-count))
        visible-height (min content-height
                            (max 60.0 (- (double viewport-height) 40.0)))]
    (max 0.0 (- content-height visible-height))))

(defn dependency-tooltip-key
  [dependency]
  (when dependency
    [(:module dependency)
     (:direction dependency)
     (mapv :text (:tooltip-lines dependency))]))

(defn hovered-dependency-at-event
  [{:keys [scene declutter-mode scroll-x scroll-y zoom] :as state} event
   {:keys [point-in-toolbar? dependency-indicators hovered-dependency]}]
  (let [[mx my] (mouse-pos event)]
    (when-not (point-in-toolbar? mx my)
      (let [z (double (or zoom 1.0))
            world-mx (+ (/ mx z) (/ (double (or scroll-x 0.0)) z))
            world-my (+ (/ my z) (/ (double (or scroll-y 0.0)) z))
            indicators (dependency-indicators scene declutter-mode)]
        (hovered-dependency indicators world-mx world-my)))))

(defn handle-key-pressed
  [state event deps]
  (let [state (sync-viewport-size state deps)
        k (:key event)
        mouse-x (event-mouse-coord event :x :mouse-x)
        mouse-y (event-mouse-coord event :y :mouse-y)]
    (cond
      (= :escape k) (do (q/exit) state)
      (plus-key? k) (zoom-in-at-screen-pos state mouse-x mouse-y deps)
      (minus-key? k) (or (zoom-out-at-screen-pos state mouse-x mouse-y deps) state)
      :else state)))

(defn handle-mouse-wheel
  [{:keys [scene scroll-y viewport-height viewport-width zoom] :as state} event
   {:keys [scaled-content-width scaled-content-height clamp-scroll clamp-scroll-x
           point-in-toolbar? dependency-indicators hovered-dependency]}]
  (let [state (sync-viewport-size state {:scaled-content-width scaled-content-width
                                         :scaled-content-height scaled-content-height
                                         :clamp-scroll clamp-scroll
                                         :clamp-scroll-x clamp-scroll-x})
        dependency (when (and point-in-toolbar? dependency-indicators hovered-dependency)
                     (hovered-dependency-at-event state event
                                                 {:point-in-toolbar? point-in-toolbar?
                                                  :dependency-indicators dependency-indicators
                                                  :hovered-dependency hovered-dependency}))
        tooltip-range (dependency-tooltip-scroll-range (:tooltip-lines dependency) (:viewport-height state))
        units (cond
                (number? event) (double event)
                (map? event) (double (or (:count event)
                                         (:amount event)
                                         (:rotation event)
                                         (:wheel-rotation event)
                                         0))
                :else 0.0)
        delta (* 30.0 units)]
    (if (and dependency (pos? tooltip-range))
      (let [next-tooltip-scroll (-> (+ (double (or (:dependency-tooltip-scroll state) 0.0)) delta)
                                    (max 0.0)
                                    (min tooltip-range))]
        (assoc state
               :dependency-tooltip-scroll next-tooltip-scroll
               :dependency-tooltip-key (dependency-tooltip-key dependency)))
      (let [content-height (scaled-content-height (:scene state) (or (:zoom state) 1.0))
            next-scroll (clamp-scroll (+ (:scroll-y state) delta) content-height (:viewport-height state))]
        (assoc state
               :scroll-y next-scroll
               :viewport-height (:viewport-height state)
               :viewport-width (:viewport-width state))))))

(defn handle-mouse-pressed
  [{:keys [scene scroll-y viewport-height viewport-width zoom] :as state} event
   {:keys [scaled-content-width scaled-content-height clamp-scroll clamp-scroll-x
           scrollbar-rect horizontal-scrollbar-rect point-in-rect?]}]
  (let [state (sync-viewport-size state {:scaled-content-width scaled-content-width
                                         :scaled-content-height scaled-content-height
                                         :clamp-scroll clamp-scroll
                                         :clamp-scroll-x clamp-scroll-x})
        content-height (scaled-content-height (:scene state) (or (:zoom state) 1.0))
        content-width (scaled-content-width (:scene state) (or (:zoom state) 1.0))
        horizontal-visible? (> content-width (:viewport-width state))
        vertical-visible? (> content-height (:viewport-height state))
        thumb (scrollbar-rect content-height (:viewport-height state) (:scroll-y state) (:viewport-width state)
                              horizontal-visible?)
        h-thumb (horizontal-scrollbar-rect content-width (:viewport-width state) (:scroll-x state) (:viewport-height state)
                                           vertical-visible?)
        [mx my] (mouse-pos event)]
    (cond
      (and thumb (point-in-rect? thumb mx my))
      (assoc state
             :dragging-scrollbar? :vertical
             :drag-offset (- my (:y thumb)))

      (and h-thumb (point-in-rect? h-thumb mx my))
      (assoc state
             :dragging-scrollbar? :horizontal
             :drag-offset (- mx (:x h-thumb)))

      :else state)))

(defn handle-mouse-dragged
  [{:keys [scene viewport-height dragging-scrollbar? drag-offset zoom] :as state} event
   {:keys [scaled-content-width scaled-content-height clamp-scroll clamp-scroll-x
           thumb-y->scroll thumb-x->scroll]}]
  (let [state (sync-viewport-size state {:scaled-content-width scaled-content-width
                                         :scaled-content-height scaled-content-height
                                         :clamp-scroll clamp-scroll
                                         :clamp-scroll-x clamp-scroll-x})]
    (case dragging-scrollbar?
      :vertical
      (let [content-height (scaled-content-height (:scene state) (or (:zoom state) 1.0))
            [_ mouse-y] (mouse-pos event)
            thumb-y (- mouse-y (double (or drag-offset 0)))
            next-scroll (thumb-y->scroll thumb-y content-height (:viewport-height state))]
        (assoc state :scroll-y next-scroll))

      :horizontal
      (let [content-width (scaled-content-width (:scene state) (or (:zoom state) 1.0))
            [mouse-x _] (mouse-pos event)
            thumb-x (- mouse-x (double (or drag-offset 0)))
            next-scroll (thumb-x->scroll thumb-x content-width (:viewport-width state))]
        (assoc state :scroll-x next-scroll))

      state)))

(defn sync-viewport-size
  [{:keys [scene zoom viewport-width viewport-height scroll-x scroll-y] :as state}
   {:keys [scaled-content-width scaled-content-height clamp-scroll clamp-scroll-x]}]
  (let [next-viewport-width (canvas-dimension q/width viewport-width)
        next-viewport-height (canvas-dimension q/height viewport-height)
        content-width (scaled-content-width scene (or zoom 1.0))
        content-height (scaled-content-height scene (or zoom 1.0))
        next-scroll-x (clamp-scroll-x (double (or scroll-x 0.0)) content-width next-viewport-width)
        next-scroll-y (clamp-scroll (double (or scroll-y 0.0)) content-height next-viewport-height)]
    (assoc state
           :viewport-width next-viewport-width
           :viewport-height next-viewport-height
           :scroll-x next-scroll-x
           :scroll-y next-scroll-y)))

(defn handle-mouse-released
  [{:keys [dragging-scrollbar?] :as state} event deps]
  (if dragging-scrollbar?
    (assoc state :dragging-scrollbar? false :drag-offset nil :suppress-next-click? false)
    (let [base-state (assoc state :dragging-scrollbar? false :drag-offset nil)]
      (or (apply-toolbar-click base-state event deps)
          (apply-drilldown-click base-state event deps)))))

(defn handle-mouse-clicked
  [state event deps]
  (if (:dragging-scrollbar? state)
    state
    (or (apply-toolbar-click state event deps)
        (apply-drilldown-click state event deps))))
