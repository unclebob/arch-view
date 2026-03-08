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
    (cond
      (keyword? b) b
      (= b 1) :left
      (= b 2) :center
      (= b 3) :right
      :else nil)))

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
  [state mx my {:keys [point-in-rect? back-button-rect toolbar-height]}]
  (cond
    (and (seq (:namespace-path state))
         (point-in-rect? (back-button-rect) mx my)) :back
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
        state))))

(defn apply-drilldown-click
  [{:keys [scene scroll-x scroll-y namespace-path zoom] :as state} event
   {:keys [hovered-module-position view-architecture push-nav-state drilldown-scene open-source-file-window!]}]
  (let [[mx my] (mouse-pos event)
        z (double (or zoom 1.0))
        world-mx (+ (/ mx z) (/ (double (or scroll-x 0.0)) z))
        world-my (+ (/ my z) (/ (double (or scroll-y 0.0)) z))
        hovered (hovered-module-position (:module-positions scene) world-mx world-my)]
    (if hovered
      (let [candidate (conj (or namespace-path []) (:module hovered))
            child-view (view-architecture (:architecture state) candidate)]
        (if (seq (get-in child-view [:graph :nodes]))
          (-> state
              push-nav-state
              (drilldown-scene candidate 0.0 0.0))
          (do
            (when-let [source-file (:source-file hovered)]
              (open-source-file-window! source-file))
            state)))
      state)))

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

(defn handle-key-pressed
  [state event deps]
  (let [k (:key event)
        mouse-x (event-mouse-coord event :x :mouse-x)
        mouse-y (event-mouse-coord event :y :mouse-y)]
    (cond
      (= :escape k) (do (q/exit) state)
      (plus-key? k) (zoom-in-at-screen-pos state mouse-x mouse-y deps)
      (minus-key? k) (or (zoom-out-at-screen-pos state mouse-x mouse-y deps) state)
      :else state)))

(defn handle-mouse-wheel
  [{:keys [scene scroll-y viewport-height viewport-width zoom] :as state} event
   {:keys [scaled-content-height clamp-scroll]}]
  (let [content-height (scaled-content-height scene (or zoom 1.0))
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

(defn handle-mouse-pressed
  [{:keys [scene scroll-y viewport-height viewport-width zoom] :as state} event
   {:keys [scaled-content-height scrollbar-rect point-in-rect?]}]
  (let [content-height (scaled-content-height scene (or zoom 1.0))
        thumb (scrollbar-rect content-height viewport-height scroll-y viewport-width)
        [mx my] (mouse-pos event)]
    (if (and thumb (point-in-rect? thumb mx my))
      (assoc state
             :dragging-scrollbar? true
             :drag-offset (- my (:y thumb)))
      state)))

(defn handle-mouse-dragged
  [{:keys [scene viewport-height dragging-scrollbar? drag-offset zoom] :as state} event
   {:keys [scaled-content-height thumb-y->scroll]}]
  (if-not dragging-scrollbar?
    state
    (let [content-height (scaled-content-height scene (or zoom 1.0))
          [_ mouse-y] (mouse-pos event)
          thumb-y (- mouse-y (double (or drag-offset 0)))
          next-scroll (thumb-y->scroll thumb-y content-height viewport-height)]
      (assoc state :scroll-y next-scroll))))

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
