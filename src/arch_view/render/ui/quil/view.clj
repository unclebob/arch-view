;; mutation-tested: 2026-03-08
(ns arch-view.render.ui.quil.view
  (:require [arch-view.domain.architecture-projection :as projection]
            [arch-view.render.ui.quil.canvas :as canvas]
            [arch-view.render.ui.quil.events :as events]
            [arch-view.render.ui.util.dependency-indicators :as dependency-indicators]
            [arch-view.render.ui.util.functional :as functional]
            [arch-view.render.ui.util.module-hover :as module-hover]
            [arch-view.render.ui.util.quil-lifecycle :as quil-lifecycle]
            [arch-view.render.ui.util.scene-state :as scene-state]
            [arch-view.render.ui.util.view-bootstrap :as view-bootstrap]
            [arch-view.render.ui.util.viewport :as viewport]
            [arch-view.render.ui.swing.source-window :as source-window]
            [quil.core :as q]
            [quil.middleware :as m]))

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

(defn- rendered-label-lines
  [{:keys [display-label label max-label-chars]}]
  (functional/rendered-label-lines {:display-label display-label
                                    :label label
                                    :max-label-chars max-label-chars}))

(def ^:private toolbar-height 38.0)
(def ^:private back-button-width 360.0)
(def ^:private reanalyze-button-width 180.0)
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
    (str "Back: " (last (vec namespace-path)))))

(defn- back-button-rect
  []
  {:x 10.0 :y 6.0 :width back-button-width :height button-height})

(defn- declutter-button-rect
  []
  (let [{:keys [x width]} (back-button-rect)]
    {:x (+ x width 10.0) :y 6.0 :width declutter-button-width :height button-height}))

(defn- reanalyze-button-label
  [{:keys [reanalyze-status]}]
  (if (= :running reanalyze-status)
    "Analyzing..."
    "Reanalyze"))

(defn- reanalyze-button-rect
  []
  (let [{:keys [x width]} (back-button-rect)]
    {:x (+ x width 10.0) :y 6.0 :width reanalyze-button-width :height button-height}))

(def ^:private racetrack-margin 24.0)
(def ^:private reanalyze-feedback-ms 1000)

(defn current-time-ms
  []
  (System/currentTimeMillis))

(defn build-scene
  ([architecture]
   (functional/build-scene architecture))
  ([architecture {:keys [canvas-width layer-height layer-gap]
                  :or {canvas-width 1200 layer-height 140 layer-gap 24}}]
   (functional/build-scene architecture {:canvas-width canvas-width
                                         :layer-height layer-height
                                         :layer-gap layer-gap})))

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

(defn hovered-module
  [module-positions mx my]
  (module-hover/hovered-module module-positions mx my rendered-label-lines label-width))

(defn hovered-layer-label
  [layer-rects mx my]
  (module-hover/hovered-layer-label layer-rects mx my label-width))

(defn- hovered-module-position
  [module-positions mx my]
  (module-hover/hovered-module-position module-positions mx my rendered-label-lines label-width))

(declare view-architecture
         build-scene-for-path
         drilldown-scene
         reanalyze-state
         start-reanalyze-state
         push-nav-state
         point-in-toolbar?)

(defn attach-drillable-markers
  [scene architecture namespace-path]
  (scene-state/attach-drillable-markers scene architecture namespace-path view-architecture))

(defn scroll-range
  [content-height viewport-height]
  (viewport/scroll-range content-height viewport-height))

(defn clamp-scroll
  [scroll-y content-height viewport-height]
  (viewport/clamp-scroll scroll-y content-height viewport-height))

(defn clamp-scroll-x
  [scroll-x content-width viewport-width]
  (viewport/clamp-scroll-x scroll-x content-width viewport-width))

(defn scrollbar-rect
  ([content-height viewport-height scroll-y viewport-width]
   (viewport/scrollbar-rect content-height viewport-height scroll-y viewport-width))
  ([content-height viewport-height scroll-y viewport-width horizontal-scrollbar?]
   (viewport/scrollbar-rect content-height viewport-height scroll-y viewport-width horizontal-scrollbar?)))

(defn horizontal-scrollbar-rect
  ([content-width viewport-width scroll-x viewport-height]
   (viewport/horizontal-scrollbar-rect content-width viewport-width scroll-x viewport-height))
  ([content-width viewport-width scroll-x viewport-height vertical-scrollbar?]
   (viewport/horizontal-scrollbar-rect content-width viewport-width scroll-x viewport-height vertical-scrollbar?)))

(defn- content-height-for-scene
  [scene]
  (viewport/content-height-for-scene scene))

(defn- scaled-content-height
  [scene zoom]
  (viewport/scaled-content-height scene zoom))

(defn- content-width-for-scene
  [scene]
  (viewport/content-width-for-scene scene racetrack-margin))

(defn- scaled-content-width
  [scene zoom]
  (viewport/scaled-content-width scene zoom racetrack-margin))

(defn thumb-y->scroll
  [thumb-y content-height viewport-height]
  (viewport/thumb-y->scroll thumb-y content-height viewport-height))

(defn thumb-x->scroll
  [thumb-x content-width viewport-width]
  (viewport/thumb-x->scroll thumb-x content-width viewport-width))

(defn- point-in-rect?
  [{:keys [x y width height]} px py]
  (viewport/point-in-rect? {:x x :y y :width width :height height} px py))

(defn- draw-scene-content
  [scene viewport-width dependency-indicators]
  (canvas/draw-scene-content scene viewport-width dependency-indicators
                             {:rendered-label rendered-label
                              :rendered-label-lines rendered-label-lines
                              :draw-dependency-indicators dependency-indicators/draw-dependency-indicators}))

(defn dependency-indicators
  [scene declutter-mode]
  (dependency-indicators/dependency-indicators scene declutter-mode strip-top-namespace))

(defn- draw-toolbar
  [{:keys [namespace-path nav-stack reload-architecture reanalyze-status] :as state}]
  (canvas/draw-toolbar {:namespace-path namespace-path
                        :nav-stack nav-stack
                        :reload-architecture reload-architecture
                        :reanalyze-status reanalyze-status}
                       {:back-button-rect back-button-rect
                        :back-button-label back-button-label
                        :reanalyze-button-rect reanalyze-button-rect
                        :reanalyze-button-label reanalyze-button-label
                        :toolbar-height toolbar-height}))

(defn- draw-tooltip
  [full-name mx my]
  (canvas/draw-tooltip full-name mx my))

(defn- draw-tooltip-lines
  ([lines mx my]
   (canvas/draw-tooltip-lines lines mx my))
  ([lines mx my opts]
   (canvas/draw-tooltip-lines lines mx my opts)))

(defn hovered-dependency
  [indicators mx my]
  (dependency-indicators/hovered-dependency indicators mx my))

(defn dependency-tooltip-key
  [dependency]
  (events/dependency-tooltip-key dependency))

(defn hovered-dependency-for-state
  [{:keys [scene declutter-mode scroll-x scroll-y zoom] :as state}]
  (let [mx (double (q/mouse-x))
        my (double (q/mouse-y))]
    (when-not (point-in-toolbar? mx my)
      (let [z (double (or zoom 1.0))
            world-mx (+ (/ mx z) (/ (double (or scroll-x 0.0)) z))
            world-my (+ (/ my z) (/ (double (or scroll-y 0.0)) z))
            indicators (dependency-indicators scene declutter-mode)]
        (hovered-dependency indicators world-mx world-my)))))

(defn- draw-scrollbars
  [{:keys [scene scroll-x scroll-y viewport-height viewport-width zoom]}]
  (let [content-height (scaled-content-height scene zoom)
        content-width (scaled-content-width scene zoom)
        horizontal-visible? (> content-width viewport-width)
        vertical-visible? (> content-height viewport-height)]
    (canvas/draw-scrollbar content-height viewport-height scroll-y viewport-width
                           {:scrollbar-rect scrollbar-rect
                            :horizontal-scrollbar-visible? horizontal-visible?})
    (canvas/draw-horizontal-scrollbar content-width viewport-width scroll-x viewport-height
                                      {:horizontal-scrollbar-rect horizontal-scrollbar-rect
                                       :vertical-scrollbar-visible? vertical-visible?})))

(defn- draw-scene
  [{:keys [scene declutter-mode scroll-x scroll-y viewport-height viewport-width zoom] :as state}]
  (canvas/draw-scene state
                     {:scaled-content-height scaled-content-height
                      :point-in-toolbar? point-in-toolbar?
                      :dependency-indicators dependency-indicators
                      :hovered-dependency hovered-dependency
                      :hovered-module-position hovered-module-position
                      :hovered-layer-label hovered-layer-label
                      :draw-scene-content draw-scene-content
                      :draw-toolbar draw-toolbar
                      :draw-tooltip draw-tooltip
                      :draw-tooltip-lines draw-tooltip-lines
                      :draw-scrollbars draw-scrollbars}))

(defn update-state
  [state]
  (let [previous-width (:viewport-width state)
        now (current-time-ms)
        state (events/sync-viewport-size state
                                         {:scaled-content-width scaled-content-width
                                          :scaled-content-height scaled-content-height
                                          :clamp-scroll clamp-scroll
                                          :clamp-scroll-x clamp-scroll-x})
        state (if (and (:reanalyze-requested? state)
                       (number? (:reanalyze-started-at state))
                       (>= (- now (:reanalyze-started-at state)) reanalyze-feedback-ms))
                (reanalyze-state state)
                state)
        state (if (and (:architecture state)
                       (not= (double (or previous-width 0.0))
                             (double (or (:viewport-width state) 0.0))))
                (assoc state :scene (build-scene-for-path (:architecture state)
                                                          (vec (or (:namespace-path state) []))
                                                          (:viewport-width state)))
                state)
        hovered (hovered-dependency-for-state state)
        key (dependency-tooltip-key hovered)]
    (cond
      (nil? hovered) (assoc state :dependency-tooltip-key nil :dependency-tooltip-scroll 0.0)
      (not= key (:dependency-tooltip-key state)) (assoc state :dependency-tooltip-key key :dependency-tooltip-scroll 0.0)
      :else state)))

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
                             {:scaled-content-width scaled-content-width
                              :scaled-content-height scaled-content-height
                              :clamp-scroll clamp-scroll
                              :clamp-scroll-x clamp-scroll-x
                              :point-in-toolbar? point-in-toolbar?
                              :dependency-indicators dependency-indicators
                              :hovered-dependency hovered-dependency}))

(defn handle-mouse-pressed
  [{:keys [scene scroll-y viewport-height viewport-width zoom] :as state} event]
  (events/handle-mouse-pressed state event
                               {:scaled-content-width scaled-content-width
                                :scaled-content-height scaled-content-height
                                :clamp-scroll clamp-scroll
                                :clamp-scroll-x clamp-scroll-x
                                :scrollbar-rect scrollbar-rect
                                :horizontal-scrollbar-rect horizontal-scrollbar-rect
                                :point-in-rect? point-in-rect?}))

(defn handle-mouse-dragged
  [{:keys [scene viewport-height dragging-scrollbar? drag-offset zoom] :as state} event]
  (events/handle-mouse-dragged state event
                               {:scaled-content-width scaled-content-width
                                :scaled-content-height scaled-content-height
                                :clamp-scroll clamp-scroll
                                :clamp-scroll-x clamp-scroll-x
                                :thumb-y->scroll thumb-y->scroll
                                :thumb-x->scroll thumb-x->scroll}))

(defn- point-in-toolbar?
  [mx my]
  (events/point-in-toolbar? mx my toolbar-height))

(defn- apply-toolbar-click
  [state event]
  (events/apply-toolbar-click state event
                              {:point-in-rect? point-in-rect?
                               :back-button-rect back-button-rect
                               :reanalyze-button-rect (when (:reload-architecture state) reanalyze-button-rect)
                               :reanalyze-state start-reanalyze-state
                               :drilldown-scene drilldown-scene
                               :toolbar-height toolbar-height}))

(defn handle-mouse-released
  [{:keys [dragging-scrollbar?] :as state} event]
  (events/handle-mouse-released state event
                                {:point-in-rect? point-in-rect?
                                 :back-button-rect back-button-rect
                                 :reanalyze-button-rect (when (:reload-architecture state) reanalyze-button-rect)
                                 :reanalyze-state start-reanalyze-state
                                 :drilldown-scene drilldown-scene
                                 :toolbar-height toolbar-height
                                 :hovered-module-position hovered-module-position
                                 :view-architecture view-architecture
                                 :push-nav-state push-nav-state
                                 :open-source-file-window! source-window/open-source-file-window!}))

(defn handle-mouse-clicked
  [state event]
  (events/handle-mouse-clicked state event
                               {:point-in-rect? point-in-rect?
                                :back-button-rect back-button-rect
                                :reanalyze-button-rect (when (:reload-architecture state) reanalyze-button-rect)
                                :reanalyze-state start-reanalyze-state
                                :drilldown-scene drilldown-scene
                                :toolbar-height toolbar-height
                                :hovered-module-position hovered-module-position
                                :view-architecture view-architecture
                                :push-nav-state push-nav-state
                                :open-source-file-window! source-window/open-source-file-window!}))

(defn view-architecture
  [architecture namespace-path]
  (projection/view-architecture architecture namespace-path))

(defn- scene-canvas-width
  [viewport-width]
  (max 1200 (double (or viewport-width 1200.0))))

(defn- build-scene-for-path
  [architecture path viewport-width]
  (let [view (view-architecture architecture path)]
    (-> (build-scene view {:canvas-width (scene-canvas-width viewport-width)})
        (attach-drillable-markers architecture path))))

(defn- drilldown-scene
  [state path scroll-x scroll-y]
  (assoc state
         :namespace-path path
         :scroll-x (double (or scroll-x 0.0))
         :scroll-y (double (or scroll-y 0.0))
         :scene (build-scene-for-path (:architecture state) path (:viewport-width state))
         :routed-edges nil))

(defn initial-scene-for-show
  [scene architecture]
  (if architecture
    (build-scene-for-path architecture [] nil)
    scene))

(defn reanalyze-state
  [{:keys [reload-architecture namespace-path] :as state}]
  (if-not reload-architecture
    state
    (let [architecture (reload-architecture)
          path (vec (or namespace-path []))
          target-path (if (seq (get-in (view-architecture architecture path) [:graph :nodes]))
                        path
                        [])
          scene (build-scene-for-path architecture target-path (:viewport-width state))]
      (assoc state
             :architecture architecture
             :namespace-path target-path
             :nav-stack []
             :scene scene
             :reanalyze-requested? false
             :reanalyze-status nil
             :reanalyze-started-at nil
             :scroll-x 0.0
             :scroll-y 0.0
             :zoom 1.0
             :zoom-stack []
             :dependency-tooltip-key nil
             :dependency-tooltip-scroll 0.0
             :routed-edges nil))))

(defn start-reanalyze-state
  [{:keys [reload-architecture reanalyze-status] :as state}]
  (if (or (nil? reload-architecture)
          (= :running reanalyze-status))
    state
    (assoc state
           :reanalyze-requested? true
           :reanalyze-status :running
           :reanalyze-started-at (current-time-ms))))

(defn- push-nav-state
  [{:keys [namespace-path scroll-x scroll-y nav-stack] :as state}]
  (scene-state/push-nav-state state))

(defn show!
  ([scene]
   (show! scene {}))
  ([scene {:keys [title architecture reload-architecture]
           :or {title "architecture-viewer"}}]
   (let [effective-architecture (or architecture {:scene scene})
         initial-scene (initial-scene-for-show scene architecture)
         viewport-height (view-bootstrap/viewport-height-for-scene initial-scene content-height-for-scene)
         width (view-bootstrap/viewport-width-for-scene initial-scene content-width-for-scene)]
     (q/sketch
       :title title
       :size [width viewport-height]
       :features [:resizable]
       :setup (fn []
                (view-bootstrap/initial-sketch-state {:scene initial-scene
                                                      :architecture effective-architecture
                                                      :reload-architecture reload-architecture
                                                      :has-architecture? (boolean architecture)
                                                      :viewport-height viewport-height
                                                      :viewport-width width}))
       :update update-state
       :draw draw-scene
       :key-pressed handle-key-pressed
       :mouse-wheel handle-mouse-wheel
       :mouse-pressed handle-mouse-pressed
       :mouse-dragged handle-mouse-dragged
        :mouse-released handle-mouse-released
        :middleware [m/fun-mode]))))

(defn wait-until-closed!
  [sketch]
  (quil-lifecycle/wait-until-closed! sketch))
