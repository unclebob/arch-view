(ns arch-view.render.ui.quil.canvas-spec
  (:require [arch-view.render.ui.quil.canvas :as sut]
            [speclj.core :refer :all]))

(describe "quil canvas"
  (it "draw-scene-content renders dependency indicators via callback"
    (let [indicator-calls (atom nil)]
      (with-redefs [quil.core/background (fn [& _])
                    quil.core/fill (fn [& _])
                    quil.core/stroke (fn [& _])
                    quil.core/stroke-weight (fn [& _])
                    quil.core/rect (fn [& _])
                    quil.core/text-align (fn [& _])
                    quil.core/text (fn [& _])
                    quil.core/no-stroke (fn [& _])]
        (sut/draw-scene-content
         {:layer-rects [{:x 0 :y 0 :width 10 :height 10 :label "L" :abstract? false}]
          :module-positions [{:x 5 :y 5 :kind :concrete :label "m"}]
          :edge-drawables []}
         100
         [{:id 1} {:id 2}]
         {:rendered-label (fn [m] (:label m))
          :draw-dependency-indicators (fn [indicators]
                                        (reset! indicator-calls (mapv :id indicators)))}))
      (should= [1 2] @indicator-calls)))

  (it "draw-toolbar writes back label"
    (let [labels (atom [])]
      (with-redefs [quil.core/no-stroke (fn [& _])
                    quil.core/fill (fn [& _])
                    quil.core/rect (fn [& _])
                    quil.core/text-align (fn [& _])
                    quil.core/text (fn [label _ _] (swap! labels conj label))]
        (sut/draw-toolbar
         {:namespace-path ["a"] :nav-stack []}
         {:back-button-rect (fn [] {:x 0.0 :y 0.0 :width 10.0 :height 10.0})
          :back-button-label (fn [_] "Back")
          :toolbar-height 38.0})
      (should= ["Back"] @labels))))

  (it "draw-scene draws hovered tooltip text"
    (let [tooltips (atom [])]
      (with-redefs [quil.core/mouse-x (fn [] 40.0)
                    quil.core/mouse-y (fn [] 50.0)
                    quil.core/cursor (fn [& _])
                    quil.core/background (fn [& _])
                    quil.core/push-matrix (fn [] nil)
                    quil.core/scale (fn [& _])
                    quil.core/translate (fn [& _])
                    quil.core/pop-matrix (fn [] nil)]
        (sut/draw-scene
         {:scene {:layer-rects [] :module-positions [] :edge-drawables []}
          :declutter-mode :all
          :scroll-x 0.0
          :scroll-y 0.0
          :viewport-height 300
          :viewport-width 500
          :zoom 1.0}
         {:scaled-content-height (fn [_ _] 1000.0)
          :point-in-toolbar? (fn [_ _] false)
          :dependency-indicators (fn [& _] [{:tooltip-lines ["a->b(2)"]}])
          :hovered-dependency (fn [& _] {:tooltip-lines ["a->b(2)"]})
          :hovered-module-position (fn [& _] nil)
          :hovered-layer-label (fn [& _] nil)
          :draw-scene-content (fn [& _])
          :draw-toolbar (fn [& _])
          :draw-tooltip (fn [text _ _] (swap! tooltips conj text))
          :draw-tooltip-lines (fn [lines _ _]
                                (swap! tooltips into lines))
          :draw-scrollbar (fn [& _])})
        (should= ["a->b(2)"] @tooltips)))))
