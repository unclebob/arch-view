(ns arch-view.render.ui.quil.canvas-spec
  (:require [arch-view.render.ui.quil.canvas :as sut]
            [speclj.core :refer :all]))

(describe "quil canvas"
  (it "clamps tooltip origin inside canvas bounds"
    (let [fill-calls (atom [])
          text-calls (atom [])]
      (with-redefs [quil.core/width (fn [] 100.0)
                    quil.core/height (fn [] 80.0)
                    quil.core/fill (fn [& xs] (swap! fill-calls conj xs))
                    quil.core/stroke (fn [& _])
                    quil.core/rect (fn [& _])
                    quil.core/no-stroke (fn [& _])
                    quil.core/text-align (fn [& _])
                    quil.core/text (fn [t x y] (swap! text-calls conj [t x y]))]
        (sut/draw-tooltip "very-long-module-name" 95.0 75.0))
      (should-not= nil (seq @fill-calls))
      (let [[_ x y] (first @text-calls)]
        (should= true (<= x 94.0))
        (should= true (<= y 74.0)))))

  (it "draw-tooltip-lines colors cyclic lines red and non-cyclic black"
    (let [fills (atom [])]
      (with-redefs [quil.core/width (fn [] 300.0)
                    quil.core/height (fn [] 200.0)
                    quil.core/fill (fn [& xs] (swap! fills conj xs))
                    quil.core/stroke (fn [& _])
                    quil.core/rect (fn [& _])
                    quil.core/no-stroke (fn [& _])
                    quil.core/text-align (fn [& _])
                    quil.core/text (fn [& _])]
        (sut/draw-tooltip-lines [{:text "a" :cycle? true}
                                 {:text "b" :cycle? false}]
                                20.0 20.0))
      (should= true (some #(= [180 0 0] %) @fills))
      (should= true (some #(= [0 0 0] %) @fills))))

  (it "draw-tooltip-lines supports clipped scrolling for long dependency lists"
    (let [texts (atom [])]
      (with-redefs [quil.core/width (fn [] 300.0)
                    quil.core/height (fn [] 120.0)
                    quil.core/fill (fn [& _])
                    quil.core/stroke (fn [& _])
                    quil.core/rect (fn [& _])
                    quil.core/no-stroke (fn [& _])
                    quil.core/text-align (fn [& _])
                    quil.core/text (fn [text _ _] (swap! texts conj text))]
        (sut/draw-tooltip-lines (mapv (fn [n] {:text (str "line-" n) :cycle? false}) (range 8))
                                20.0 20.0
                                {:scroll-y 32.0
                                 :max-height 60.0}))
      (should-not= nil (some #{"line-2"} @texts))
      (should= nil (some #{"line-0"} @texts))))

  (it "draw-scrollbar renders when thumb exists and skips when missing"
    (let [rect-calls (atom 0)]
      (with-redefs [quil.core/no-stroke (fn [& _])
                    quil.core/fill (fn [& _])
                    quil.core/rect (fn [& _] (swap! rect-calls inc))]
        (sut/draw-scrollbar 1200.0 500.0 200.0 800.0
                            {:scrollbar-rect (fn [& _]
                                               {:x 100.0 :y 40.0 :width 8.0 :height 60.0})})
        (sut/draw-scrollbar 500.0 500.0 0.0 800.0
                            {:scrollbar-rect (fn [& _] nil)}))
      (should= 2 @rect-calls)))

  (it "draw-scene keeps hover inactive while cursor is on toolbar"
    (let [tooltip-lines (atom nil)]
      (with-redefs [quil.core/mouse-x (fn [] 20.0)
                    quil.core/mouse-y (fn [] 10.0)
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
          :point-in-toolbar? (fn [_ _] true)
          :dependency-indicators (fn [& _] [{:tooltip-lines ["a->b(2)"]}])
          :hovered-dependency (fn [& _] {:tooltip-lines ["a->b(2)"]})
          :hovered-module-position (fn [& _] {:full-name "x"})
          :hovered-layer-label (fn [& _] {:full-name "y"})
          :draw-scene-content (fn [& _])
          :draw-toolbar (fn [& _])
          :draw-tooltip (fn [& _] (reset! tooltip-lines :tooltip))
          :draw-tooltip-lines (fn [lines _ _] (reset! tooltip-lines lines))
          :draw-scrollbars (fn [& _])}))
      (should= nil @tooltip-lines)))

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

  (it "draw-scene-content renders abstract and leaf layers plus multiline labels"
    (let [rect-calls (atom [])
          text-calls (atom [])]
      (with-redefs [quil.core/background (fn [& _])
                    quil.core/fill (fn [& _])
                    quil.core/stroke (fn [& _])
                    quil.core/stroke-weight (fn [& _])
                    quil.core/rect (fn [& xs] (swap! rect-calls conj xs))
                    quil.core/text-align (fn [& _])
                    quil.core/text (fn [& xs] (swap! text-calls conj xs))
                    quil.core/no-stroke (fn [& _])]
        (sut/draw-scene-content
         {:layer-rects [{:x 20 :y 20 :width 100 :height 50 :abstract? true :leaf? false}
                        {:x 150 :y 20 :width 100 :height 50 :abstract? false :leaf? true}]
          :module-positions [{:x 70 :y 45 :kind :abstract :display-label "a.long_name"}
                             {:x 200 :y 45 :kind :concrete :display-label "leaf"}]}
         320
         []
         {:rendered-label (fn [m] (:display-label m))
          :rendered-label-lines (fn [m]
                                  (if (= "a.long_name" (:display-label m))
                                    ["a.long" "name"]
                                    [(:display-label m)]))
          :draw-dependency-indicators (fn [_])}))
      (should= 4 (count @rect-calls))
      (should= 3 (count @text-calls))))

  (it "draw-scene-content renders cycle list text below the diagram"
    (let [text-calls (atom [])]
      (with-redefs [quil.core/background (fn [& _])
                    quil.core/fill (fn [& _])
                    quil.core/stroke (fn [& _])
                    quil.core/stroke-weight (fn [& _])
                    quil.core/rect (fn [& _])
                    quil.core/text-align (fn [& _])
                    quil.core/text (fn [& xs] (swap! text-calls conj xs))
                    quil.core/no-stroke (fn [& _])]
        (sut/draw-scene-content
         {:layer-rects [{:x 20 :y 20 :width 100 :height 50 :abstract? false :leaf? false}]
          :module-positions [{:x 70 :y 45 :kind :concrete :display-label "node"}]
          :edge-drawables []
          :cycle-lines ["a->b->a" "c->d->c"]}
         320
         []
         {:rendered-label (fn [m] (:display-label m))
          :draw-dependency-indicators (fn [_])}))
      (should= true (some #(= ["Cycles:" 20.0 98.0] %) @text-calls))
      (should= true (some #(= ["a->b->a" 20.0 118.0] %) @text-calls))))

  (it "draw-scene-content falls back to rendered-label when line splitter is absent"
    (let [text-calls (atom [])]
      (with-redefs [quil.core/background (fn [& _])
                    quil.core/fill (fn [& _])
                    quil.core/stroke (fn [& _])
                    quil.core/stroke-weight (fn [& _])
                    quil.core/rect (fn [& _])
                    quil.core/text-align (fn [& _])
                    quil.core/text (fn [& xs] (swap! text-calls conj xs))
                    quil.core/no-stroke (fn [& _])]
        (sut/draw-scene-content
         {:layer-rects []
          :module-positions [{:x 10 :y 10 :kind :concrete :display-label "fallback"}]
          :edge-drawables []}
         120
         []
         {:rendered-label (fn [m] (:display-label m))
          :draw-dependency-indicators (fn [_])}))
      (should= 1 (count @text-calls))))

  (it "draw-toolbar writes back label"
    (let [labels (atom [])]
      (with-redefs [quil.core/no-stroke (fn [& _])
                    quil.core/fill (fn [& _])
                    quil.core/rect (fn [& _])
                    quil.core/text-align (fn [& _])
                    quil.core/text (fn [label _ _] (swap! labels conj label))]
        (sut/draw-toolbar
         {:namespace-path ["a"] :nav-stack [] :reload-architecture (fn [] nil)}
         {:back-button-rect (fn [] {:x 0.0 :y 0.0 :width 10.0 :height 10.0})
          :back-button-label (fn [_] "Back")
          :reanalyze-button-rect (fn [] {:x 20.0 :y 0.0 :width 10.0 :height 10.0})
          :reanalyze-button-label (fn [_] "Reanalyze")
          :toolbar-height 38.0})
      (should= ["Back" "Reanalyze"] @labels))))

  (it "draw-toolbar shows analyzing label while reanalyze is running"
    (let [labels (atom [])]
      (with-redefs [quil.core/no-stroke (fn [& _])
                    quil.core/fill (fn [& _])
                    quil.core/rect (fn [& _])
                    quil.core/text-align (fn [& _])
                    quil.core/text (fn [label _ _] (swap! labels conj label))]
        (sut/draw-toolbar
         {:namespace-path ["a"] :nav-stack [] :reload-architecture (fn [] nil) :reanalyze-status :running}
         {:back-button-rect (fn [] {:x 0.0 :y 0.0 :width 10.0 :height 10.0})
          :back-button-label (fn [_] "Back")
          :reanalyze-button-rect (fn [] {:x 20.0 :y 0.0 :width 10.0 :height 10.0})
          :reanalyze-button-label (fn [state] (if (= :running (:reanalyze-status state))
                                                "Analyzing..."
                                                "Reanalyze"))
          :toolbar-height 38.0})
      (should= ["Back" "Analyzing..."] @labels))))

  (it "draw-toolbar omits reanalyze label when reload is unavailable"
    (let [labels (atom [])]
      (with-redefs [quil.core/no-stroke (fn [& _])
                    quil.core/fill (fn [& _])
                    quil.core/rect (fn [& _])
                    quil.core/text-align (fn [& _])
                    quil.core/text (fn [label _ _] (swap! labels conj label))]
        (sut/draw-toolbar
         {:namespace-path ["a"] :nav-stack [] :reload-architecture nil}
         {:back-button-rect (fn [] {:x 0.0 :y 0.0 :width 10.0 :height 10.0})
          :back-button-label (fn [_] "Back")
          :reanalyze-button-rect (fn [] {:x 20.0 :y 0.0 :width 10.0 :height 10.0})
          :reanalyze-button-label (fn [_] "Reanalyze")
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
          :draw-tooltip-lines (fn [lines _ _ & _]
                                (swap! tooltips into lines))
          :draw-scrollbars (fn [& _])})
        (should= ["a->b(2)"] @tooltips))))

  (it "draw-hover-tooltip prefers module then layer then dependency"
    (let [calls (atom [])]
      (#'sut/draw-hover-tooltip {:full-name "module.core"}
                                {:full-name "layer.name"}
                                {:tooltip-lines [{:text "a->b"}]}
                                10.0 20.0
                                {:draw-tooltip (fn [text _ _] (swap! calls conj [:tooltip text]))
                                 :draw-tooltip-lines (fn [lines _ _ & _] (swap! calls conj [:lines lines]))})
      (#'sut/draw-hover-tooltip nil
                                {:full-name "layer.name"}
                                {:tooltip-lines [{:text "a->b"}]}
                                10.0 20.0
                                {:draw-tooltip (fn [text _ _] (swap! calls conj [:tooltip text]))
                                 :draw-tooltip-lines (fn [lines _ _ & _] (swap! calls conj [:lines lines]))})
      (#'sut/draw-hover-tooltip nil nil {:tooltip-lines [{:text "a->b"}]}
                                10.0 20.0
                                {:draw-tooltip (fn [text _ _] (swap! calls conj [:tooltip text]))
                                 :draw-tooltip-lines (fn [lines _ _ & _] (swap! calls conj [:lines lines]))})
      (should= [[:tooltip "module.core"]
                [:tooltip "layer.name"]
                [:lines [{:text "a->b"}]]]
               @calls))))
