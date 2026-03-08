(ns arch-view.render.quil-view-spec
  (:require [arch-view.render.ui.quil.view :as sut]
            [speclj.core :refer :all]))

(describe "quil scene model"
  (it "builds scene rectangles and module positions"
    (let [architecture {:layout {:layers [{:index 0 :modules ["a"]}
                                          {:index 1 :modules ["b"]}]
                                 :module->layer {"a" 0 "b" 1}}
                        :classified-edges #{{:from "a" :to "b" :type :direct}}}
          scene (sut/build-scene architecture {:canvas-width 1000 :layer-height 120 :layer-gap 30})]
      (should= 2 (count (:layer-rects scene)))
      (should= 2 (count (:module-positions scene)))
      (should= 1 (count (:edge-drawables scene)))))

  (it "computes arrowhead geometry and line endpoint"
    (let [right (sut/arrowhead-points 0 0 10 0 :standard)
          up (sut/arrowhead-points 0 10 0 0 :closed-triangle)]
      (should= [10 0] (:tip right))
      (should= [0.0 0.0] (:center right))
      (should= [0 0] (:tip up))
      (should= [0.0 10.0] (:center up))
      (should= [10 0] (sut/edge-line-endpoint 0 0 10 0 :standard))
      (should= [0.0 0.0] (sut/edge-line-endpoint 0 0 10 0 :closed-triangle))))

  (it "offsets dependency start and tip points"
    (should= [100.0 112.0] (sut/dependency-tip-point 100 200 100 100))
    (should= [100.0 188.0] (sut/dependency-tip-point 100 100 100 200))
    (should= [100.0 188.0] (sut/dependency-start-point 100 200 100 100))
    (should= [100.0 112.0] (sut/dependency-start-point 100 100 100 200)))

  (it "abbreviates and strips top namespace"
    (should= "b.c.core" (sut/abbreviate-module-name "app.backend.cache.core"))
    (should= "j.alice" (sut/abbreviate-module-name "bob.jim.alice.clj"))
    (should= "beta.module" (sut/strip-top-namespace "alpha.beta.module")))

  (it "finds hovered module and layer labels"
    (let [modules [{:module "alpha.beta.core" :label "a.b.core" :x 100.0 :y 50.0}]
          layers [{:index 0 :x 0.0 :y 0.0 :width 100.0 :height 60.0 :label "alpha" :full-name "alpha"}]]
      (should= "alpha.beta.core" (sut/hovered-module modules 100.0 50.0))
      (should= nil (sut/hovered-module modules 300.0 300.0))
      (should-not= nil (sut/hovered-layer-label layers 12.0 10.0))
      (should= nil (sut/hovered-layer-label layers 200.0 80.0))))

  (it "builds and hovers dependency indicators"
    (let [scene {:layer-rects [{:module "a" :index 0 :x 100.0 :y 100.0 :width 80.0 :height 40.0}
                               {:module "b" :index 1 :x 260.0 :y 100.0 :width 80.0 :height 40.0}]
                 :module-positions [{:module "a" :full-name "ui.alpha"}
                                    {:module "b" :full-name "ui.beta"}]
                 :edge-drawables [{:from "a" :to "b" :type :direct :cycle-break? true}
                                  {:from "b" :to "a" :type :direct :cycle-break? false}]}
          indicators (sut/dependency-indicators scene :all)
          in-a (first (filter #(and (= "a" (:module %)) (= :incoming (:direction %))) indicators))
          out-a (first (filter #(and (= "a" (:module %)) (= :outgoing (:direction %))) indicators))
          hovered (sut/hovered-dependency indicators 140.0 100.0)]
      (should= 4 (count indicators))
      (should= false (:cycle? in-a))
      (should= true (:cycle? out-a))
      (should-not= nil hovered)))

  (it "computes vertical scrolling metrics"
    (should= 600.0 (sut/scroll-range 1200 600))
    (should= 0.0 (sut/clamp-scroll -20 1200 600))
    (should= 600.0 (sut/clamp-scroll 900 1200 600))
    (should= 0.0 (sut/clamp-scroll-x -1 1000 500))
    (should= 500.0 (sut/clamp-scroll-x 900 1000 500))
    (should= nil (sut/scrollbar-rect 400 500 0 1200))
    (should-not= nil (sut/scrollbar-rect 2000 500 300 1200))
    (should= 0.0 (sut/thumb-y->scroll 12.0 2000 500)))

  (it "labels back button using the current drill path when present"
    (should= "Back" (#'sut/back-button-label {:namespace-path [] :nav-stack []}))
    (should= "Back: movement"
             (#'sut/back-button-label {:namespace-path ["game-mechanics" "movement"]
                                       :nav-stack [{:namespace-path ["game-mechanics"]}]}))))
