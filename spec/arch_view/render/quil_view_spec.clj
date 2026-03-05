(ns arch-view.render.quil-view-spec
  (:require [arch-view.render.quil-view :as sut]
            [speclj.core :refer :all]))

(describe "quil scene model"
  (it "builds full-width layer rectangles with horizontal module positions"
    (let [architecture {:layout {:layers [{:index 0 :modules ["d"]}
                                          {:index 1 :modules ["b"]}
                                          {:index 2 :modules ["a" "c"]}]
                                 :module->layer {"a" 2 "b" 1 "c" 2 "d" 0}}
                        :classified-edges #{{:from "a" :to "b" :type :direct}
                                            {:from "c" :to "d" :type :abstract}}}
          scene (sut/build-scene architecture {:canvas-width 1000 :layer-height 120 :layer-gap 30})]
      (should= [{:index 0 :x 0 :y 0 :width 1000 :height 120}
                {:index 1 :x 0 :y 150 :width 1000 :height 120}
                {:index 2 :x 0 :y 300 :width 1000 :height 120}]
               (mapv #(dissoc % :label) (:layer-rects scene)))
      (should= ["a" "c"]
               (->> (:module-positions scene)
                    (filter #(= 2 (:layer %)))
                    (sort-by :x)
                    (mapv :module)))
      (should= ["a" "c"]
               (->> (:module-positions scene)
                    (filter #(= 2 (:layer %)))
                    (sort-by :x)
                    (mapv :label)))
      (should= ["layer-0" "layer-1" "layer-2"]
               (mapv :label (:layer-rects scene)))
      (should= #{{:from "a" :to "b" :arrowhead :standard}
                 {:from "c" :to "d" :arrowhead :closed-triangle}}
               (set (map #(select-keys % [:from :to :arrowhead])
                         (:edge-drawables scene))))))

  (it "staggers module label y positions when horizontal space is tight"
    (let [architecture {:layout {:layers [{:index 0 :modules ["alpha.beta.long-module-one"
                                                               "alpha.beta.long-module-two"
                                                               "alpha.beta.long-module-three"]}]
                                 :module->layer {"alpha.beta.long-module-one" 0
                                                 "alpha.beta.long-module-two" 0
                                                 "alpha.beta.long-module-three" 0}}
                        :classified-edges #{}}
          scene (sut/build-scene architecture {:canvas-width 260 :layer-height 120 :layer-gap 30})
          ys (->> (:module-positions scene) (map :y) distinct sort vec)]
      (should= [60.0 70.0] ys)))

  (it "keeps module labels aligned when there is enough horizontal space"
    (let [architecture {:layout {:layers [{:index 0 :modules ["alpha.beta.one"
                                                               "alpha.beta.two"
                                                               "alpha.beta.three"]}]
                                 :module->layer {"alpha.beta.one" 0
                                                 "alpha.beta.two" 0
                                                 "alpha.beta.three" 0}}
                        :classified-edges #{}}
          scene (sut/build-scene architecture {:canvas-width 1400 :layer-height 120 :layer-gap 30})
          ys (->> (:module-positions scene) (map :y) distinct sort vec)]
      (should= [60] ys)))

  (it "computes arrowhead points in dependency direction"
    (let [right (sut/arrowhead-points 0 0 10 0 :standard)
          up (sut/arrowhead-points 0 10 0 0 :closed-triangle)]
      (should= [10 0] (:tip right))
      (should= [0.0 0.0] (:center right))
      (should= [0.0 5.0] (:left right))
      (should= [0.0 -5.0] (:right right))
      (should= [0 0] (:tip up))
      (should= [0.0 10.0] (:center up))
      (should= [5.0 10.0] (:left up))
      (should= [-5.0 10.0] (:right up))
      (should= true (:closed? up))))

  (it "ends abstract dependency line at arrowhead base"
    (let [standard-end (sut/edge-line-endpoint 0 0 10 0 :standard)
          abstract-end (sut/edge-line-endpoint 0 0 10 0 :closed-triangle)]
      (should= [10 0] standard-end)
      (should= [0.0 0.0] abstract-end)))

  (it "offsets target tip by direction to avoid label overlap"
    (should= [100.0 112.0] (sut/dependency-tip-point 100 200 100 100))
    (should= [100.0 188.0] (sut/dependency-tip-point 100 100 100 200))
    (should= [100.0 100.0] (sut/dependency-tip-point 200 100 100 100)))

  (it "offsets source start by direction to avoid label overlap"
    (should= [100.0 188.0] (sut/dependency-start-point 100 200 100 100))
    (should= [100.0 112.0] (sut/dependency-start-point 100 100 100 200))
    (should= [100.0 100.0] (sut/dependency-start-point 100 100 200 100)))

  (it "abbreviates module names using parent initials"
    (should= "b.module-name" (sut/abbreviate-module-name "alpha.beta.module-name"))
    (should= "module-name" (sut/abbreviate-module-name "module-name"))
    (should= "b.c.core" (sut/abbreviate-module-name "app.backend.cache.core")))

  (it "strips top-level namespace for hover names"
    (should= "beta.module" (sut/strip-top-namespace "alpha.beta.module"))
    (should= "module" (sut/strip-top-namespace "module")))

  (it "finds hovered module by label hitbox"
    (let [modules [{:module "alpha.beta.core"
                    :label "a.b.core"
                    :x 100.0
                    :y 50.0}]]
      (should= "alpha.beta.core" (sut/hovered-module modules 100.0 50.0))
      (should= nil (sut/hovered-module modules 300.0 300.0))))

  (it "computes and clamps vertical scroll range"
    (should= 600.0 (sut/scroll-range 1200 600))
    (should= 0.0 (sut/scroll-range 300 600))
    (should= 0.0 (sut/clamp-scroll -20 1200 600))
    (should= 300.0 (sut/clamp-scroll 300 1200 600))
    (should= 600.0 (sut/clamp-scroll 900 1200 600)))

  (it "builds scrollbar thumb when content exceeds viewport"
    (let [bar (sut/scrollbar-rect 2000 500 300 1200)]
      (should-not= nil bar)
      (should= 1188.0 (:x bar))
      (should= 8.0 (:width bar))
      (should= true (> (:height bar) 0.0))))

  (it "omits scrollbar when content fits viewport"
    (should= nil (sut/scrollbar-rect 400 500 0 1200)))

  (it "maps scrollbar thumb y back to scroll offset"
    (should= 0.0 (sut/thumb-y->scroll 12.0 2000 500))
    (should= true (> (sut/thumb-y->scroll 300.0 2000 500) 0.0))
    (should= 1500.0 (sut/thumb-y->scroll 10000.0 2000 500)))

  (it "exits sketch when escape is pressed"
    (let [exited? (atom false)]
      (with-redefs [quil.core/exit (fn [] (reset! exited? true))]
        (sut/handle-key-pressed {} {:key :escape}))
      (should= true @exited?)))

  (it "ignores non-escape key presses"
    (let [exited? (atom false)]
      (with-redefs [quil.core/exit (fn [] (reset! exited? true))]
        (sut/handle-key-pressed {} {:key :a}))
      (should= false @exited?)))

  (it "drills down when a clickable namespace label is clicked"
    (let [architecture {:graph {:nodes #{"empire.alpha.one"
                                         "empire.alpha.two"
                                         "empire.beta.one"}
                                :edges #{}
                                :abstract-modules #{}}
                        :classified-edges #{}}
          root-view (sut/view-architecture architecture [])
          root-scene (sut/build-scene root-view)
          alpha-pos (some #(when (= "alpha" (:module %)) %) (:module-positions root-scene))
          state {:scene root-scene
                 :architecture architecture
                 :namespace-path []
                 :scroll-y 0.0
                 :dragging-scrollbar? false
                 :drag-offset nil
                 :viewport-height 600
                 :viewport-width 1200}
          next-state (sut/handle-mouse-clicked state {:x (:x alpha-pos) :y (:y alpha-pos)})]
      (should= ["alpha"] (:namespace-path next-state))
      (should= #{"one" "two"}
               (->> (get-in next-state [:scene :module-positions])
                    (map :module)
                    set))))

  (it "keeps state unchanged when clicked namespace has no deeper children"
    (let [architecture {:graph {:nodes #{"empire.alpha.one"}
                                :edges #{}
                                :abstract-modules #{}}
                        :classified-edges #{}}
          alpha-view (sut/view-architecture architecture ["alpha"])
          alpha-scene (sut/build-scene alpha-view)
          one-pos (some #(when (= "one" (:module %)) %) (:module-positions alpha-scene))
          state {:scene alpha-scene
                 :architecture architecture
                 :namespace-path ["alpha"]
                 :scroll-y 0.0
                 :dragging-scrollbar? false
                 :drag-offset nil
                 :viewport-height 600
                 :viewport-width 1200}
          next-state (sut/handle-mouse-clicked state {:x (:x one-pos) :y (:y one-pos)})]
      (should= state next-state)))

  (it "marks drillable namespace labels with a plus prefix"
    (let [architecture {:graph {:nodes #{"empire.alpha.one"
                                         "empire.alpha.two"
                                         "empire.beta"}
                                :edges #{}
                                :abstract-modules #{}}
                        :classified-edges #{}}
          root-view (sut/view-architecture architecture [])
          root-scene (sut/build-scene root-view)
          marked (sut/attach-drillable-markers root-scene architecture [])
          labels (into {} (map (juxt :module :display-label) (:module-positions marked)))]
      (should= "+ alpha" (get labels "alpha"))
      (should= "beta" (get labels "beta"))))

  (it "builds grouped initial scene for show when architecture is provided"
    (let [architecture {:graph {:nodes #{"empire.alpha.one"
                                         "empire.alpha.two"
                                         "empire.beta.one"}
                                :edges #{}
                                :abstract-modules #{}}
                        :classified-edges #{}}
          module-scene {:layer-rects [{:index 0 :x 0 :y 0 :width 1000 :height 100 :label "modules"}]
                        :module-positions [{:module "empire.alpha.one" :x 200 :y 50 :label "alpha.one"}
                                           {:module "empire.beta.one" :x 600 :y 50 :label "beta.one"}]
                        :edge-drawables []}
          initial (sut/initial-scene-for-show module-scene architecture)
          modules (->> (:module-positions initial) (map :module) set)
          labels (into {} (map (juxt :module :display-label) (:module-positions initial)))]
      (should= #{"alpha" "beta"} modules)
      (should= "+ alpha" (get labels "alpha"))
      (should= "+ beta" (get labels "beta"))))

  (it "cycles declutter modes across all four states"
    (should= :concrete (sut/next-declutter-mode :all))
    (should= :abstract (sut/next-declutter-mode :concrete))
    (should= :between-layers (sut/next-declutter-mode :abstract))
    (should= :all (sut/next-declutter-mode :between-layers)))

  (it "filters edges by declutter mode"
    (let [scene {:module-positions [{:module "a" :layer 0 :x 100 :y 60}
                                    {:module "b" :layer 1 :x 200 :y 200}
                                    {:module "c" :layer 2 :x 300 :y 340}]
                 :layer-rects [{:index 0 :x 0 :y 0 :width 400 :height 100}
                               {:index 1 :x 0 :y 120 :width 400 :height 100}
                               {:index 2 :x 0 :y 240 :width 400 :height 100}]
                 :edge-drawables [{:from "a" :to "b" :type :direct :arrowhead :standard}
                                  {:from "b" :to "c" :type :abstract :arrowhead :closed-triangle}]}
          concrete (sut/declutter-edge-drawables scene :concrete)
          abstract (sut/declutter-edge-drawables scene :abstract)
          layer-mode (sut/declutter-edge-drawables scene :between-layers)]
      (should= 1 (count concrete))
      (should= :direct (:type (first concrete)))
      (should= 1 (count abstract))
      (should= :abstract (:type (first abstract)))
      (should= 2 (count layer-mode))))

  (it "collapses layer edges and promotes abstract dependency type"
    (let [scene {:module-positions [{:module "a1" :layer 0 :x 100 :y 60}
                                    {:module "a2" :layer 0 :x 200 :y 60}
                                    {:module "b1" :layer 1 :x 120 :y 200}]
                 :layer-rects [{:index 0 :x 0 :y 0 :width 400 :height 100}
                               {:index 1 :x 0 :y 120 :width 400 :height 100}]
                 :edge-drawables [{:from "a1" :to "b1" :type :direct :arrowhead :standard}
                                  {:from "a2" :to "b1" :type :abstract :arrowhead :closed-triangle}]}
          layer-edges (sut/layer-edge-drawables scene)]
      (should= 1 (count layer-edges))
      (should= :abstract (:type (first layer-edges)))
      (should= :closed-triangle (:arrowhead (first layer-edges)))
      (should= [200.0 100] (:from-point (first layer-edges)))
      (should= [200.0 120] (:to-point (first layer-edges)))))

  (it "positions upward layer arrows from top of lower layer to bottom of upper layer"
    (let [scene {:module-positions [{:module "lower" :layer 2 :x 100 :y 300}
                                    {:module "upper" :layer 1 :x 120 :y 180}]
                 :layer-rects [{:index 1 :x 0 :y 120 :width 400 :height 100}
                               {:index 2 :x 0 :y 240 :width 400 :height 100}]
                 :edge-drawables [{:from "lower" :to "upper" :type :direct :arrowhead :standard}]}
          edge (first (sut/layer-edge-drawables scene))]
      (should= [200.0 240] (:from-point edge))
      (should= [200.0 220] (:to-point edge))))

  (it "cycles declutter mode when declutter button is clicked"
    (let [state {:scene {:module-positions [] :layer-rects [] :edge-drawables []}
                 :architecture nil
                 :namespace-path nil
                 :declutter-mode :all
                 :dragging-scrollbar? false}
          next-state (sut/handle-mouse-clicked state {:x 120.0 :y 10.0})]
      (should= :concrete (:declutter-mode next-state))))

  (it "back button pops one namespace level"
    (let [architecture {:graph {:nodes #{"empire.alpha.one"
                                         "empire.alpha.two"
                                         "empire.beta.one"}
                                :edges #{}
                                :abstract-modules #{}}
                        :classified-edges #{}}
          alpha-view (sut/view-architecture architecture ["alpha"])
          alpha-scene (sut/attach-drillable-markers (sut/build-scene alpha-view) architecture ["alpha"])
          state {:scene alpha-scene
                 :architecture architecture
                 :namespace-path ["alpha"]
                 :nav-stack [{:path [] :scroll-y 140.0}]
                 :declutter-mode :all
                 :scroll-y 40.0
                 :dragging-scrollbar? false
                 :drag-offset nil
                 :viewport-height 600
                 :viewport-width 1200}
          next-state (sut/handle-mouse-clicked state {:x 20.0 :y 10.0})]
      (should= [] (:namespace-path next-state))
      (should= 140.0 (:scroll-y next-state))
      (should= #{"alpha" "beta"}
               (->> (get-in next-state [:scene :module-positions]) (map :module) set)))))

  (it "clicking a namespace pushes current scroll and resets to top"
    (let [architecture {:graph {:nodes #{"empire.alpha.one"
                                         "empire.alpha.two"
                                         "empire.beta.one"}
                                :edges #{}
                                :abstract-modules #{}}
                        :classified-edges #{}}
          root-view (sut/view-architecture architecture [])
          root-scene (sut/attach-drillable-markers (sut/build-scene root-view) architecture [])
          alpha-pos (some #(when (= "alpha" (:module %)) %) (:module-positions root-scene))
          state {:scene root-scene
                 :architecture architecture
                 :namespace-path []
                 :nav-stack []
                 :declutter-mode :all
                 :scroll-y 123.0
                 :dragging-scrollbar? false
                 :drag-offset nil
                 :viewport-height 600
                 :viewport-width 1200}
          next-state (sut/handle-mouse-clicked state {:x (:x alpha-pos) :y (- (:y alpha-pos) 123.0)})]
      (should= ["alpha"] (:namespace-path next-state))
      (should= 0.0 (:scroll-y next-state))
      (should= [{:path [] :scroll-y 123.0}] (:nav-stack next-state))))

  (it "back button at top level does nothing"
    (let [state {:scene {:module-positions [] :layer-rects [] :edge-drawables []}
                 :architecture nil
                 :namespace-path []
                 :nav-stack []
                 :declutter-mode :all
                 :dragging-scrollbar? false}
          next-state (sut/handle-mouse-clicked state {:x 20.0 :y 10.0})]
      (should= state next-state)))
