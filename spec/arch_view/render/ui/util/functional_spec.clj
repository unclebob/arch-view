(ns arch-view.render.ui.util.functional-spec
  (:require [arch-view.render.ui.util.functional :as sut]
            [speclj.core :refer :all]))

(describe "functional helper coverage"
  (it "aggregates edges by layer, ignores invalid pairs, and promotes abstract"
    (let [scene {:edge-drawables [{:from "a" :to "b" :type :direct}
                                  {:from "c" :to "d" :type :abstract}
                                  {:from "e" :to "e" :type :direct}
                                  {:from "x" :to "y" :type :direct}]}
          module->layer {"a" 0 "b" 1
                         "c" 0 "d" 1
                         "e" 2}
          grouped (#'sut/aggregate-layer-edges scene module->layer)]
      (should= :abstract (get grouped [0 1]))
      (should= nil (get grouped [2 2]))
      (should= 1 (count grouped))))

  (it "assigns first available lane and opens a new lane when overlapping"
    (let [edge-a {:from-point [0.0 0.0] :to-point [0.0 100.0]}
          edge-b {:from-point [200.0 0.0] :to-point [200.0 100.0]}
          edge-c {:from-point [0.0 20.0] :to-point [0.0 120.0]}
          first-assignment (#'sut/assign-lane [] edge-a)
          reused-assignment (#'sut/assign-lane (:lanes first-assignment) edge-b)
          new-lane-assignment (#'sut/assign-lane (:lanes first-assignment) edge-c)]
      (should= 0 (:lane-idx first-assignment))
      (should= 0 (:lane-idx reused-assignment))
      (should= 1 (:lane-idx new-lane-assignment))))

  (it "builds base edges from layer centers or rectangle fallback centers"
    (let [layer-rects {0 {:x 0.0 :y 0.0 :width 100.0 :height 80.0}
                       1 {:x 200.0 :y 40.0 :width 120.0 :height 60.0}}
          explicit-centers {0 [[20.0 30.0]]
                            1 [[250.0 90.0]]}
          centered (#'sut/make-layer-base-edge layer-rects explicit-centers [0 1] :direct)
          fallback (#'sut/make-layer-base-edge layer-rects {} [0 1] :abstract)]
      (should= [20.0 30.0] (:from-point centered))
      (should= [250.0 90.0] (:to-point centered))
      (should= [50.0 40.0] (:from-point fallback))
      (should= [260.0 70.0] (:to-point fallback))
      (should= :closed-triangle (:arrowhead fallback))))

  (it "declutters edge drawables across all modes"
    (let [scene {:module-positions [{:module "a" :layer 0 :x 20.0 :y 20.0}
                                    {:module "b" :layer 1 :x 20.0 :y 120.0}]
                 :layer-rects [{:index 0 :x 0.0 :y 0.0 :width 100.0 :height 80.0}
                               {:index 1 :x 0.0 :y 100.0 :width 100.0 :height 80.0}]
                 :edge-drawables [{:from "a" :to "b" :type :direct :arrowhead :standard}
                                  {:from "b" :to "a" :type :abstract :arrowhead :closed-triangle}]}
          concrete (sut/declutter-edge-drawables scene :concrete)
          abstract (sut/declutter-edge-drawables scene :abstract)
          layer (sut/declutter-edge-drawables scene :layer)
          all (sut/declutter-edge-drawables scene :all)]
      (should= 1 (count concrete))
      (should= :direct (:type (first concrete)))
      (should= 1 (count abstract))
      (should= :abstract (:type (first abstract)))
      (should= 2 (count layer))
      (should= 2 (count all))))

  (it "assigns unique slots and keeps heavy fan-in near center track"
    (let [placement (sut/assign-layer-slots [0 1 2] [[0 1] [2 1]] {0 0 1 4 2 0})
          slots (vals placement)]
      (should= 3 (count (set slots)))
      (should= 2 (get-in placement [1 :track]))))

  (it "labels declutter modes with expected text"
    (should= "View: All" (sut/declutter-label :all))
    (should= "View: Abstract" (sut/declutter-label :abstract))
    (should= "View: Concrete" (sut/declutter-label :concrete))
    (should= "View: All" (sut/declutter-label :layer))
    (should= "View: All" (sut/declutter-label :unknown)))

  (it "cycles declutter mode and anchors on rectangle edges"
    (should= :abstract (sut/next-declutter-mode :all))
    (should= :concrete (sut/next-declutter-mode :abstract))
    (should= :all (sut/next-declutter-mode :concrete))
    (should= :all (sut/next-declutter-mode :layer))
    (let [rect {:x 10.0 :y 20.0 :width 100.0 :height 80.0}]
      (should= :right (:side (sut/rect-edge-anchor rect 200.0 50.0)))
      (should= :left (:side (sut/rect-edge-anchor rect -50.0 50.0)))
      (should= :bottom (:side (sut/rect-edge-anchor rect 60.0 200.0)))
      (should= :top (:side (sut/rect-edge-anchor rect 60.0 -20.0))))))
