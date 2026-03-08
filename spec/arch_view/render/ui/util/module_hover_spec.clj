(ns arch-view.render.ui.util.module-hover-spec
  (:require [arch-view.render.ui.util.module-hover :as sut]
            [speclj.core :refer :all]))

(describe "module hover"
  (it "returns full module position map when hovering a module"
    (let [positions [{:module "alpha.core" :x 100.0 :y 50.0 :display-label "alpha.core"}
                     {:module "beta.core" :x 260.0 :y 50.0 :display-label "beta.core"}]
          lines (fn [m] [(:display-label m)])
          width (fn [s] (* 7.0 (count s)))
          hovered (sut/hovered-module-position positions 100.0 50.0 lines width)]
      (should= "alpha.core" (:module hovered))
      (should= nil (sut/hovered-module-position positions 400.0 400.0 lines width))))

  (it "returns module id and layer when hovering labels"
    (let [positions [{:module "alpha.core" :x 100.0 :y 50.0 :display-label "alpha.core"}]
          layers [{:x 20.0 :y 10.0 :label "game"}]
          lines (fn [m] [(:display-label m)])
          width (fn [s] (* 7.0 (count s)))]
      (should= "alpha.core" (sut/hovered-module positions 100.0 50.0 lines width))
      (should= nil (sut/hovered-module positions 0.0 0.0 lines width))
      (should-not= nil (sut/hovered-layer-label layers 30.0 20.0 width))
      (should= nil (sut/hovered-layer-label layers 2.0 2.0 width)))))
