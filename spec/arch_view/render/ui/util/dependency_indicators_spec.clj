(ns arch-view.render.ui.util.dependency-indicators-spec
  (:require [arch-view.render.ui.util.dependency-indicators :as sut]
            [quil.core :as q]
            [speclj.core :refer :all]))

(describe "dependency indicators"
  (it "builds incoming and outgoing indicators with full names"
    (let [scene {:layer-rects [{:module "a" :x 100.0 :y 80.0 :width 80.0 :height 40.0}
                               {:module "b" :x 280.0 :y 80.0 :width 80.0 :height 40.0}]
                 :module-positions [{:module "a" :full-name "empire.alpha"}
                                    {:module "b" :full-name "empire.beta"}]
                 :edge-drawables [{:from "a" :to "b" :type :direct :cycle-break? true}
                                  {:from "c" :to "a" :type :direct :cycle-break? false}]}
          indicators (sut/dependency-indicators scene :all (fn [s] (str "stripped:" s)))
          incoming-a (first (filter #(and (= "a" (:module %))
                                          (= :incoming (:direction %)))
                                    indicators))
          outgoing-a (first (filter #(and (= "a" (:module %))
                                          (= :outgoing (:direction %)))
                                    indicators))]
      (should= 3 (count indicators))
      (should= [{:text "stripped:c" :cycle? false}] (:tooltip-lines incoming-a))
      (should= [{:text "empire.beta" :cycle? true}] (:tooltip-lines outgoing-a))
      (should= true (:cycle? outgoing-a))))

  (it "draws triangles with cycle and non-cycle fill colors"
    (let [calls (atom [])]
      (with-redefs [q/no-stroke (fn [] (swap! calls conj :no-stroke))
                    q/fill (fn [& args] (swap! calls conj [:fill args]))
                    q/triangle (fn [& args] (swap! calls conj [:triangle args]))]
        (sut/draw-dependency-indicators [{:triangle [[0.0 0.0] [1.0 0.0] [0.5 1.0]]
                                          :cycle? false}
                                         {:triangle [[2.0 2.0] [3.0 2.0] [2.5 3.0]]
                                          :cycle? true}]))
      (should= :no-stroke (first @calls))
      (should= true (some #(= [:fill [0 0 0]] %) @calls))
      (should= true (some #(= [:fill [180 0 0]] %) @calls))
      (should= [:triangle [2.0 2.0 3.0 2.0 2.5 3.0]] (last @calls))))

  (it "hovers when inside triangle or within hover tolerance"
    (let [indicators [{:module "a"
                       :direction :incoming
                       :triangle [[10.0 10.0] [22.0 10.0] [16.0 20.392304845]]}
                      {:module "b"
                       :direction :outgoing
                       :triangle [[40.0 10.0] [52.0 10.0] [46.0 20.392304845]]}]
          inside (sut/hovered-dependency indicators 16.0 14.0)
          near-edge (sut/hovered-dependency indicators 16.0 6.5)
          none (sut/hovered-dependency indicators 80.0 80.0)]
      (should= "a" (:module inside))
      (should= "a" (:module near-edge))
      (should= nil none))))
