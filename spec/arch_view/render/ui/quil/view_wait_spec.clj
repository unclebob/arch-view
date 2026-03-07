(ns arch-view.render.ui.quil.view-wait-spec
  (:require [arch-view.render.ui.quil.view :as sut]
            [speclj.core :refer :all]))

(describe "quil view helper coverage"
  (it "checks safe displayability and stop condition"
    (should= true (#'sut/safe-displayable? nil true))
    (should= false (#'sut/safe-displayable? (Object.) false))
    (let [looping-calls (atom 0)]
      (with-redefs [sut/safe-looping? (fn [_] (swap! looping-calls inc) true)
                    sut/safe-displayable? (fn [_ _] false)]
        (sut/wait-until-closed! :sketch))
      (should= 1 @looping-calls)
      (sut/wait-until-closed! nil)))

  (it "builds sketch options in show"
    (let [captured (atom nil)
          scene {:layer-rects [{:x 0.0 :y 0.0 :width 300.0 :height 120.0}]
                 :module-positions []
                 :edge-drawables []}]
      (with-redefs [quil.core/sketch (fn [& args] (reset! captured (apply hash-map args)) :ok)]
        (should= :ok (sut/show! scene {:title "T"})))
      (should= "T" (:title @captured))
      (should= true (contains? @captured :mouse-released))))

  (it "covers route-helper branches"
    (let [bounds {:min-x 0.0 :max-x 500.0 :min-y 0.0 :max-y 500.0}]
      (should= 200.0 (#'sut/choose-route-column 100.0 100.0 300.0 300.0 {:all-rects [] :from-rect nil :to-rect nil} bounds))
      (should= [0.0 0.0] (first (#'sut/nudge-path [[0.0 0.0] [10.0 0.0] [10.0 10.0]] 0.0 10.0)))
      (should-not= nil (#'sut/place-non-overlapping-path [[0.0 0.0] [50.0 0.0] [100.0 0.0]]
                                                          {:all-rects []}
                                                          [[[0.0 0.0] [100.0 0.0]]]))))

  (it "keeps existing source anchor when already on rectangle edge"
    (let [from-rect {:x 400.0 :y 90.0 :width 40.0 :height 60.0}
          path [[420.0 90.0] [560.0 90.0] [560.0 240.0]]
          normalized (#'sut/normalize-route-endpoints path {:from-rect from-rect
                                                            :to-rect nil
                                                            :from-side :top
                                                            :to-side nil})]
      (should= [420.0 90.0] (first normalized))))

  (it "reanchors source when current point is not on rectangle edge"
    (let [from-rect {:x 400.0 :y 90.0 :width 40.0 :height 60.0}
          path [[425.0 95.0] [560.0 90.0] [560.0 240.0]]
          normalized (#'sut/normalize-route-endpoints path {:from-rect from-rect
                                                            :to-rect nil
                                                            :from-side :top
                                                            :to-side nil})]
      (should= true (#'sut/point-on-rect-edge? from-rect
                                               (first (first normalized))
                                               (second (first normalized))))))

  (it "spreads top edge anchors using dominant offset component"
    (let [rect {:x 400.0 :y 90.0 :width 40.0 :height 60.0}
          shifted (#'sut/apply-edge-constrained-offset [420.0 90.0] :top rect 0.0 12.0)]
      (should= [430.0 90.0] shifted))))
