(ns arch-view.render.ui.util.viewport-spec
  (:require [arch-view.render.ui.util.viewport :as sut]
            [speclj.core :refer :all]))

(describe "viewport helpers"
  (it "measures scene dimensions and scaling"
    (let [scene {:layer-rects [{:x 10.0 :y 20.0 :width 100.0 :height 60.0}
                               {:x 160.0 :y 120.0 :width 80.0 :height 50.0}]
                 :cycle-lines ["a->b->a" "c->d->c"]}]
      (should= 314.0 (sut/content-height-for-scene scene))
      (should= 500.0 (sut/content-width-for-scene scene 240.0))
      (should= 628.0 (sut/scaled-content-height scene 2.0))
      (should= 260.0 (sut/scaled-content-width scene 0.5 260.0))))

  (it "maps thumb positions and point containment"
    (let [rect {:x 100.0 :y 200.0 :width 20.0 :height 10.0}]
      (should= true (sut/point-in-rect? rect 110.0 205.0))
      (should= false (sut/point-in-rect? rect 121.0 205.0))
      (should= 0.0 (sut/thumb-y->scroll 12.0 300.0 400.0))
      (should= 0.0 (sut/thumb-y->scroll 12.0 1200.0 400.0))
      (should= 800.0 (sut/thumb-y->scroll 370.0 1200.0 400.0))
      (should= 0.0 (sut/thumb-x->scroll 12.0 300.0 400.0))
      (should= 0.0 (sut/thumb-x->scroll 12.0 1200.0 400.0))
      (should= 800.0 (sut/thumb-x->scroll 370.0 1200.0 400.0))))

  (it "handles zero-scroll and empty-scene edge cases"
    (should= 0.0 (sut/scroll-range 300.0 500.0))
    (should= nil (sut/scrollbar-rect 500.0 500.0 0.0 900.0))
    (should= nil (sut/horizontal-scrollbar-rect 500.0 500.0 0.0 900.0))
    (should= 40.0 (sut/content-height-for-scene {:layer-rects []}))
    (should= 200.0 (sut/content-width-for-scene {:layer-rects []} 180.0))))
