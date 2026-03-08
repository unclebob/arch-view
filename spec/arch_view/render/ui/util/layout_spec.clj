(ns arch-view.render.ui.util.layout-spec
  (:require [arch-view.render.ui.util.layout :as sut]
            [speclj.core :refer :all]))

(describe "layout helpers"
  (it "computes dependency pairs and incoming counts by layer"
    (let [pairs (sut/dependency-pairs-by-layer
                 [{:from "a" :to "b"}
                  {:from "b" :to "c"}
                  {:from "x" :to "missing"}]
                 {"a" 0 "b" 1 "c" 1 "x" 2})
          incoming (sut/incoming-counts-by-layer [0 1 2] pairs)]
      (should= [[0 1] [1 1]] pairs)
      (should= {0 0 1 1 2 0} incoming)))

  (it "detects segment and edge crossings"
    (let [placement {0 {:track 0 :row 0}
                     1 {:track 1 :row 1}
                     2 {:track 0 :row 1}
                     3 {:track 1 :row 0}}]
      (should= true (sut/segment-crosses? [0 0] [1 1] [0 1] [1 0]))
      (should= false (sut/segment-crosses? [0 0] [1 0] [0 1] [1 1]))
      (should= true (sut/edge-cross? placement [0 1] [2 3]))
      (should= false (sut/edge-cross? placement [0 1] [1 2]))
      (should= [0.0 0.0] (sut/edge-point placement 0))
      (should= nil (sut/edge-point placement 99))))

  (it "assigns non-overlapping slots for layer indexes"
    (let [placement (sut/assign-layer-slots [0 1 2]
                                            [[0 1] [2 1]]
                                            {0 0 1 2 2 0})]
      (should= 3 (count placement))
      (should= 3 (count (set (vals placement)))))))
