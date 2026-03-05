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
               (:layer-rects scene))
      (should= ["a" "c"]
               (->> (:module-positions scene)
                    (filter #(= 2 (:layer %)))
                    (sort-by :x)
                    (mapv :module)))
      (should= #{{:from "a" :to "b" :arrowhead :standard}
                 {:from "c" :to "d" :arrowhead :closed-triangle}}
               (set (map #(select-keys % [:from :to :arrowhead])
                         (:edge-drawables scene)))))))
