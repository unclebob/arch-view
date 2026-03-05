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
      (should= ["layer-0" "layer-1" "layer-2"]
               (mapv :label (:layer-rects scene)))
      (should= #{{:from "a" :to "b" :arrowhead :standard}
                 {:from "c" :to "d" :arrowhead :closed-triangle}}
               (set (map #(select-keys % [:from :to :arrowhead])
                         (:edge-drawables scene))))))

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

  (it "exits sketch when escape is pressed"
    (let [exited? (atom false)]
      (with-redefs [quil.core/exit (fn [] (reset! exited? true))]
        (sut/handle-key-pressed {} {:key :escape}))
      (should= true @exited?)))

  (it "ignores non-escape key presses"
    (let [exited? (atom false)]
      (with-redefs [quil.core/exit (fn [] (reset! exited? true))]
        (sut/handle-key-pressed {} {:key :a}))
      (should= false @exited?))))
