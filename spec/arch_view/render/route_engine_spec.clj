(ns arch-view.render.route-engine-spec
  (:require [arch-view.render.route-engine :as sut]
            [speclj.core :refer :all]))

(describe "route engine"
  (it "routes edges and accumulates placed segments"
    (let [edges [{:id :e1 :from "a" :to "b" :from-rect {:x 0}}
                 {:id :e2 :from "c" :to "d"}]
          out (sut/route-edges
               {:spaced-edges edges
                :resolve-edge-path (fn [edge]
                                     {:points (if (= :e1 (:id edge))
                                                [[0 0] [10 0] [10 10]]
                                                [[20 0] [20 10]])})
                :normalize-route-endpoints (fn [points _] points)
                :place-non-overlapping-path (fn [base _ placed]
                                              (if (seq placed)
                                                (conj (vec base) [20 20])
                                                base))
                :path-segments (fn [points] (map vector points (rest points)))})]
      (should= 2 (count out))
      (should= true (:anchored? (first out)))
      (should= [[20 0] [20 10] [20 20]] (:route-points (second out)))))

  (it "drops edges when no path can be produced"
    (let [out (sut/route-edges
               {:spaced-edges [{:id :e1 :from "a" :to "b"}]
                :resolve-edge-path (fn [_] {:points []})
                :normalize-route-endpoints (fn [_ _] [])
                :place-non-overlapping-path (fn [_ _ _] nil)
                :path-segments (fn [_] [])})]
      (should= [] out)))

  (it "falls back to base path when overlap resolver rejects it"
    (let [out (sut/route-edges
               {:spaced-edges [{:id :e1 :from "a" :to "b"}]
                :resolve-edge-path (fn [_] {:points [[0 0] [10 0]]})
                :normalize-route-endpoints (fn [points _] points)
                :place-non-overlapping-path (fn [_ _ _] nil)
                :path-segments (fn [points] (map vector points (rest points)))})]
      (should= 1 (count out))
      (should= [[0 0] [10 0]] (:route-points (first out))))))
