(ns arch-view.render.ui.util.scene-state-spec
  (:require [arch-view.render.ui.util.scene-state :as sut]
            [speclj.core :refer :all]))

(describe "scene state"
  (it "marks modules drillable when child view has nodes"
    (let [scene {:module-positions [{:module "a" :label "A"}
                                    {:module "b" :label "B"}]}
          marked (sut/attach-drillable-markers scene
                                               {:any :architecture}
                                               []
                                               (fn [_ path]
                                                 (if (= path ["a"])
                                                   {:graph {:nodes #{"x"}}}
                                                   {:graph {:nodes #{}}})))]
      (should= true (get-in marked [:module-positions 0 :drillable?]))
      (should= false (get-in marked [:module-positions 1 :drillable?]))
      (should= "A" (get-in marked [:module-positions 0 :display-label]))))

  (it "updates scene and scroll when drilling down"
    (let [state {:architecture {:graph :x}}
          next-state (sut/drilldown-scene state ["game"] nil 12.5
                                          {:view-architecture (fn [_ _] {:graph :view})
                                           :build-scene (fn [_] {:module-positions []})
                                           :attach-drillable-markers (fn [scene _ _] scene)})]
      (should= ["game"] (:namespace-path next-state))
      (should= 0.0 (:scroll-x next-state))
      (should= 12.5 (:scroll-y next-state))
      (should= {:module-positions []} (:scene next-state))))

  (it "pushes current namespace and scroll values onto nav stack"
    (let [state {:namespace-path ["a" "b"]
                 :scene {:module-positions [{:module "b"}]}
                 :scroll-x 11
                 :scroll-y 22
                 :zoom 1.4
                 :zoom-stack [{:zoom 1.2}]
                 :nav-stack []}
          next-state (sut/push-nav-state state)]
      (should= [{:path ["a" "b"]
                 :scene {:module-positions [{:module "b"}]}
                 :scroll-x 11.0
                 :scroll-y 22.0
                 :zoom 1.4
                 :zoom-stack [{:zoom 1.2}]}]
               (:nav-stack next-state))))

  (it "builds initial scene from architecture root when architecture is provided"
    (let [scene {:module-positions [{:module "x"}]}
          built (sut/initial-scene-for-show scene
                                            {:graph :yes}
                                            {:view-architecture (fn [_ _] {:view :root})
                                             :build-scene (fn [_] {:module-positions [{:module "root"}]})
                                             :attach-drillable-markers (fn [sc _ _] (assoc sc :tag :attached))})]
      (should= :attached (:tag built))
      (should= [{:module "root"}] (:module-positions built))))

  (it "leaves scene unchanged when architecture is missing"
    (let [scene {:module-positions [{:module "x"}]}
          unchanged (sut/attach-drillable-markers scene nil [] (fn [& _] nil))]
      (should= scene unchanged))))
