(ns arch-view.render.ui.quil.events-spec
  (:require [arch-view.render.ui.quil.events :as sut]
            [speclj.core :refer :all]))

(describe "quil events"
  (it "handles mouse wheel events from number and map payloads"
    (let [deps {:scaled-content-height (fn [_ _] 1000.0)
                :clamp-scroll (fn [v _ _] v)}
          state {:scene {} :scroll-y 10.0 :viewport-height 400 :viewport-width 700 :zoom 1.0}
          numeric (sut/handle-mouse-wheel state 2 deps)
          mapped (sut/handle-mouse-wheel state {:count -1} deps)
          unknown (sut/handle-mouse-wheel state :none deps)]
      (should= 70.0 (:scroll-y numeric))
      (should= -20.0 (:scroll-y mapped))
      (should= 10.0 (:scroll-y unknown))))

  (it "starts and updates scrollbar dragging when thumb is hit"
    (let [deps-press {:scaled-content-height (fn [_ _] 1000.0)
                      :scrollbar-rect (fn [& _] {:x 10.0 :y 50.0 :width 8.0 :height 100.0})
                      :point-in-rect? (fn [_ x y] (and (<= 10.0 x 18.0) (<= 50.0 y 150.0)))}
          deps-drag {:scaled-content-height (fn [_ _] 1000.0)
                     :thumb-y->scroll (fn [thumb-y _ _] thumb-y)}
          base {:scene {} :scroll-y 0.0 :viewport-height 400 :viewport-width 700 :zoom 1.0}
          pressed (sut/handle-mouse-pressed base {:x 12.0 :y 60.0} deps-press)
          dragged (sut/handle-mouse-dragged (assoc pressed :dragging-scrollbar? true) {:x 12.0 :y 100.0} deps-drag)]
      (should= true (:dragging-scrollbar? pressed))
      (should= 10.0 (:drag-offset pressed))
      (should= 90.0 (:scroll-y dragged))))

  (it "returns state unchanged when not dragging"
    (let [state {:scene {} :dragging-scrollbar? false :viewport-height 400 :zoom 1.0}
          out (sut/handle-mouse-dragged state {:x 0 :y 0}
                                        {:scaled-content-height (fn [_ _] 1000.0)
                                         :thumb-y->scroll (fn [v _ _] v)})]
      (should= state out)))

  (it "maps button kinds from keyword, numeric, and fallback mouse-button"
    (with-redefs [quil.core/mouse-button (fn [] 2)]
      (should= :right (sut/button-kind {:button :right}))
      (should= :left (sut/button-kind {:button 1}))
      (should= :center (sut/button-kind {}))
      (should= nil (sut/button-kind {:button 99}))))

  (it "maps button kind from alternate event keys and handles fallback errors"
    (with-redefs [quil.core/mouse-button (fn [] (throw (ex-info "no mouse button" {})))]
      (should= :right (sut/button-kind {:mouse-button 3}))
      (should= :left (sut/button-kind {:which 1}))
      (should= nil (sut/button-kind {}))))

  (it "resolves toolbar target and navigates up"
    (let [state {:namespace-path ["a"]}
          deps {:point-in-rect? (fn [rect x y] (and (= 0 (:id rect)) (= x 1.0) (= y 1.0)))
                :back-button-rect (fn [] {:id 0})
                :declutter-button-rect (fn [] {:id 1})
                :toolbar-height 38.0}]
      (should= :back (sut/toolbar-click-target state 1.0 1.0 deps))
      (should= nil (sut/toolbar-click-target state 2.0 2.0 deps))
      (should= {:v 1 :nav-stack []}
               (sut/navigate-up {:v 1 :nav-stack [{:path [] :scroll-x 0.0 :scroll-y 0.0}]}
                                {:drilldown-scene (fn [s _ _ _] (assoc s :nav-stack []))}))))

  (it "targets declutter button and returns state unchanged when nav stack is empty"
    (let [state {:namespace-path []}
          deps {:point-in-rect? (fn [rect x y] (and (= (:id rect) 1) (= x 2.0) (= y 2.0)))
                :back-button-rect (fn [] {:id 0})
                :declutter-button-rect (fn [] {:id 1})
                :toolbar-height 38.0}]
      (should= :declutter (sut/toolbar-click-target state 2.0 2.0 deps))
      (should= {:v 2}
               (sut/navigate-up {:v 2}
                                {:drilldown-scene (fn [s _ _ _] (assoc s :unexpected true))}))))

  (it "applies toolbar click actions"
    (let [state {:namespace-path ["a"] :declutter-mode :all :nav-stack []}
          deps {:point-in-rect? (fn [rect x y] (and (= (:hit rect) [x y]) true))
                :back-button-rect (fn [] {:hit [1.0 1.0]})
                :declutter-button-rect (fn [] {:hit [2.0 2.0]})
                :next-declutter-mode (fn [_] :abstract)
                :drilldown-scene (fn [s _ _ _] (assoc s :went-back true))
                :toolbar-height 38.0}
          back (sut/apply-toolbar-click (assoc state :nav-stack [{:path [] :scroll-x 0.0 :scroll-y 0.0}]) {:x 1.0 :y 1.0} deps)
          declutter (sut/apply-toolbar-click state {:x 2.0 :y 2.0} deps)
          none (sut/apply-toolbar-click state {:x 2.0 :y 80.0} deps)]
      (should= true (:went-back back))
      (should= :abstract (:declutter-mode declutter))
      (should= nil none)))

  (it "handles mouse release for dragging reset, toolbar click, and drilldown fallback"
    (let [deps {:point-in-rect? (fn [_ _ _] false)
                :back-button-rect (fn [] {:x 0})
                :declutter-button-rect (fn [] {:x 0})
                :next-declutter-mode (fn [m] m)
                :drilldown-scene (fn [s _ _ _] s)
                :toolbar-height 38.0
                :hovered-module-position (fn [& _] nil)
                :view-architecture (fn [& _] nil)
                :push-nav-state identity
                :open-source-file-window! (fn [& _])}
          dragged (sut/handle-mouse-released {:dragging-scrollbar? true :drag-offset 10.0} {:x 0 :y 0} deps)
          toolbar-hit (sut/handle-mouse-released {:dragging-scrollbar? false :drag-offset 10.0}
                                                 {:x 5.0 :y 5.0}
                                                 (assoc deps
                                                        :point-in-rect? (fn [rect x y] false)
                                                        :declutter-button-rect (fn [] {:hit [5.0 5.0]})
                                                        :point-in-rect? (fn [rect x y] (= [x y] (:hit rect)))
                                                        :next-declutter-mode (fn [_] :abstract)))
          drilldown-hit (sut/handle-mouse-released {:dragging-scrollbar? false
                                                    :scene {}
                                                    :namespace-path []
                                                    :architecture {:graph {:nodes #{}}
                                                                   :classified-edges #{}}
                                                    :zoom 1.0}
                                                   {:x 15.0 :y 80.0}
                                                   (assoc deps
                                                          :point-in-rect? (fn [& _] false)
                                                          :hovered-module-position (fn [& _] {:module "alpha"})
                                                          :view-architecture (fn [& _] {:graph {:nodes #{"child"}}})
                                                          :push-nav-state (fn [s] (assoc s :pushed true))
                                                          :drilldown-scene (fn [s _ _ _] (assoc s :drilled true))))]
      (should= false (:dragging-scrollbar? dragged))
      (should= nil (:drag-offset dragged))
      (should= :abstract (:declutter-mode toolbar-hit))
      (should= true (:drilled drilldown-hit))))

  (it "keeps state when drilldown click hits nothing and opens source for leaf click"
    (let [opened (atom nil)
          base-state {:scene {:module-positions []}
                      :namespace-path []
                      :architecture {:graph {:nodes #{}}
                                     :classified-edges #{}}
                      :zoom 1.0}
          deps {:hovered-module-position (fn [& _] nil)
                :view-architecture (fn [& _] {:graph {:nodes #{}}})
                :push-nav-state identity
                :drilldown-scene (fn [s _ _ _] (assoc s :drilled true))
                :open-source-file-window! (fn [path] (reset! opened path))}
          no-hover (sut/apply-drilldown-click base-state {:x 1.0 :y 80.0} deps)
          leaf-state (assoc base-state :scene {:module-positions [{:module "m" :source-file "/tmp/m.clj"}]})
          leaf-click (sut/apply-drilldown-click
                      leaf-state
                      {:x 1.0 :y 80.0}
                      (assoc deps
                             :hovered-module-position (fn [& _] {:module "m" :source-file "/tmp/m.clj"})))]
      (should= base-state no-hover)
      (should= leaf-state leaf-click)
      (should= "/tmp/m.clj" @opened))))
