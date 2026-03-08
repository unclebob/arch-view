(ns arch-view.render.ui.quil.events-spec
  (:require [arch-view.render.ui.quil.events :as sut]
            [speclj.core :refer :all]))

(describe "quil events"
  (it "detects control modifier from direct flags, sets, numeric mask, and key state"
    (should= true (sut/control-down? {:control true}))
    (should= true (sut/control-down? {:ctrl true}))
    (should= true (sut/control-down? {:control-key? true}))
    (should= true (sut/control-down? {:modifiers #{:control}}))
    (should= true (sut/control-down? {:modifiers [:ctrl]}))
    (should= false (sut/control-down? {:modifiers :not-a-set}))
    (should= true (sut/control-down? {:modifiers 128}))
    (should= false (sut/control-down? {:modifiers 0}))
    (with-redefs [quil.core/key-pressed? (fn [] true)
                  quil.core/key-as-keyword (fn [] :control)]
      (should= true (sut/control-down? {})))
    (with-redefs [quil.core/key-pressed? (fn [] (throw (ex-info "no key" {})))]
      (should= false (sut/control-down? {}))))

  (it "computes zoom-in and zoom-out state at screen positions"
    (let [deps {:scaled-content-width (fn [_ z] (* 1000.0 z))
                :scaled-content-height (fn [_ z] (* 800.0 z))
                :clamp-scroll (fn [v _ _] (max 0.0 v))
                :clamp-scroll-x (fn [v _ _] (max 0.0 v))}
          state {:scene {}
                 :zoom 1.0
                 :zoom-stack []
                 :scroll-x 100.0
                 :scroll-y 50.0
                 :viewport-width 500.0
                 :viewport-height 400.0}
          zoomed (sut/zoom-in-at-screen-pos state 200.0 100.0 deps)
          unzoomed (sut/zoom-out-at-screen-pos zoomed 200.0 100.0 deps)]
      (should= 1 (count (:zoom-stack zoomed)))
      (should= 1.1 (:zoom zoomed))
      (should= 1.0 (:zoom unzoomed))
      (should= 0 (count (:zoom-stack unzoomed)))))

  (it "uses fallback values when zoom-out stack entry is partial"
    (let [deps {:scaled-content-width (fn [_ _] 1000.0)
                :scaled-content-height (fn [_ _] 800.0)
                :clamp-scroll (fn [v _ _] (max 0.0 v))
                :clamp-scroll-x (fn [v _ _] (max 0.0 v))}
          state {:scene {}
                 :zoom 1.2
                 :scroll-x 30.0
                 :scroll-y 40.0
                 :viewport-width 500.0
                 :viewport-height 400.0
                 :zoom-stack [{:zoom 1.0}]}
          out (#'sut/zoom-out-at-screen-pos* state 10.0 20.0 deps (:zoom-stack state))]
      (should= 1.0 (:zoom out))
      (should= 0 (count (:zoom-stack out)))))

  (it "handles key press branches for escape plus and minus"
    (let [deps {:scaled-content-width (fn [_ z] (* 1000.0 z))
                :scaled-content-height (fn [_ z] (* 800.0 z))
                :clamp-scroll (fn [v _ _] (max 0.0 v))
                :clamp-scroll-x (fn [v _ _] (max 0.0 v))}
          state {:scene {}
                 :zoom 1.0
                 :zoom-stack [{:zoom 0.9 :scroll-x 0.0 :scroll-y 0.0}]
                 :scroll-x 0.0
                 :scroll-y 0.0
                 :viewport-width 500.0
                 :viewport-height 400.0}
          exited (atom false)]
      (with-redefs [quil.core/exit (fn [] (reset! exited true))]
        (should= state (sut/handle-key-pressed state {:key :escape :x 0 :y 0} deps)))
      (should= true @exited)
      (should= 1.1 (:zoom (sut/handle-key-pressed state {:key :+ :x 100 :y 100} deps)))
      (should= 0.9 (:zoom (sut/handle-key-pressed state {:key :- :x 100 :y 100} deps)))))

  (it "resolves event mouse coordinates from event keys and Quil fallback"
    (should= 12.0 (#'sut/event-mouse-coord {:x 12} :x :mouse-x))
    (should= 13.0 (#'sut/event-mouse-coord {:mouse-x 13} :x :mouse-x))
    (with-redefs [quil.core/mouse-x (fn [] 14.0)]
      (should= 14.0 (#'sut/event-mouse-coord {} :x :mouse-x)))
    (with-redefs [quil.core/mouse-y (fn [] (throw (ex-info "no mouse" {})))]
      (should= 0.0 (#'sut/event-mouse-coord {} :y :mouse-y))))

  (it "handles zoom-click via control modifier and mouse button"
    (let [deps {:scaled-content-height (fn [_ z] (* 800.0 z))
                :clamp-scroll (fn [v _ _] (max 0.0 v))}
          state {:scene {}
                 :zoom 1.0
                 :zoom-stack [{:zoom 0.9 :center-world-y 100.0 :screen-y 80.0 :scroll-y 10.0}]
                 :scroll-y 20.0
                 :viewport-height 400.0}]
      (with-redefs [sut/control-down? (fn [_] true)]
        (should= 1.1 (:zoom (sut/apply-zoom-click state {:button :right :x 40.0 :y 80.0} deps)))
        (should= 0.9 (:zoom (sut/apply-zoom-click state {:button :left :x 40.0 :y 80.0} deps))))
      (with-redefs [sut/control-down? (fn [_] false)]
        (should= nil (sut/apply-zoom-click state {:button :right :x 40.0 :y 80.0} deps)))))

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
                :toolbar-height 38.0}]
      (should= :back (sut/toolbar-click-target state 1.0 1.0 deps))
      (should= nil (sut/toolbar-click-target state 2.0 2.0 deps))
      (should= {:v 1 :nav-stack []}
               (sut/navigate-up {:v 1 :nav-stack [{:path [] :scroll-x 0.0 :scroll-y 0.0}]}
                                {:drilldown-scene (fn [s _ _ _] (assoc s :nav-stack []))}))))

  (it "returns state unchanged when nav stack is empty"
    (let [state {:namespace-path []}
          deps {:point-in-rect? (fn [rect x y] (and (= (:id rect) 1) (= x 2.0) (= y 2.0)))
                :back-button-rect (fn [] {:id 0})
                :toolbar-height 38.0}]
      (should= nil (sut/toolbar-click-target state 2.0 2.0 deps))
      (should= {:v 2}
               (sut/navigate-up {:v 2}
                                {:drilldown-scene (fn [s _ _ _] (assoc s :unexpected true))}))))

  (it "applies toolbar click actions"
    (let [state {:namespace-path ["a"] :declutter-mode :all :nav-stack []}
          deps {:point-in-rect? (fn [rect x y] (and (= (:hit rect) [x y]) true))
                :back-button-rect (fn [] {:hit [1.0 1.0]})
                :drilldown-scene (fn [s _ _ _] (assoc s :went-back true))
                :toolbar-height 38.0}
          back (sut/apply-toolbar-click (assoc state :nav-stack [{:path [] :scroll-x 0.0 :scroll-y 0.0}]) {:x 1.0 :y 1.0} deps)
          declutter (sut/apply-toolbar-click state {:x 2.0 :y 2.0} deps)
          none (sut/apply-toolbar-click state {:x 2.0 :y 80.0} deps)]
      (should= true (:went-back back))
      (should= state declutter)
      (should= nil none)))

  (it "handles mouse release for dragging reset, toolbar click, and drilldown fallback"
    (let [deps {:point-in-rect? (fn [_ _ _] false)
                :back-button-rect (fn [] {:x 0})
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
                                                        :point-in-rect? (fn [rect x y] false)))
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
      (should= false (:dragging-scrollbar? toolbar-hit))
      (should= true (:drilled drilldown-hit))))

  (it "handles mouse clicked branch for drag guard, toolbar, and drilldown"
    (let [base-state {:scene {}
                      :namespace-path []
                      :architecture {:graph {:nodes #{}}
                                     :classified-edges #{}}
                      :zoom 1.0}
          deps {:point-in-rect? (fn [& _] false)
                :back-button-rect (fn [] {:x 0})
                :drilldown-scene (fn [s _ _ _] (assoc s :drilled true))
                :toolbar-height 38.0
                :hovered-module-position (fn [& _] nil)
                :view-architecture (fn [& _] {:graph {:nodes #{}}})
                :push-nav-state identity
                :open-source-file-window! (fn [& _])}]
      (should= {:dragging-scrollbar? true}
               (sut/handle-mouse-clicked {:dragging-scrollbar? true} {:x 0 :y 0} deps))
      (should= true
               (:drilled
                (sut/handle-mouse-clicked base-state
                                          {:x 30.0 :y 80.0}
                                          (assoc deps
                                                 :hovered-module-position (fn [& _] {:module "a"})
                                                 :view-architecture (fn [& _] {:graph {:nodes #{"child"}}})))))
      (should= base-state
               (sut/handle-mouse-clicked base-state {:x 4.0 :y 90.0} deps))))

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
