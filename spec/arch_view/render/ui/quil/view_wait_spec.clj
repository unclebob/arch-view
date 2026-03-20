(ns arch-view.render.ui.quil.view-wait-spec
  (:require [arch-view.render.ui.quil.view :as sut]
            [arch-view.render.ui.util.quil-lifecycle :as lifecycle]
            [speclj.core :refer :all]))

(describe "quil view helper coverage"
  (it "delegates wait-until-closed to lifecycle utility"
    (let [called? (atom nil)]
      (with-redefs [lifecycle/wait-until-closed! (fn [sketch]
                                                   (reset! called? sketch))]
        (sut/wait-until-closed! :s))
      (should= :s @called?)))

  (it "builds sketch options in show"
    (let [captured (atom nil)
          scene {:layer-rects [{:x 0.0 :y 0.0 :width 300.0 :height 120.0}]
                 :module-positions []
                 :edge-drawables []}]
      (with-redefs [quil.core/sketch (fn [& args] (reset! captured (apply hash-map args)) :ok)]
        (should= :ok (sut/show! scene {:title "T"})))
      (should= "T" (:title @captured))
      (should= true (contains? @captured :mouse-released))
      (should= true (contains? @captured :update))
      (should= [:resizable] (:features @captured)))))

  (it "reanalyzes the current namespace when reload is available"
    (with-redefs [sut/view-architecture (fn [architecture path] {:graph {:nodes (if (= path ["a"]) #{"child"} #{"root"})}})
                  sut/build-scene-for-path (fn [architecture path width] {:path path :width width})]
      (let [next-state (sut/reanalyze-state {:reload-architecture (fn [] {:graph :fresh})
                                             :namespace-path ["a"]
                                             :reanalyze-requested? true
                                             :reanalyze-status :running
                                             :reanalyze-started-at 1234
                                             :viewport-width 1400.0
                                             :nav-stack [{:path []}]
                                             :zoom 1.5
                                             :zoom-stack [{}]
                                             :dependency-tooltip-key :k
                                             :dependency-tooltip-scroll 10.0})]
        (should= {:graph :fresh} (:architecture next-state))
        (should= ["a"] (:namespace-path next-state))
        (should= {:path ["a"] :width 1400.0} (:scene next-state))
        (should= false (:reanalyze-requested? next-state))
        (should= nil (:reanalyze-status next-state))
        (should= nil (:reanalyze-started-at next-state))
        (should= [] (:nav-stack next-state))
        (should= 1.0 (:zoom next-state))
        (should= 0.0 (:dependency-tooltip-scroll next-state)))))

  (it "marks reanalysis as running before the next update cycle"
    (with-redefs [sut/current-time-ms (fn [] 4242)]
      (should= {:reload-architecture :reload
                :reanalyze-requested? true
                :reanalyze-status :running
                :reanalyze-started-at 4242}
               (select-keys (sut/start-reanalyze-state {:reload-architecture :reload})
                            [:reload-architecture :reanalyze-requested? :reanalyze-status :reanalyze-started-at]))))

  (it "does not restart reanalysis while it is already running"
    (should= {:reload-architecture :reload
              :reanalyze-requested? true
              :reanalyze-status :running
              :reanalyze-started-at 4242}
             (select-keys (sut/start-reanalyze-state {:reload-architecture :reload
                                                      :reanalyze-requested? true
                                                      :reanalyze-status :running
                                                      :reanalyze-started-at 4242})
                          [:reload-architecture :reanalyze-requested? :reanalyze-status :reanalyze-started-at])))

  (it "keeps showing analyzing before the one-second minimum elapses"
    (with-redefs [quil.core/width (fn [] 1200.0)
                  quil.core/height (fn [] 700.0)
                  sut/current-time-ms (fn [] 1500)]
      (let [state (sut/update-state {:architecture {:graph :yes}
                                     :reload-architecture (fn [] {:graph :fresh})
                                     :namespace-path []
                                     :scene {:layer-rects [] :module-positions [] :edge-drawables []}
                                     :viewport-width 1200.0
                                     :viewport-height 700.0
                                     :scroll-x 0.0
                                     :scroll-y 0.0
                                     :zoom 1.0
                                     :reanalyze-requested? true
                                     :reanalyze-status :running
                                     :reanalyze-started-at 700
                                     :dependency-tooltip-key nil
                                     :dependency-tooltip-scroll 0.0})]
        (should= true (:reanalyze-requested? state))
        (should= :running (:reanalyze-status state))
        (should= 700 (:reanalyze-started-at state)))))

  (it "reanalyzes after the one-second minimum elapses"
    (with-redefs [quil.core/width (fn [] 1200.0)
                  quil.core/height (fn [] 700.0)
                  sut/current-time-ms (fn [] 2000)
                  sut/view-architecture (fn [architecture path] {:graph {:nodes (if (= path ["a"]) #{"child"} #{"root"})}})
                  sut/build-scene-for-path (fn [architecture path width] {:path path :width width})]
      (let [state (sut/update-state {:architecture {:graph :stale}
                                     :reload-architecture (fn [] {:graph :fresh})
                                     :namespace-path ["a"]
                                     :scene {:layer-rects [] :module-positions [] :edge-drawables []}
                                     :viewport-width 1400.0
                                     :viewport-height 700.0
                                     :scroll-x 0.0
                                     :scroll-y 0.0
                                     :zoom 1.0
                                     :reanalyze-requested? true
                                     :reanalyze-status :running
                                     :reanalyze-started-at 900
                                     :dependency-tooltip-key nil
                                     :dependency-tooltip-scroll 0.0})]
        (should= {:graph :fresh} (:architecture state))
        (should= ["a"] (:namespace-path state))
        (should= {:path ["a"] :width 1400.0} (:scene state))
        (should= false (:reanalyze-requested? state))
        (should= nil (:reanalyze-status state))
        (should= nil (:reanalyze-started-at state)))))

  (it "rebuilds the current scene when the viewport width changes"
    (let [built (atom nil)]
      (with-redefs [quil.core/width (fn [] 1600.0)
                    quil.core/height (fn [] 900.0)
                    sut/view-architecture (fn [_ path] {:path path})
                    sut/build-scene (fn [view opts]
                                      (reset! built {:view view :opts opts})
                                      {:module-positions []})
                    sut/attach-drillable-markers (fn [scene _ _] scene)]
        (let [state (sut/update-state {:architecture {:graph :yes}
                                       :namespace-path ["alpha"]
                                       :scene {:layer-rects [] :module-positions [] :edge-drawables []}
                                       :viewport-width 1200.0
                                       :viewport-height 700.0
                                       :scroll-x 0.0
                                       :scroll-y 0.0
                                       :zoom 1.0
                                       :dependency-tooltip-key nil
                                       :dependency-tooltip-scroll 0.0})]
          (should= 1600.0 (:viewport-width state))
          (should= {:path ["alpha"]} (:view @built))
          (should= {:canvas-width 1600.0} (:opts @built))))))
