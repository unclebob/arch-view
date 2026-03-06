(ns arch-view.render.quil-view-spec
  (:require [clojure.string :as str]
            [arch-view.render.quil-view :as sut]
            [speclj.core :refer :all]))

(describe "quil scene model"
  (it "builds racetrack-sized layer rectangles with horizontal module positions"
    (let [architecture {:layout {:layers [{:index 0 :modules ["d"]}
                                          {:index 1 :modules ["b"]}
                                          {:index 2 :modules ["a" "c"]}]
                                 :module->layer {"a" 2 "b" 1 "c" 2 "d" 0}}
                        :classified-edges #{{:from "a" :to "b" :type :direct}
                                            {:from "c" :to "d" :type :abstract}}}
          scene (sut/build-scene architecture {:canvas-width 1000 :layer-height 120 :layer-gap 30})]
      (should= [171.2 171.2 171.2]
               (mapv :width (:layer-rects scene)))
      (should= true (every? #(<= 24.0 (:x %)) (:layer-rects scene)))
      (should= true (every? #(>= (:y %) 42.0) (:layer-rects scene)))
      (should= ["a" "c"]
               (->> (:module-positions scene)
                    (filter #(= 2 (:layer %)))
                    (sort-by :x)
                    (mapv :module)))
      (should= ["a" "c"]
               (->> (:module-positions scene)
                    (filter #(= 2 (:layer %)))
                    (sort-by :x)
                    (mapv :label)))
      (should= ["layer-0" "layer-1" "layer-2"]
               (mapv :label (:layer-rects scene)))
      (should= #{{:from "a" :to "b" :arrowhead :standard}
                 {:from "c" :to "d" :arrowhead :closed-triangle}}
               (set (map #(select-keys % [:from :to :arrowhead])
                         (:edge-drawables scene))))))

  (it "marks layers abstract when they contain abstract modules"
    (let [architecture {:layout {:layers [{:index 0 :modules ["alpha"]}
                                          {:index 1 :modules ["beta"]}]
                                 :module->layer {"alpha" 0 "beta" 1}}
                        :module->kind {"alpha" :concrete
                                       "beta" :abstract}
                        :classified-edges #{}}
          scene (sut/build-scene architecture {:canvas-width 1000 :layer-height 120 :layer-gap 30})
          by-index (into {} (map (juxt :index identity) (:layer-rects scene)))]
      (should= false (:abstract? (get by-index 0)))
      (should= true (:abstract? (get by-index 1)))))

  (it "staggers module label y positions when horizontal space is tight"
    (let [architecture {:layout {:layers [{:index 0 :modules ["alpha.beta.long-module-one"
                                                               "alpha.beta.long-module-two"
                                                               "alpha.beta.long-module-three"]}]
                                 :module->layer {"alpha.beta.long-module-one" 0
                                                 "alpha.beta.long-module-two" 0
                                                 "alpha.beta.long-module-three" 0}}
                        :classified-edges #{}}
          scene (sut/build-scene architecture {:canvas-width 260 :layer-height 120 :layer-gap 30})
          ys (->> (:module-positions scene) (map :y) distinct sort vec)]
      (should= [102.0 112.0] ys)))

  (it "keeps module labels aligned when there is enough horizontal space"
    (let [architecture {:layout {:layers [{:index 0 :modules ["alpha.beta.one"
                                                               "alpha.beta.two"
                                                               "alpha.beta.three"]}]
                                 :module->layer {"alpha.beta.one" 0
                                                 "alpha.beta.two" 0
                                                 "alpha.beta.three" 0}}
                        :classified-edges #{}}
          scene (sut/build-scene architecture {:canvas-width 1400 :layer-height 120 :layer-gap 30})
          ys (->> (:module-positions scene) (map :y) distinct sort vec)]
      (should= [102.0] ys)))

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

  (it "abbreviates module names using parent initials"
    (should= "b.module-name" (sut/abbreviate-module-name "alpha.beta.module-name"))
    (should= "module-name" (sut/abbreviate-module-name "module-name"))
    (should= "b.c.core" (sut/abbreviate-module-name "app.backend.cache.core")))

  (it "strips top-level namespace for hover names"
    (should= "beta.module" (sut/strip-top-namespace "alpha.beta.module"))
    (should= "module" (sut/strip-top-namespace "module")))

  (it "formats edge hover labels with dependency count"
    (should= "a->b(3)" (sut/edge-hover-label {:from "a" :to "b" :count 3}))
    (should= "a->b(1)" (sut/edge-hover-label {:from "a" :to "b"})))

  (it "labels back button with destination namespace"
    (should= "Back" (#'sut/back-button-label {:namespace-path [] :nav-stack []}))
    (should= "Back: root" (#'sut/back-button-label {:namespace-path ["application"]
                                                    :nav-stack [{:path []}]}))
    (should= "Back: application" (#'sut/back-button-label {:namespace-path ["application" "state"]
                                                           :nav-stack [{:path ["application"]}]})))

  (it "counts crossings between edges based on layer track/row points"
    (let [placement {0 {:track 0 :row 0}
                     1 {:track 1 :row 0}
                     2 {:track 1 :row 1}
                     3 {:track 0 :row 1}}
          crossing [[0 2] [1 3]]
          non-crossing [[0 3] [1 2]]]
      (should= 1 (#'sut/edge-crossing-count placement crossing))
      (should= 0 (#'sut/edge-crossing-count placement non-crossing))))

  (it "avoids upward placement for simple dependencies"
    (let [slots (#'sut/assign-layer-slots [0 1] [[0 1]] {0 0 1 1})]
      (should= true (<= (get-in slots [0 :row])
                        (get-in slots [1 :row])))))

  (it "prefers center track for high fan-in layers"
    (let [slots (#'sut/assign-layer-slots [0] [] {0 10})]
      (should= 2 (get-in slots [0 :track]))))

  (it "finds hovered module by label hitbox"
    (let [modules [{:module "alpha.beta.core"
                    :label "a.b.core"
                    :x 100.0
                    :y 50.0}]]
      (should= "alpha.beta.core" (sut/hovered-module modules 100.0 50.0))
      (should= nil (sut/hovered-module modules 300.0 300.0))))

  (it "finds hovered layer label by label hitbox"
    (let [layers [{:index 0
                   :x 0.0
                   :y 0.0
                   :width 1000.0
                   :height 120.0
                   :label "alpha.beta"
                   :full-name "alpha.beta"}]]
      (should-not= nil (sut/hovered-layer-label layers 12.0 10.0))
      (should= nil (sut/hovered-layer-label layers 600.0 80.0))))

  (it "finds hovered arrow and prefers nearest segment"
    (let [points {"a" {:x 100.0 :y 100.0}
                  "b" {:x 300.0 :y 100.0}
                  "c" {:x 100.0 :y 130.0}
                  "d" {:x 300.0 :y 130.0}}
          bounds {:min-x 0.0 :max-x 500.0 :min-y 0.0 :max-y 500.0}
          edges [{:from "a" :to "b" :count 2 :arrowhead :standard}
                 {:from "c" :to "d" :count 5 :arrowhead :standard}]
          hovered (sut/hovered-edge edges points bounds 170.0 102.0)]
      (should= "a" (:from hovered))
      (should= "b" (:to hovered))
      (should= 2 (:count hovered))))

  (it "respects caller-provided hover tolerance"
    (let [points {"a" {:x 100.0 :y 100.0}
                  "b" {:x 300.0 :y 100.0}}
          bounds {:min-x 0.0 :max-x 500.0 :min-y 0.0 :max-y 500.0}
          edges [{:from "a" :to "b" :count 1 :arrowhead :standard}]
          miss (sut/hovered-edge edges points bounds 170.0 109.0 8.0)
          hit (sut/hovered-edge edges points bounds 170.0 109.0 10.0)]
      (should= nil miss)
      (should-not= nil hit)))

  (it "uses tighter tolerance for diagonal edges"
    (let [points {"a" {:x 100.0 :y 100.0}
                  "b" {:x 300.0 :y 300.0}
                  "c" {:x 100.0 :y 100.0}
                  "d" {:x 300.0 :y 100.0}}
          bounds {:min-x 0.0 :max-x 500.0 :min-y 0.0 :max-y 500.0}
          diagonal [{:from "a" :to "b" :count 1 :arrowhead :standard}]
          horizontal [{:from "c" :to "d" :count 1 :arrowhead :standard}]
          diag-hit (sut/hovered-edge diagonal points bounds 170.0 177.0 8.0)
          horiz-hit (sut/hovered-edge horizontal points bounds 170.0 106.0 8.0)]
      (should= nil diag-hit)
      (should-not= nil horiz-hit)))

  (it "separates opposite-direction horizontal arrows"
    (let [points {"a" {:x 100.0 :y 100.0}
                  "b" {:x 300.0 :y 100.0}}
          edges [{:from "a" :to "b" :type :direct :arrowhead :standard}
                 {:from "b" :to "a" :type :direct :arrowhead :standard}]
          spaced (sut/apply-parallel-arrow-spacing edges points)
          ys (->> spaced (map :parallel-offset-y) sort vec)]
      (should= [-7.5 7.5] ys)))

  (it "computes and clamps vertical scroll range"
    (should= 600.0 (sut/scroll-range 1200 600))
    (should= 0.0 (sut/scroll-range 300 600))
    (should= 0.0 (sut/clamp-scroll -20 1200 600))
    (should= 300.0 (sut/clamp-scroll 300 1200 600))
    (should= 600.0 (sut/clamp-scroll 900 1200 600)))

  (it "builds scrollbar thumb when content exceeds viewport"
    (let [bar (sut/scrollbar-rect 2000 500 300 1200)]
      (should-not= nil bar)
      (should= 1188.0 (:x bar))
      (should= 8.0 (:width bar))
      (should= true (> (:height bar) 0.0))))

  (it "omits scrollbar when content fits viewport"
    (should= nil (sut/scrollbar-rect 400 500 0 1200)))

  (it "maps scrollbar thumb y back to scroll offset"
    (should= 0.0 (sut/thumb-y->scroll 12.0 2000 500))
    (should= true (> (sut/thumb-y->scroll 300.0 2000 500) 0.0))
    (should= 1500.0 (sut/thumb-y->scroll 10000.0 2000 500)))

  (it "zooms in on + key and restores prior zoom on - key"
    (let [state {:scene {:layer-rects [{:x 0.0 :y 0.0 :width 2000.0 :height 1000.0}]}
                 :zoom 1.0
                 :zoom-stack []
                 :scroll-x 40.0
                 :scroll-y 120.0
                 :viewport-height 600
                 :viewport-width 1200
                 :dragging-scrollbar? false}
          zoomed (sut/handle-key-pressed state {:key \+ :x 300.0 :y 200.0})
          restored (sut/handle-key-pressed zoomed {:key \- :x 300.0 :y 200.0})]
      (should= 1.1 (:zoom zoomed))
      (should= 1 (count (:zoom-stack zoomed)))
      (should= 1.0 (:zoom restored))
      (should= 40.0 (:scroll-x restored))
      (should= 0 (count (:zoom-stack restored)))))

  (it "supports keyword + and - keys for zoom controls"
    (let [state {:scene {:layer-rects [{:x 0.0 :y 0.0 :width 100.0 :height 1000.0}]}
                 :zoom 1.0
                 :zoom-stack []
                 :scroll-x 0.0
                 :scroll-y 0.0
                 :viewport-height 600
                 :viewport-width 1200
                 :dragging-scrollbar? false}
          zoomed (sut/handle-key-pressed state {:key :+ :x 20.0 :y 80.0})
          restored (sut/handle-key-pressed zoomed {:key :- :x 20.0 :y 80.0})]
      (should= 1.1 (:zoom zoomed))
      (should= 1.0 (:zoom restored))))

  (it "stores zoom center on stack and restores it on - key"
    (let [state {:scene {:layer-rects [{:x 0.0 :y 0.0 :width 2400.0 :height 1200.0}]
                         :module-positions []
                         :edge-drawables []}
                 :architecture nil
                 :namespace-path []
                 :zoom 1.0
                 :zoom-stack []
                 :scroll-x 200.0
                 :scroll-y 150.0
                 :viewport-height 700
                 :viewport-width 1200
                 :dragging-scrollbar? false}
          zoomed (sut/handle-key-pressed state {:key \+ :x 250.0 :y 300.0})
          stack-top (peek (:zoom-stack zoomed))
          restored (sut/handle-key-pressed zoomed {:key \- :x 250.0 :y 300.0})]
      (should= true (contains? stack-top :center-world-x))
      (should= true (contains? stack-top :center-world-y))
      (should= true (contains? stack-top :screen-x))
      (should= true (contains? stack-top :screen-y))
      (should= 1.0 (:zoom restored))
      (should= 200.0 (:scroll-x restored))
      (should= 150.0 (:scroll-y restored))))

  (it "exits sketch when escape is pressed"
    (let [exited? (atom false)]
      (with-redefs [quil.core/exit (fn [] (reset! exited? true))]
        (sut/handle-key-pressed {} {:key :escape}))
      (should= true @exited?)))

  (it "ignores non-escape key presses"
    (let [exited? (atom false)]
      (with-redefs [quil.core/exit (fn [] (reset! exited? true))]
        (sut/handle-key-pressed {} {:key :a}))
      (should= false @exited?)))

  (it "drills down when a clickable namespace label is clicked"
    (let [architecture {:graph {:nodes #{"empire.alpha.one"
                                         "empire.alpha.two"
                                         "empire.beta.one"}
                                :edges #{}
                                :abstract-modules #{}}
                        :classified-edges #{}}
          root-view (sut/view-architecture architecture [])
          root-scene (sut/build-scene root-view)
          alpha-pos (some #(when (= "alpha" (:module %)) %) (:module-positions root-scene))
          state {:scene root-scene
                 :architecture architecture
                 :namespace-path []
                 :scroll-y 0.0
                 :dragging-scrollbar? false
                 :drag-offset nil
                 :viewport-height 600
                 :viewport-width 1200}
          next-state (sut/handle-mouse-clicked state {:x (:x alpha-pos) :y (:y alpha-pos)})]
      (should= ["alpha"] (:namespace-path next-state))
      (should= #{"one" "two"}
               (->> (get-in next-state [:scene :module-positions])
                    (map :module)
                    set))))

  (it "keeps state unchanged when clicked namespace has no deeper children"
    (let [architecture {:graph {:nodes #{"empire.alpha.one"}
                                :edges #{}
                                :abstract-modules #{}}
                        :classified-edges #{}}
          alpha-view (sut/view-architecture architecture ["alpha"])
          alpha-scene (sut/build-scene alpha-view)
          one-pos (some #(when (= "one" (:module %)) %) (:module-positions alpha-scene))
          state {:scene alpha-scene
                 :architecture architecture
                 :namespace-path ["alpha"]
                 :scroll-y 0.0
                 :dragging-scrollbar? false
                 :drag-offset nil
                 :viewport-height 600
                 :viewport-width 1200}
          next-state (sut/handle-mouse-clicked state {:x (:x one-pos) :y (:y one-pos)})]
      (should= state next-state)))

  (it "marks drillable namespace labels with a plus prefix"
    (let [architecture {:graph {:nodes #{"empire.alpha.one"
                                         "empire.alpha.two"
                                         "empire.beta"}
                                :edges #{}
                                :abstract-modules #{}}
                        :classified-edges #{}}
          root-view (sut/view-architecture architecture [])
          root-scene (sut/build-scene root-view)
          marked (sut/attach-drillable-markers root-scene architecture [])
          labels (into {} (map (juxt :module :display-label) (:module-positions marked)))]
      (should= "+ alpha" (get labels "alpha"))
      (should= "beta" (get labels "beta"))))

  (it "builds grouped initial scene for show when architecture is provided"
    (let [architecture {:graph {:nodes #{"empire.alpha.one"
                                         "empire.alpha.two"
                                         "empire.beta.one"}
                                :edges #{}
                                :abstract-modules #{}}
                        :classified-edges #{}}
          module-scene {:layer-rects [{:index 0 :x 0 :y 0 :width 1000 :height 100 :label "modules"}]
                        :module-positions [{:module "empire.alpha.one" :x 200 :y 50 :label "alpha.one"}
                                           {:module "empire.beta.one" :x 600 :y 50 :label "beta.one"}]
                        :edge-drawables []}
          initial (sut/initial-scene-for-show module-scene architecture)
          modules (->> (:module-positions initial) (map :module) set)
          labels (into {} (map (juxt :module :display-label) (:module-positions initial)))]
      (should= #{"alpha" "beta"} modules)
      (should= "+ alpha" (get labels "alpha"))
      (should= "+ beta" (get labels "beta"))))

  (it "orders grouped layers by incoming dependency count"
    (let [architecture {:graph {:nodes #{"empire.ui"
                                         "empire.acceptance"
                                         "empire.application"}
                                :edges #{{:from "empire.ui" :to "empire.acceptance"}}
                                :abstract-modules #{}}
                        :classified-edges #{{:from "empire.ui" :to "empire.acceptance" :type :direct}}}
          root-view (sut/view-architecture architecture [])
          scene (sut/build-scene root-view)
          y-by-label (into {} (map (juxt :label :y) (:layer-rects scene)))
          labels (mapv :label (:layer-rects scene))]
      (should= true (> (get y-by-label "acceptance")
                       (get y-by-label "ui")))
      (should-not= nil (some #{"ui"} labels))
      (should-not= nil (some #{"acceptance"} labels))
      (should-not= nil (some #{"application"} labels))))

  (it "aggregates grouped edge counts in namespace view"
    (let [architecture {:graph {:nodes #{"empire.a.one"
                                         "empire.b.one"
                                         "empire.a.two"
                                         "empire.b.two"}
                                :edges #{}
                                :abstract-modules #{}}
                        :classified-edges #{{:from "empire.a.one" :to "empire.b.one" :type :direct}
                                            {:from "empire.a.two" :to "empire.b.two" :type :direct}}}
          root-view (sut/view-architecture architecture [])
          edge (first (:classified-edges root-view))]
      (should= "a" (:from edge))
      (should= "b" (:to edge))
      (should= 2 (:count edge))))

  (it "applies 15px parallel spacing to overlapping non-layer edges"
    (let [points {"a" {:x 200.0 :y 60.0}
                  "b" {:x 200.0 :y 300.0}
                  "c" {:x 200.0 :y 120.0}
                  "d" {:x 200.0 :y 360.0}}
          edges [{:from "a" :to "b" :type :direct :arrowhead :standard}
                 {:from "c" :to "d" :type :direct :arrowhead :standard}]
          spaced (sut/apply-parallel-arrow-spacing edges points)
          [e1 e2] spaced
          o1 (or (:parallel-offset-x e1) (:parallel-offset-y e1))
          o2 (or (:parallel-offset-x e2) (:parallel-offset-y e2))]
      (should= 2 (count spaced))
      (should-not= nil o1)
      (should-not= nil o2)
      (should= 15.0 (Math/abs (- o1 o2))))))

  (it "separates overlapping angled arrows by 15px using perpendicular offsets"
    (let [points {"a" {:x 100.0 :y 100.0}
                  "b" {:x 200.0 :y 200.0}
                  "c" {:x 120.0 :y 120.0}
                  "d" {:x 220.0 :y 220.0}}
          edges [{:from "a" :to "b" :type :direct :arrowhead :standard}
                 {:from "c" :to "d" :type :direct :arrowhead :standard}]
          [e1 e2] (sut/apply-parallel-arrow-spacing edges points)
          v1 [(:parallel-offset-x e1) (:parallel-offset-y e1)]
          v2 [(:parallel-offset-x e2) (:parallel-offset-y e2)]
          dx (- (first v1) (first v2))
          dy (- (second v1) (second v2))
          dist (Math/sqrt (+ (* dx dx) (* dy dy)))]
      (should= 15.0 dist)))

  (it "keeps 15px spacing across many overlapping vertical arrows"
    (let [points {"a" {:x 100.0 :y 100.0}
                  "b" {:x 100.0 :y 300.0}
                  "c" {:x 100.0 :y 110.0}
                  "d" {:x 100.0 :y 310.0}
                  "e" {:x 100.0 :y 120.0}
                  "f" {:x 100.0 :y 320.0}
                  "g" {:x 100.0 :y 130.0}
                  "h" {:x 100.0 :y 330.0}}
          edges [{:from "a" :to "b" :type :direct :arrowhead :standard}
                 {:from "c" :to "d" :type :direct :arrowhead :standard}
                 {:from "e" :to "f" :type :direct :arrowhead :standard}
                 {:from "g" :to "h" :type :direct :arrowhead :standard}]
          offsets (->> (sut/apply-parallel-arrow-spacing edges points)
                       (map :parallel-offset-x)
                       sort
                       vec)]
      (should= [-22.5 -7.5 7.5 22.5] offsets)))

  (it "uses local lane counts per overlap cluster to avoid clamped restacking"
    (let [points (merge {"x1" {:x 100.0 :y 100.0}
                         "x2" {:x 100.0 :y 320.0}
                         "x3" {:x 100.0 :y 110.0}
                         "x4" {:x 100.0 :y 330.0}
                         "x5" {:x 100.0 :y 120.0}
                         "x6" {:x 100.0 :y 340.0}
                         "x7" {:x 100.0 :y 130.0}
                         "x8" {:x 100.0 :y 350.0}
                         "a1" {:x 300.0 :y 200.0}
                         "b1" {:x 600.0 :y 200.0}
                         "a2" {:x 300.0 :y 205.0}
                         "b2" {:x 600.0 :y 205.0}})
          edges [{:from "x1" :to "x2" :type :direct :arrowhead :standard}
                 {:from "x3" :to "x4" :type :direct :arrowhead :standard}
                 {:from "x5" :to "x6" :type :direct :arrowhead :standard}
                 {:from "x7" :to "x8" :type :direct :arrowhead :standard}
                 {:from "a1" :to "b1" :type :direct :arrowhead :standard}
                 {:from "a2" :to "b2" :type :direct :arrowhead :standard}]
          spaced (sut/apply-parallel-arrow-spacing edges points)
          right-cluster (->> spaced
                             (filter #(contains? #{"a1" "a2"} (:from %)))
                             (sort-by :from))
          offsets (mapv :parallel-offset-y right-cluster)]
      (should= 2 (count right-cluster))
      (should= [-7.5 7.5] (sort offsets))))

  (it "keeps rectangle-anchored endpoints on rectangle edges under offsets"
    (let [from-rect {:x 10.0 :y 20.0 :width 100.0 :height 60.0}
          to-rect {:x 300.0 :y 40.0 :width 120.0 :height 80.0}
          edge {:from "a"
                :to "b"
                :from-rect from-rect
                :to-rect to-rect
                :parallel-offset-x 12.0
                :parallel-offset-y 15.0
                :arrowhead :standard}
          points {"a" {:x 50.0 :y 50.0}
                  "b" {:x 350.0 :y 80.0}}
          recorded (atom nil)]
      (with-redefs [sut/draw-arrow-between-points (fn [x1 y1 x2 y2 _ _]
                                                    (reset! recorded [x1 y1 x2 y2]))]
        (#'sut/draw-edge points {:min-x 0.0 :max-x 1000.0 :min-y 0.0 :max-y 1000.0} edge))
      (let [[x1 _ x2 _] @recorded]
        (should= 110.0 x1)
        (should= 300.0 x2))))

  (it "does not force a fixed per-track vertical offset"
    (let [architecture {:layout {:layers [{:index 0 :modules ["a"]}
                                          {:index 1 :modules ["b"]}]
                                 :module->layer {"a" 0 "b" 1}}
                        :classified-edges #{}}
          scene (sut/build-scene architecture {:canvas-width 1000 :layer-height 120 :layer-gap 30})
          by-index (into {} (map (juxt :index identity) (:layer-rects scene)))
          y0 (:y (get by-index 0))
          y1 (:y (get by-index 1))]
      (should= y0 y1)))

  (it "avoids anchoring on rectangle corners for orthogonal arrows"
    (let [rect {:x 10.0 :y 20.0 :width 100.0 :height 60.0}
          right-top (#'sut/rect-edge-anchor rect 500.0 20.0)
          top-left (#'sut/rect-edge-anchor rect 10.0 -100.0)]
      (should= [110.0 28.0] (:point right-top))
      (should= [18.0 20.0] (:point top-left))))

  (it "cycles declutter modes across three states"
    (should= :concrete (sut/next-declutter-mode :all))
    (should= :abstract (sut/next-declutter-mode :concrete))
    (should= :all (sut/next-declutter-mode :abstract)))

  (it "filters edges by declutter mode"
    (let [scene {:module-positions [{:module "a" :layer 0 :x 100 :y 60}
                                    {:module "b" :layer 1 :x 200 :y 200}
                                    {:module "c" :layer 2 :x 300 :y 340}]
                 :layer-rects [{:index 0 :x 0 :y 0 :width 400 :height 100}
                               {:index 1 :x 0 :y 120 :width 400 :height 100}
                               {:index 2 :x 0 :y 240 :width 400 :height 100}]
                 :edge-drawables [{:from "a" :to "b" :type :direct :arrowhead :standard}
                                  {:from "b" :to "c" :type :abstract :arrowhead :closed-triangle}]}
          concrete (sut/declutter-edge-drawables scene :concrete)
          abstract (sut/declutter-edge-drawables scene :abstract)]
      (should= 1 (count concrete))
      (should= :direct (:type (first concrete)))
      (should= 1 (count abstract))
      (should= :abstract (:type (first abstract)))))

  (it "collapses layer edges and promotes abstract dependency type"
    (let [scene {:module-positions [{:module "a1" :layer 0 :x 100 :y 60}
                                    {:module "a2" :layer 0 :x 200 :y 60}
                                    {:module "b1" :layer 1 :x 120 :y 200}]
                 :layer-rects [{:index 0 :x 0 :y 0 :width 400 :height 100}
                               {:index 1 :x 0 :y 120 :width 400 :height 100}]
                 :edge-drawables [{:from "a1" :to "b1" :type :direct :arrowhead :standard}
                                  {:from "a2" :to "b1" :type :abstract :arrowhead :closed-triangle}]}
          layer-edges (sut/layer-edge-drawables scene)]
      (should= 1 (count layer-edges))
      (should= :abstract (:type (first layer-edges)))
      (should= :closed-triangle (:arrowhead (first layer-edges)))
      (should= 60.0 (second (:from-point (first layer-edges))))
      (should= 200.0 (second (:to-point (first layer-edges))))))

  (it "positions upward layer arrows from top of lower layer to bottom of upper layer"
    (let [scene {:module-positions [{:module "lower" :layer 2 :x 100 :y 300}
                                    {:module "upper" :layer 1 :x 120 :y 180}]
                 :layer-rects [{:index 1 :x 0 :y 120 :width 400 :height 100}
                               {:index 2 :x 0 :y 240 :width 400 :height 100}]
                 :edge-drawables [{:from "lower" :to "upper" :type :direct :arrowhead :standard}]}
          edge (first (sut/layer-edge-drawables scene))]
      (should= 300.0 (second (:from-point edge)))
      (should= 180.0 (second (:to-point edge)))))

  (it "separates overlapping layer arrows by 15px"
    (let [scene {:module-positions [{:module "a" :layer 0 :x 100 :y 60}
                                    {:module "b" :layer 2 :x 100 :y 300}
                                    {:module "c" :layer 1 :x 100 :y 180}
                                    {:module "d" :layer 3 :x 100 :y 420}]
                 :layer-rects [{:index 0 :x 0 :y 0 :width 400 :height 100}
                               {:index 1 :x 0 :y 120 :width 400 :height 100}
                               {:index 2 :x 0 :y 240 :width 400 :height 100}
                               {:index 3 :x 0 :y 360 :width 400 :height 100}]
                 :edge-drawables [{:from "a" :to "b" :type :direct :arrowhead :standard}
                                  {:from "c" :to "d" :type :direct :arrowhead :standard}]}
          edges (sort-by :from (sut/layer-edge-drawables scene))
          [e1 e2] edges
          o1 (or (:parallel-offset-x e1) (:parallel-offset-y e1))
          o2 (or (:parallel-offset-x e2) (:parallel-offset-y e2))]
      (should= 2 (count edges))
      (should-not= nil o1)
      (should-not= nil o2)
      (should= 15.0 (Math/abs (- o1 o2)))))

  (it "cycles declutter mode when declutter button is clicked"
    (let [state {:scene {:module-positions [] :layer-rects [] :edge-drawables []}
                 :architecture nil
                 :namespace-path nil
                 :declutter-mode :all
                 :dragging-scrollbar? false}
          next-state (sut/handle-mouse-clicked state {:x 120.0 :y 10.0})]
      (should= :concrete (:declutter-mode next-state))))

  (it "back button pops one namespace level"
    (let [architecture {:graph {:nodes #{"empire.alpha.one"
                                         "empire.alpha.two"
                                         "empire.beta.one"}
                                :edges #{}
                                :abstract-modules #{}}
                        :classified-edges #{}}
          alpha-view (sut/view-architecture architecture ["alpha"])
          alpha-scene (sut/attach-drillable-markers (sut/build-scene alpha-view) architecture ["alpha"])
          state {:scene alpha-scene
                 :architecture architecture
                 :namespace-path ["alpha"]
                 :nav-stack [{:path [] :scroll-y 140.0}]
                 :declutter-mode :all
                 :scroll-y 40.0
                 :dragging-scrollbar? false
                 :drag-offset nil
                 :viewport-height 600
                 :viewport-width 1200}
          next-state (sut/handle-mouse-clicked state {:x 20.0 :y 10.0})]
      (should= [] (:namespace-path next-state))
      (should= 140.0 (:scroll-y next-state))
      (should= #{"alpha" "beta"}
               (->> (get-in next-state [:scene :module-positions]) (map :module) set))))

  (it "clicking a namespace pushes current scroll and resets to top"
    (let [architecture {:graph {:nodes #{"empire.alpha.one"
                                         "empire.alpha.two"
                                         "empire.beta.one"}
                                :edges #{}
                                :abstract-modules #{}}
                        :classified-edges #{}}
          root-view (sut/view-architecture architecture [])
          root-scene (sut/attach-drillable-markers (sut/build-scene root-view) architecture [])
          alpha-pos (some #(when (= "alpha" (:module %)) %) (:module-positions root-scene))
          state {:scene root-scene
                 :architecture architecture
                 :namespace-path []
                 :nav-stack []
                 :declutter-mode :all
                 :scroll-y 123.0
                 :dragging-scrollbar? false
                 :drag-offset nil
                 :viewport-height 600
                 :viewport-width 1200}
          next-state (sut/handle-mouse-clicked state {:x (:x alpha-pos) :y (- (:y alpha-pos) 123.0)})]
      (should= ["alpha"] (:namespace-path next-state))
      (should= 0.0 (:scroll-y next-state))
      (should= [{:path [] :scroll-y 123.0}] (:nav-stack next-state))))

  (it "labels leaf nodes with source filename extension"
    (let [architecture {:graph {:nodes #{"empire.alpha.one"}
                                :edges #{}
                                :abstract-modules #{}
                                :module->source-file {"empire.alpha.one" "/tmp/one.cljc"}}
                        :classified-edges #{}}
          alpha-view (sut/view-architecture architecture ["alpha"])
          scene (sut/build-scene alpha-view)
          one-node (some #(when (= "one" (:module %)) %) (:module-positions scene))]
      (should= "one.cljc" (:label one-node))
      (should= true (:leaf? one-node))
      (should= "/tmp/one.cljc" (:source-file one-node))))

  (it "shows both namespace node and same-named leaf file when both exist"
    (let [architecture {:graph {:nodes #{"empire.application.state"
                                         "empire.application.state.effects"}
                                :edges #{}
                                :abstract-modules #{}
                                :module->source-file {"empire.application.state" "/tmp/state.cljc"
                                                      "empire.application.state.effects" "/tmp/effects.cljc"}}
                        :classified-edges #{}}
          app-view (sut/view-architecture architecture ["application"])
          app-scene (sut/attach-drillable-markers (sut/build-scene app-view) architecture ["application"])
          labels (set (map :display-label (:module-positions app-scene)))]
      (should-not= nil (some #{"+ state"} labels))
      (should-not= nil (some #{"state.cljc"} labels))))

  (it "marks namespace as abstract when any descendant module is abstract"
    (let [architecture {:graph {:nodes #{"empire.alpha.one.impl"
                                         "empire.alpha.two"}
                                :edges #{}
                                :abstract-modules #{"empire.alpha.one.impl"}
                                :module->source-file {"empire.alpha.one.impl" "/tmp/impl.cljc"
                                                      "empire.alpha.two" "/tmp/two.cljc"}}
                        :classified-edges #{}}
          root-view (sut/view-architecture architecture [])
          alpha-node (some #(when (= "alpha" (:module %)) %) (:module-positions (sut/build-scene root-view)))]
      (should= :abstract (:kind alpha-node))))

  (it "opens source window when leaf node is clicked"
    (let [opened (atom nil)
          architecture {:graph {:nodes #{"empire.alpha.one"}
                                :edges #{}
                                :abstract-modules #{}
                                :module->source-file {"empire.alpha.one" "/tmp/one.cljc"}}
                        :classified-edges #{}}
          alpha-view (sut/view-architecture architecture ["alpha"])
          scene (sut/attach-drillable-markers (sut/build-scene alpha-view) architecture ["alpha"])
          one-pos (some #(when (= "one" (:module %)) %) (:module-positions scene))
          state {:scene scene
                 :architecture architecture
                 :namespace-path ["alpha"]
                 :nav-stack []
                 :declutter-mode :all
                 :scroll-y 0.0
                 :dragging-scrollbar? false
                 :drag-offset nil
                 :viewport-height 600
                 :viewport-width 1200}]
      (with-redefs [sut/open-source-file-window! (fn [path] (reset! opened path))]
        (let [next-state (sut/handle-mouse-clicked state {:x (:x one-pos) :y (:y one-pos)})]
          (should= state next-state)
          (should= "/tmp/one.cljc" @opened)))))

  (it "renders source html with line numbers and preserved indentation"
    (let [html (#'sut/source->html "/tmp/example.clj" "(ns sample)\n\t(defn x []\n\t  :ok)\n")]
      (should= true (str/includes? html "class='ln'>1</td>"))
      (should= true (str/includes? html "class='ln'>2</td>"))
      (should= true (str/includes? html "<td class='code'><pre>  (defn x []</pre></td>"))
      (should= true (str/includes? html "<span class='kw'>:ok</span>"))))

  (it "back button at top level does nothing"
    (let [state {:scene {:module-positions [] :layer-rects [] :edge-drawables []}
                 :architecture nil
                 :namespace-path []
                 :nav-stack []
                 :declutter-mode :all
                 :dragging-scrollbar? false}
          next-state (sut/handle-mouse-clicked state {:x 20.0 :y 10.0})]
      (should= state next-state)))
