(ns arch-view.render.quil-view-spec
  (:require [clojure.string :as str]
            [arch-view.render.ui.quil.view :as sut]
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
      (should= [85.6 85.6 85.6 85.6]
               (mapv :width (:layer-rects scene)))
      (should= true (every? #(<= 24.0 (:x %)) (:layer-rects scene)))
      (should= true (every? #(>= (:y %) 42.0) (:layer-rects scene)))
      (should= ["a" "c"]
               (->> (:module-positions scene)
                    (filter #(contains? #{"a" "c"} (:module %)))
                    (sort-by :x)
                    (mapv :module)))
      (should= ["a" "c"]
               (->> (:module-positions scene)
                    (filter #(contains? #{"a" "c"} (:module %)))
                    (sort-by :x)
                    (mapv :label)))
      (should= ["d" "b" "a" "c"]
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
          by-label (into {} (map (juxt :label identity) (:layer-rects scene)))]
      (should= false (:abstract? (get by-label "alpha")))
      (should= true (:abstract? (get by-label "beta")))))

  (it "marks module labels as cycle-related when subtree contains a cycle"
    (let [architecture {:layout {:layers [{:index 0 :modules ["alpha"]}
                                          {:index 1 :modules ["beta"]}]
                                 :module->layer {"alpha" 0 "beta" 1}}
                        :module->cycle? {"alpha" true}
                        :classified-edges #{}}
          scene (sut/build-scene architecture {:canvas-width 1000 :layer-height 120 :layer-gap 30})
          by-module (into {} (map (juxt :module identity) (:module-positions scene)))]
      (should= true (:cycle? (get by-module "alpha")))
      (should= false (:cycle? (get by-module "beta")))))

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
      (should= [72.0] ys)))

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
      (should= [72.0] ys)))

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
    (should= "b.c.core" (sut/abbreviate-module-name "app.backend.cache.core"))
    (should= "j.alice" (sut/abbreviate-module-name "bob.jim.alice.clj")))

  (it "strips top-level namespace for hover names"
    (should= "beta.module" (sut/strip-top-namespace "alpha.beta.module"))
    (should= "module" (sut/strip-top-namespace "module")))

  (it "formats edge hover labels with dependency count"
    (should= "a->b(3)" (sut/edge-hover-label {:from "a" :to "b" :count 3}))
    (should= "a->b(1)" (sut/edge-hover-label {:from "a" :to "b"})))

  (it "labels back button with destination namespace"
    (should= "Back" (#'sut/back-button-label {:namespace-path [] :nav-stack []}))
    (should= "Back: application" (#'sut/back-button-label {:namespace-path ["application"]
                                                           :nav-stack [{:path []}]}))
    (should= "Back: state" (#'sut/back-button-label {:namespace-path ["application" "state"]
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

  (it "separates overlapping arrows that share the same source"
    (let [points {"acceptance" {:x 100.0 :y 100.0}
                  "config" {:x 300.0 :y 170.0}
                  "state" {:x 300.0 :y 230.0}}
          edges [{:from "acceptance" :to "config" :type :direct :arrowhead :standard}
                 {:from "acceptance" :to "state" :type :direct :arrowhead :standard}]
          [e1 e2] (sut/apply-parallel-arrow-spacing edges points)
          v1 [(:parallel-offset-x e1) (:parallel-offset-y e1)]
          v2 [(:parallel-offset-x e2) (:parallel-offset-y e2)]
          dx (- (first v1) (first v2))
          dy (- (second v1) (second v2))
          dist (Math/sqrt (+ (* dx dx) (* dy dy)))]
      (should= true (< (Math/abs (- dist 15.0)) 0.2))))

  (it "uses rectilinear routing when a short edge crosses a rectangle"
    (let [edge {:all-rects [{:x 160.0 :y 120.0 :width 60.0 :height 80.0}]
                :from-rect nil
                :to-rect nil}]
      (should= true (boolean (#'sut/needs-rectilinear-route? 120.0 150.0 240.0 170.0 edge)))))

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

  (it "marks drillable namespace labels without prefix decoration"
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
      (should= "alpha" (get labels "alpha"))
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
      (should= "alpha" (get labels "alpha"))
      (should= "beta" (get labels "beta"))))

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
          labels (mapv :label (:layer-rects scene))
          modules (set (map :module (:module-positions scene)))]
      (should= true (> (get y-by-label "acceptance")
                       (get y-by-label "ui")))
      (should-not= nil (some #{"ui"} labels))
      (should-not= nil (some #{"acceptance"} labels))
      (should= true (contains? modules "application"))))

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

  (it "preserves external dependencies in drilldown display edges"
    (let [architecture {:graph {:nodes #{"empire.acceptance.generator"
                                         "empire.acceptance.parser.ir-contracts"}
                                :edges #{}
                                :abstract-modules #{}}
                        :classified-edges #{{:from "empire.acceptance.generator"
                                             :to "empire.acceptance.parser.ir-contracts"
                                             :type :direct}}}
          parser-view (sut/view-architecture architecture ["acceptance" "parser"])]
      (should-not= nil
                   (some #(and (= (:from %) "empire.acceptance.generator")
                               (= (:to %) "ir-contracts"))
                         (:display-edges parser-view)))))

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

  (it "routes long crossing edges with vertical segments outside blocking rectangles"
    (let [from-rect {:x 20.0 :y 20.0 :width 120.0 :height 80.0}
          blocker {:x 230.0 :y 80.0 :width 140.0 :height 260.0}
          to-rect {:x 520.0 :y 320.0 :width 120.0 :height 80.0}
          points {"a" {:x 80.0 :y 60.0}
                  "b" {:x 580.0 :y 360.0}}
          edge {:from "a"
                :to "b"
                :arrowhead :standard
                :from-rect from-rect
                :to-rect to-rect
                :all-rects [from-rect blocker to-rect]}
          resolved (#'sut/resolved-edge-path points {:min-x 0.0 :max-x 900.0 :min-y 0.0 :max-y 700.0} edge)
          segments (map vector (:points resolved) (rest (:points resolved)))
          verticals (filter (fn [[p1 p2]]
                              (< (Math/abs (- (double (first p1))
                                              (double (first p2))))
                                 0.1))
                            segments)]
      (should= true (seq verticals))
      (doseq [[[x1 y1] [x2 y2]] verticals]
        (should= false (#'sut/segment-intersects-rect? x1 y1 x2 y2 blocker)))))

  (it "ensures final segment is perpendicular to target rectangle side"
    (let [from-rect {:x 40.0 :y 40.0 :width 100.0 :height 80.0}
          to-rect {:x 260.0 :y 240.0 :width 120.0 :height 80.0}
          points {"a" {:x 90.0 :y 80.0}
                  "b" {:x 320.0 :y 240.0}}
          edge {:from "a"
                :to "b"
                :arrowhead :standard
                :from-rect from-rect
                :to-rect to-rect
                :all-rects [from-rect to-rect]}
          resolved (#'sut/resolved-edge-path points {:min-x 0.0 :max-x 900.0 :min-y 0.0 :max-y 700.0} edge)
          [p1 p2] (take-last 2 (:points resolved))
          dx (Math/abs (- (double (first p2)) (double (first p1))))
          dy (Math/abs (- (double (second p2)) (double (second p1))))]
      (should= true (or (< dx 0.1) (< dy 0.1)))))

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
      (should= [110.0 30.0] (:point right-top))
      (should= [20.0 20.0] (:point top-left))))

  (it "cycles view modes across three states"
    (should= :abstract (sut/next-declutter-mode :all))
    (should= :concrete (sut/next-declutter-mode :abstract))
    (should= :all (sut/next-declutter-mode :concrete)))

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

  (it "labels view modes as requested"
    (should= "View: All" (sut/declutter-label :all))
    (should= "View: Abstract" (sut/declutter-label :abstract))
    (should= "View: Concrete" (sut/declutter-label :concrete))
    (should= "View: All" (sut/declutter-label :layer)))

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

  (it "reuses the same lane for non-overlapping layer arrows"
    (let [scene {:module-positions [{:module "a" :layer 0 :x 100 :y 60}
                                    {:module "b" :layer 1 :x 100 :y 180}
                                    {:module "c" :layer 2 :x 500 :y 300}
                                    {:module "d" :layer 3 :x 500 :y 420}]
                 :layer-rects [{:index 0 :x 0 :y 0 :width 400 :height 100}
                               {:index 1 :x 0 :y 120 :width 400 :height 100}
                               {:index 2 :x 420 :y 240 :width 400 :height 100}
                               {:index 3 :x 420 :y 360 :width 400 :height 100}]
                 :edge-drawables [{:from "a" :to "b" :type :direct :arrowhead :standard}
                                  {:from "c" :to "d" :type :direct :arrowhead :standard}]}
          offsets (->> (sut/layer-edge-drawables scene)
                       (map (juxt :parallel-offset-x :parallel-offset-y))
                       distinct
                       vec)]
      (should= 1 (count offsets))))

  (it "ignores same-layer and unknown-module edges in layer mode"
    (let [scene {:module-positions [{:module "a" :layer 0 :x 100 :y 60}
                                    {:module "b" :layer 0 :x 200 :y 60}]
                 :layer-rects [{:index 0 :x 0 :y 0 :width 400 :height 100}]
                 :edge-drawables [{:from "a" :to "b" :type :direct :arrowhead :standard}
                                  {:from "x" :to "y" :type :direct :arrowhead :standard}]}
          edges (sut/layer-edge-drawables scene)]
      (should= [] edges)))

  (it "does not change declutter mode from toolbar non-back clicks"
    (let [state {:scene {:module-positions [] :layer-rects [] :edge-drawables []}
                 :architecture nil
                 :namespace-path nil
                 :declutter-mode :all
                 :dragging-scrollbar? false}
          next-state (sut/handle-mouse-clicked state {:x 120.0 :y 10.0})]
      (should= :all (:declutter-mode next-state))))

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

  (it "labels leaf nodes with source filename base name"
    (let [architecture {:graph {:nodes #{"empire.alpha.one"}
                                :edges #{}
                                :abstract-modules #{}
                                :module->source-file {"empire.alpha.one" "/tmp/one.cljc"}}
                        :classified-edges #{}}
          alpha-view (sut/view-architecture architecture ["alpha"])
          scene (sut/build-scene alpha-view)
          one-node (some #(when (= "one" (:module %)) %) (:module-positions scene))]
      (should= "one" (:label one-node))
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
      (should-not= nil (some #{"state"} labels))
      (should-not= nil (some #{"state"} labels))))

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

(describe "quil decrap coverage"
  (it "handles segment overlap and invalid-touch edge cases"
    (let [rect {:x 10.0 :y 20.0 :width 100.0 :height 60.0}
          to-rect {:x 240.0 :y 40.0 :width 120.0 :height 80.0}
          edge {:from-rect rect :to-rect to-rect}]
      (should= true (#'sut/segment-overlaps-rect-edge? [[20.0 20.0] [90.0 20.0]] rect))
      (should= true (#'sut/segment-overlaps-rect-edge? [[10.0 30.0] [10.0 70.0]] rect))
      (should= false (#'sut/segment-overlaps-rect-edge? [[20.0 30.0] [90.0 40.0]] rect))
      (should= false (#'sut/segment-invalid-for-rect? 0 1 [[0.0 0.0] [5.0 5.0]] rect edge))
      (should= true (#'sut/segment-invalid-for-rect? 0 1 [[60.0 40.0] [80.0 50.0]] rect edge))
      (should= false (#'sut/segment-invalid-for-rect? 0 1 [[110.0 40.0] [140.0 40.0]] rect edge))
      (should= true (#'sut/segment-invalid-for-rect? 0 1 [[90.0 40.0] [140.0 40.0]] rect edge))))

  (it "applies ctrl-click zoom in and out"
    (let [state {:scene {:layer-rects [{:x 0.0 :y 0.0 :width 2200.0 :height 1600.0}]}
                 :zoom 1.0
                 :zoom-stack []
                 :scroll-y 200.0
                 :viewport-height 700}
          no-ctrl (#'sut/apply-zoom-click state {:button :right})
          zoomed (#'sut/apply-zoom-click state {:control true :button :right :x 180.0 :y 300.0})
          restored (#'sut/apply-zoom-click zoomed {:control true :button :left :x 180.0 :y 300.0})]
      (should= nil no-ctrl)
      (should= 1.1 (:zoom zoomed))
      (should= 1 (count (:zoom-stack zoomed)))
      (should= 1.0 (:zoom restored))
      (should= 0 (count (:zoom-stack restored)))
      (should= nil (#'sut/apply-zoom-click state {:control true :button :left :x 180.0 :y 300.0}))
      (should= nil (#'sut/apply-zoom-click state {:control true :button :center :x 180.0 :y 300.0}))))

  (it "draws single-segment edges through draw-arrow-between-points"
    (let [called (atom nil)]
      (with-redefs [sut/draw-arrow-between-points (fn [x1 y1 x2 y2 arrowhead use-clearance?]
                                                    (reset! called [x1 y1 x2 y2 arrowhead use-clearance?]))
                    quil.core/stroke (fn [& _])
                    quil.core/line (fn [& _])
                    sut/draw-arrowhead (fn [& _])]
        (#'sut/draw-edge {} {:min-x 0.0 :max-x 500.0 :min-y 0.0 :max-y 500.0}
         {:route-points [[10.0 20.0] [40.0 20.0]]
          :arrowhead :standard}))
      (should= [10.0 20.0 40.0 20.0 :standard true] @called)))

  (it "draws multi-segment edges by drawing each segment and arrowhead"
    (let [lines (atom [])
          arrow (atom nil)]
      (with-redefs [quil.core/stroke (fn [& _])
                    quil.core/line (fn [x1 y1 x2 y2] (swap! lines conj [x1 y1 x2 y2]))
                    sut/draw-arrowhead (fn [x1 y1 x2 y2 arrowhead] (reset! arrow [x1 y1 x2 y2 arrowhead]))
                    sut/draw-arrow-between-points (fn [& _] (throw (ex-info "unexpected single-segment path" {})))]
        (#'sut/draw-edge {} {:min-x 0.0 :max-x 500.0 :min-y 0.0 :max-y 500.0}
         {:route-points [[0.0 0.0] [20.0 0.0] [20.0 30.0]]
          :arrowhead :closed-triangle}))
      (should= [[0.0 0.0 20.0 0.0]
                [20.0 0.0 20.0 20.0]]
               @lines)
      (should= [20.0 0.0 20.0 30.0 :closed-triangle] @arrow)))

  (it "draw-scene chooses cursor and tooltip based on hovered entities"
    (let [state {:scene {:layer-rects [{:x 0.0 :y 0.0 :width 400.0 :height 200.0}]
                        :module-positions [{:module "n" :x 20.0 :y 20.0}]
                        :edge-drawables []}
                 :declutter-mode :all
                 :scroll-x 0.0
                 :scroll-y 0.0
                 :viewport-height 300
                 :viewport-width 500
                 :zoom 1.0}
          cursor-calls (atom [])
          tooltip-calls (atom [])]
      (with-redefs [quil.core/mouse-x (fn [] 40.0)
                    quil.core/mouse-y (fn [] 50.0)
                    quil.core/cursor (fn [kind] (swap! cursor-calls conj kind))
                    quil.core/background (fn [& _])
                    quil.core/push-matrix (fn [] nil)
                    quil.core/scale (fn [& _])
                    quil.core/translate (fn [& _])
                    quil.core/pop-matrix (fn [] nil)
                    sut/draw-scene-content (fn [& _])
                    sut/draw-toolbar (fn [& _])
                    sut/draw-scrollbar (fn [& _])
                    sut/dependency-indicators (fn [& _] [{:tooltip-lines ["a->b(2)"]}])
                    sut/hovered-module-position (fn [& _] {:full-name "module.full" :drillable? true})
                    sut/hovered-layer-label (fn [& _] nil)
                    sut/hovered-dependency (fn [& _] {:tooltip-lines ["a->b(2)"]})
                    sut/draw-tooltip (fn [text _ _] (swap! tooltip-calls conj text))
                    sut/draw-tooltip-lines (fn [lines _ _] (swap! tooltip-calls into lines))]
        (#'sut/draw-scene state)
        (with-redefs [sut/hovered-module-position (fn [& _] nil)
                      sut/hovered-layer-label (fn [& _] {:full-name "layer.full"})]
          (#'sut/draw-scene state))
        (with-redefs [sut/hovered-module-position (fn [& _] nil)
                      sut/hovered-layer-label (fn [& _] nil)]
          (#'sut/draw-scene state)))
      (should= [:cross :arrow :arrow] @cursor-calls)
      (should= ["module.full" "layer.full" "a->b(2)"] @tooltip-calls))))

(describe "quil routing helper coverage"
  (it "applies constrained offsets by rectangle side"
    (let [rect {:x 10.0 :y 20.0 :width 100.0 :height 80.0}]
      (should= [10.0 40.0] (#'sut/apply-edge-constrained-offset [10.0 35.0] :left rect 0.0 5.0))
      (should= [110.0 90.0] (#'sut/apply-edge-constrained-offset [110.0 85.0] :right rect 0.0 20.0))
      (should= [30.0 20.0] (#'sut/apply-edge-constrained-offset [20.0 20.0] :top rect 10.0 0.0))
      (should= [100.0 100.0] (#'sut/apply-edge-constrained-offset [90.0 100.0] :bottom rect 10.0 0.0))))

  (it "computes source exit points for anchored and free edges"
    (let [bounds {:min-x 0.0 :max-x 200.0 :min-y 0.0 :max-y 200.0}]
      (should= [50.0 30.0] (#'sut/source-exit-point 50.0 50.0 100.0 100.0 :top bounds))
      (should= [50.0 70.0] (#'sut/source-exit-point 50.0 50.0 100.0 100.0 :bottom bounds))
      (should= [30.0 50.0] (#'sut/source-exit-point 50.0 50.0 100.0 100.0 :left bounds))
      (should= [70.0 50.0] (#'sut/source-exit-point 50.0 50.0 100.0 100.0 :right bounds))
      (should= [61.0 72.0] (#'sut/source-exit-point 50.0 50.0 100.0 150.0 nil bounds))))

  (it "enforces perpendicular source and target segments"
    (let [path [[0.0 0.0] [30.0 10.0] [50.0 40.0]]
          source-fixed (#'sut/enforce-source-perpendicular path :top)
          target-fixed (#'sut/enforce-target-perpendicular path :right)]
      (should= [0.0 0.0] (first source-fixed))
      (should= 0.0 (first (second source-fixed)))
      (should= 40.0 (second (last target-fixed)))
      (should= 50.0 (first (last target-fixed)))))

  (it "detects segment intersections and collinear overlap"
    (should= true (#'sut/segments-intersect? 0.0 0.0 10.0 10.0 0.0 10.0 10.0 0.0))
    (should= false (#'sut/segments-intersect? 0.0 0.0 5.0 0.0 0.0 1.0 5.0 1.0))
    (should= true (#'sut/segments-intersect? 0.0 0.0 10.0 0.0 10.0 0.0 20.0 0.0))
    (should= true (#'sut/collinear-overlap? [[0.0 0.0] [10.0 0.0]] [[5.0 0.0] [15.0 0.0]]))
    (should= false (#'sut/collinear-overlap? [[0.0 0.0] [10.0 0.0]] [[11.0 0.0] [15.0 0.0]]))
    (should= true (#'sut/collinear-overlap? [[2.0 1.0] [2.0 10.0]] [[2.0 6.0] [2.0 14.0]]))
    (should= false (#'sut/collinear-overlap? [[0.0 0.0] [5.0 5.0]] [[0.0 1.0] [5.0 6.0]])))

  (it "adjusts source/target perpendicular path variants for each side group"
    (let [target-path [[0.0 0.0] [5.0 5.0] [10.0 20.0]]
          source-path [[0.0 0.0] [10.0 8.0] [20.0 8.0]]
          already-target [[0.0 0.0] [10.0 5.0] [10.0 20.0]]
          already-source [[0.0 0.0] [0.0 8.0] [20.0 8.0]]]
      (should= [[0.0 0.0] [5.0 5.0] [10.0 5.0] [10.0 20.0]]
               (vec (#'sut/adjust-target-perpendicular target-path :top)))
      (should= [[0.0 0.0] [5.0 5.0] [5.0 20.0] [10.0 20.0]]
               (vec (#'sut/adjust-target-perpendicular target-path :right)))
      (should= already-target (vec (#'sut/adjust-target-perpendicular already-target :bottom)))
      (should= [0.0 0.0] (first (vec (#'sut/adjust-target-perpendicular already-target :left))))
      (should= [10.0 20.0] (last (vec (#'sut/adjust-target-perpendicular already-target :left))))
      (should= target-path (vec (#'sut/adjust-target-perpendicular target-path nil)))
      (should= [[0.0 0.0] [0.0 8.0] [20.0 8.0]]
               (vec (#'sut/adjust-source-perpendicular source-path :top)))
      (should= [[0.0 0.0] [10.0 0.0] [20.0 8.0]]
               (vec (#'sut/adjust-source-perpendicular source-path :left)))
      (should= [0.0 0.0] (first (vec (#'sut/adjust-source-perpendicular already-source :bottom))))
      (should= [20.0 8.0] (last (vec (#'sut/adjust-source-perpendicular already-source :bottom))))
      (should= [0.0 0.0] (first (vec (#'sut/adjust-source-perpendicular already-source :right))))
      (should= [20.0 8.0] (last (vec (#'sut/adjust-source-perpendicular already-source :right))))
      (should= source-path (vec (#'sut/adjust-source-perpendicular source-path nil)))))

  (it "resolves direct path without rectilinear detour when no blockers"
    (let [points {"a" {:x 10.0 :y 10.0}
                  "b" {:x 60.0 :y 30.0}}
          resolved (#'sut/resolved-edge-path points
                                             {:min-x 0.0 :max-x 200.0 :min-y 0.0 :max-y 200.0}
                                             {:from "a" :to "b"
                                              :from-rect nil
                                              :to-rect nil
                                              :all-rects []})]
      (should= [[10.0 10.0] [60.0 10.0] [60.0 30.0]] (:points resolved))
      (should= false (:anchored? resolved))))

  (it "computes ignored rect set and adjusted source exits for blocked/unblocked first segment"
    (let [r1 {:x 0.0 :y 0.0 :width 10.0 :height 10.0}
          r2 {:x 20.0 :y 20.0 :width 10.0 :height 10.0}
          edge {:from-rect r1 :to-rect r2 :all-rects [r1 r2]}
          bounds {:min-x 0.0 :max-x 200.0 :min-y 0.0 :max-y 200.0}]
      (should= #{} (#'sut/edge-ignored-rects {}))
      (should= #{r1 r2} (#'sut/edge-ignored-rects edge))
      (with-redefs [sut/segment-intersects-any-rect? (fn [& _] false)]
        (should= [30.0 10.0]
                 (#'sut/adjusted-source-exit-point 30.0 30.0 100.0 100.0 :top bounds edge)))
      (with-redefs [sut/segment-intersects-any-rect? (fn [& _] true)]
        (should= [30.0 50.0]
                 (#'sut/adjusted-source-exit-point 30.0 30.0 100.0 100.0 :top bounds edge))))))

  (it "nudge-path preserves endpoints while keeping an orthogonal path"
    (let [path [[0.0 0.0] [10.0 0.0] [10.0 20.0] [20.0 20.0]]
          nudged (#'sut/nudge-path path 0.0 10.0)]
      (should= [0.0 0.0] (first nudged))
      (should= [20.0 20.0] (last nudged))
      (should= true (>= (count nudged) 3))))

  (it "chooses midpoint route column when clear"
    (let [bounds {:min-x 0.0 :max-x 500.0 :min-y 0.0 :max-y 500.0}
          x (#'sut/choose-route-column 100.0 100.0 300.0 300.0 {:all-rects [] :from-rect nil :to-rect nil} bounds)]
      (should= 200.0 x)))

  (it "returns nil when no non-overlapping sidestep candidate is available"
    (let [base [[0.0 0.0] [50.0 0.0] [100.0 0.0]]
          placed [[[0.0 0.0] [100.0 0.0]]]
          chosen (#'sut/place-non-overlapping-path base {:all-rects []} placed)]
      (should= nil chosen)))

  (it "checks safe displayability and close wait behavior"
    (should= true (#'sut/safe-displayable? nil true))
    (should= false (#'sut/safe-displayable? (Object.) false))
    (let [looping-calls (atom 0)]
      (with-redefs [sut/safe-looping? (fn [_] (swap! looping-calls inc) true)
                    sut/safe-displayable? (fn [_ _] false)]
        (sut/wait-until-closed! :sketch))
      (should= 1 @looping-calls)
      (sut/wait-until-closed! nil)))

  (it "builds sketch options and initial state in show!"
    (let [captured (atom nil)
          scene {:layer-rects [{:x 0.0 :y 0.0 :width 300.0 :height 120.0}]
                 :module-positions []
                 :edge-drawables []}]
      (with-redefs [quil.core/sketch (fn [& args] (reset! captured (apply hash-map args)) :ok)]
        (should= :ok (sut/show! scene {:title "T"})))
      (let [opts @captured
            setup-state ((:setup opts))]
        (should= "T" (:title opts))
        (should= true (contains? setup-state :scene))
        (should= true (contains? setup-state :viewport-height))
        (should= true (contains? opts :mouse-released)))))

(describe "quil wrapper helper coverage"
  (it "scales scroll for zoom and handles non-positive prior zoom"
    (should= 200.0 (#'sut/scaled-scroll-for-zoom 100.0 1.0 2.0))
    (should= 100.0 (#'sut/scaled-scroll-for-zoom 100.0 0.0 2.0)))

  (it "draws arrow between points for both direct and abstract edges"
    (let [strokes (atom [])
          lines (atom [])
          arrows (atom [])]
      (with-redefs [quil.core/stroke (fn [& xs] (swap! strokes conj xs))
                    quil.core/line (fn [& xs] (swap! lines conj xs))
                    sut/draw-arrowhead (fn [& xs] (swap! arrows conj xs))]
        (#'sut/draw-arrow-between-points 0.0 0.0 20.0 0.0 :standard false)
        (#'sut/draw-arrow-between-points 0.0 0.0 20.0 20.0 :closed-triangle true))
      (should= 2 (count @lines))
      (should= 2 (count @arrows))
      (should= true (some #(= [0 0 0] %) @strokes))
      (should= true (some #(= [0 128 0] %) @strokes))))

  (it "prepares edge drawables by attaching layer rectangles and route points"
    (let [scene {:layer-rects [{:index [0 "a"] :module "a" :x 0.0 :y 0.0 :width 100.0 :height 80.0}
                               {:index [1 "b"] :module "b" :x 200.0 :y 120.0 :width 100.0 :height 80.0}]
                 :module-positions [{:module "a" :layer [0 "a"] :x 20.0 :y 20.0}
                                    {:module "b" :layer [1 "b"] :x 240.0 :y 140.0}]
                 :edge-drawables [{:from "a" :to "b" :type :direct :arrowhead :standard}]}
          routed (#'sut/prepare-edge-drawables scene :all)
          edge (first routed)]
      (should= 1 (count routed))
      (should-not= nil (:from-rect edge))
      (should-not= nil (:to-rect edge))
      (should= true (>= (count (:route-points edge)) 4))))

  (it "routes adjacent levels directly and multi-level edges via rectangle sides"
    (let [scene {:layer-rects [{:index [0 "a"] :module "a" :row 0 :x 200.0 :y 40.0 :width 100.0 :height 60.0}
                               {:index [1 "b"] :module "b" :row 1 :x 220.0 :y 130.0 :width 100.0 :height 60.0}
                               {:index [2 "c"] :module "c" :row 2 :x 500.0 :y 220.0 :width 100.0 :height 60.0}]
                 :module-positions [{:module "a" :layer [0 "a"] :x 250.0 :y 70.0}
                                    {:module "b" :layer [1 "b"] :x 270.0 :y 160.0}
                                    {:module "c" :layer [2 "c"] :x 550.0 :y 250.0}]
                 :edge-drawables [{:from "a" :to "b" :type :direct :arrowhead :standard}
                                  {:from "a" :to "c" :type :direct :arrowhead :standard}]}
          routed (sort-by :to (#'sut/prepare-edge-drawables scene :all))
          adjacent (first routed)
          multi (second routed)]
      (should= "b" (:to adjacent))
      (should= true (contains? #{:left :right :top :bottom} (:from-side adjacent)))
      (should= true (contains? #{:left :right :top :bottom} (:to-side adjacent)))
      (let [pts (:route-points adjacent)
            [p1 p2] (take-last 2 pts)
            [s1 s2] (take 2 pts)]
        (if (contains? #{:left :right} (:from-side adjacent))
          (should= true (= (second s1) (second s2)))
          (should= true (= (first s1) (first s2))))
        (if (contains? #{:left :right} (:to-side adjacent))
          (should= true (= (second p1) (second p2)))
          (should= true (= (first p1) (first p2)))))
      (should= "c" (:to multi))
      (should= true (>= (count (:route-points multi)) 4))
      (should= true (contains? #{:left :right} (:from-side multi)))
      (should= :top (:to-side multi))
      (let [[[sx _] [x2 _]] (:route-points multi)]
        (if (= :left (:from-side multi))
          (should= true (<= x2 sx))
          (should= true (>= x2 sx))))))
  )
