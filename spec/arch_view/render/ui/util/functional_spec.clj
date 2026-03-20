(ns arch-view.render.ui.util.functional-spec
  (:require [arch-view.render.ui.util.functional :as sut]
            [speclj.core :refer :all]))

(describe "functional helpers"
  (it "cycles declutter modes and labels"
    (should= :abstract (sut/next-declutter-mode :all))
    (should= :concrete (sut/next-declutter-mode :abstract))
    (should= :all (sut/next-declutter-mode :concrete))
    (should= "View: All" (sut/declutter-label :all))
    (should= "View: Abstract" (sut/declutter-label :abstract))
    (should= "View: Concrete" (sut/declutter-label :concrete)))

  (it "filters edge drawables by declutter mode"
    (let [scene {:edge-drawables [{:from "a" :to "b" :type :direct}
                                  {:from "b" :to "a" :type :abstract}]}]
      (should= 2 (count (sut/declutter-edge-drawables scene :all)))
      (should= 1 (count (sut/declutter-edge-drawables scene :concrete)))
      (should= 1 (count (sut/declutter-edge-drawables scene :abstract)))))

  (it "assigns layer slots and builds a scene"
    (let [placement (sut/assign-layer-slots [0 1 2] [[0 1] [2 1]] {0 0 1 4 2 0})
          architecture {:layout {:layers [{:index 0 :modules ["a"]}
                                          {:index 1 :modules ["b"]}]
                                 :module->layer {"a" 0 "b" 1}
                                 :cycles [["a" "b" "a"]]}
                        :module->display-label {"a" "alpha"
                                                "b" "beta"}
                        :cycle-lines ["deep.alpha->deep.beta->deep.alpha"]
                        :classified-edges #{{:from "a" :to "b" :type :direct}}}
          scene (sut/build-scene architecture)]
      (should= 3 (count (set (vals placement))))
      (should= 2 (count (:layer-rects scene)))
      (should= 1 (count (:edge-drawables scene)))
      (should= ["deep.alpha->deep.beta->deep.alpha"] (:cycle-lines scene))))

  (it "uses label helpers"
    (should= "b.module" (sut/abbreviate-module-name "alpha.beta.module"))
    (should= "beta.module" (sut/strip-top-namespace "alpha.beta.module"))
    (should= 35.0 (sut/label-width "abcde"))
    (should= "d" (sut/rendered-label {:display-label "d" :label "x"}))
    (should= ["alpha_" "beta"] (sut/split-label-lines "alpha_beta" 9))))
