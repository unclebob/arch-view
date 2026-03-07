(ns arch-view.layout.layers-spec
  (:require [arch-view.layout.layers :as sut]
            [speclj.core :refer :all]))

(describe "layer assignment"
  (it "assigns modules to vertical levels from roots to leaves"
    (let [graph {:nodes #{"a" "b" "c" "d"}
                 :edges #{{:from "a" :to "b"}
                          {:from "c" :to "b"}
                          {:from "b" :to "d"}}}
          layout (sut/assign-layers graph)]
      (should= [["a" "c"] ["b"] ["d"]]
               (mapv :modules (:layers layout)))
      (should= 0 (get-in layout [:module->layer "a"]))
      (should= 0 (get-in layout [:module->layer "c"]))
      (should= 1 (get-in layout [:module->layer "b"]))
      (should= 2 (get-in layout [:module->layer "d"]))))

  (it "handles dependency cycles without recursion overflow"
    (let [graph {:nodes #{"a" "b" "c"}
                 :edges #{{:from "a" :to "b"}
                          {:from "b" :to "a"}
                          {:from "b" :to "c"}}}
          layout (sut/assign-layers graph)]
      (should= #{"a" "b" "c"}
               (set (keys (:module->layer layout))))
      (should= true (every? integer? (vals (:module->layer layout))))
      (should= true (boolean (seq (:feedback-edges layout))))
      (should= true (empty? (:feedback-edges (sut/assign-layers {:nodes #{"a" "b"}
                                                                 :edges #{{:from "a" :to "b"}}})))))))
