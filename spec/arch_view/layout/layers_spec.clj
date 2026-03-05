(ns arch-view.layout.layers-spec
  (:require [arch-view.layout.layers :as sut]
            [speclj.core :refer :all]))

(describe "layer assignment"
  (it "assigns modules to vertical layers by dependency depth"
    (let [graph {:nodes #{"a" "b" "c" "d"}
                 :edges #{{:from "a" :to "b"}
                          {:from "c" :to "b"}
                          {:from "b" :to "d"}}}
          layout (sut/assign-layers graph)]
      (should= [["d"] ["b"] ["a" "c"]]
               (mapv :modules (:layers layout)))
      (should= 2 (get-in layout [:module->layer "a"]))
      (should= 2 (get-in layout [:module->layer "c"]))
      (should= 1 (get-in layout [:module->layer "b"]))
      (should= 0 (get-in layout [:module->layer "d"])))))
