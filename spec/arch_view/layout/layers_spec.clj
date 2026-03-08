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
                                                                 :edges #{{:from "a" :to "b"}}})))))

  (it "ignores edges that reference nodes outside the graph"
    (let [layout (sut/assign-layers {:nodes #{"a" "b"}
                                     :edges #{{:from "a" :to "b"}
                                              {:from "a" :to "missing"}
                                              {:from "missing" :to "b"}}})]
      (should= #{{:from "a" :to "b"}} (:acyclic-edges layout))
      (should= #{} (:feedback-edges layout))
      (should= 0 (get-in layout [:module->layer "a"]))
      (should= 1 (get-in layout [:module->layer "b"]))))

  (it "treats self-loops as cyclic and removes them as feedback edges"
    (let [layout (sut/assign-layers {:nodes #{"a"}
                                     :edges #{{:from "a" :to "a"}}})]
      (should= #{{:from "a" :to "a"}} (:feedback-edges layout))
      (should= #{} (:acyclic-edges layout))
      (should= 0 (get-in layout [:module->layer "a"]))))

  (it "handles larger strongly connected components via heuristic feedback"
    (let [nodes (set (map #(str "n" %) (range 9)))
          edges (set (map (fn [i]
                            {:from (str "n" i)
                             :to (str "n" (mod (inc i) 9))})
                          (range 9)))
          layout (sut/assign-layers {:nodes nodes :edges edges})]
      (should= true (seq (:feedback-edges layout)))
      (should= true (every? integer? (vals (:module->layer layout))))
      (should= 9 (count (:module->layer layout)))))

  (it "orders nodes greedily across source sink and dense cycle branches"
    (let [source-order (#'sut/greedy-order #{"a" "b" "c"} #{{:from "a" :to "b"}})
          sink-order (#'sut/greedy-order #{"a" "b" "c"} #{{:from "a" :to "b"}
                                                          {:from "b" :to "a"}
                                                          {:from "a" :to "c"}})
          dense-order (#'sut/greedy-order #{"x" "y" "z"} #{{:from "x" :to "y"}
                                                           {:from "y" :to "z"}
                                                           {:from "z" :to "x"}})]
      (should= "a" (first source-order))
      (should= "c" (first sink-order))
      (should= 3 (count dense-order))))

  (it "chooses source then sink then cycle-break nodes"
    (let [remaining #{"a" "b" "c"}
          in-map {"a" #{} "b" #{"a"} "c" #{"a"}}
          out-map {"a" #{"b" "c"} "b" #{} "c" #{}}
          source-choice (#'sut/choose-next-node remaining in-map out-map)
          sink-choice (#'sut/choose-next-node #{"b" "c"}
                                              {"b" #{"x"} "c" #{"x"}}
                                              {"b" #{} "c" #{}})
          cycle-choice (#'sut/choose-next-node #{"x" "y"}
                                               {"x" #{"y"} "y" #{"x"}}
                                               {"x" #{"y"} "y" #{"x"}})]
      (should= {:node "a" :side :left} source-choice)
      (should= {:node "b" :side :right} sink-choice)
      (should= :left (:side cycle-choice))
      (should= true (contains? #{"x" "y"} (:node cycle-choice)))))

  (it "places nodes on left or right accumulation"
    (let [[left1 right1] (#'sut/place-node [] [] :left "a")
          [left2 right2] (#'sut/place-node left1 right1 :right "b")]
      (should= ["a"] left2)
      (should= ["b"] right2)))

  (it "builds node choices and greedy step transitions"
    (should= {:node "a" :side :left} (#'sut/node-choice "a" :left))
    (should= nil (#'sut/node-choice nil :right))
    (let [initial {:remaining #{"a" "b"}
                   :active-edges #{{:from "a" :to "b"}}
                   :left []
                   :right []}
          next-state (#'sut/greedy-step initial)
          done-state (#'sut/greedy-step {:remaining #{}
                                         :active-edges #{}
                                         :left ["a"]
                                         :right ["b"]})]
      (should= #{"b"} (:remaining next-state))
      (should= #{}
               (:active-edges next-state))
      (should= ["a"] (:left next-state))
      (should= ["a"] (:left done-state))
      (should= ["b"] (:right done-state))))

  (it "removes all incident edges for a node"
    (let [edges #{{:from "a" :to "b"}
                  {:from "b" :to "c"}
                  {:from "a" :to "a"}
                  {:from "c" :to "a"}}]
      (should= #{{:from "b" :to "c"}}
               (#'sut/remove-node edges "a"))))
))
