(ns arch-view.model.classify-spec
  (:require [arch-view.model.classify :as sut]
            [speclj.core :refer :all]))

(describe "edge classification"
  (it "marks dependencies to abstract modules with :abstract type"
    (let [guidance {:component-rules [{:component :api :kind :abstract :match "^my\\.app\\.api(\\..+)?$"}
                                      {:component :all :kind :concrete :match "^my\\.app(\\..+)?$"}]}
          graph {:nodes #{"my.app.impl.core" "my.app.api.port" "my.app.util"}
                 :edges #{{:from "my.app.impl.core" :to "my.app.api.port"}
                          {:from "my.app.impl.core" :to "my.app.util"}}}
          classified (sut/classify-edges guidance graph)]
      (should= #{{:from "my.app.impl.core" :to "my.app.api.port" :type :abstract}
                 {:from "my.app.impl.core" :to "my.app.util" :type :direct}}
               classified)))

  (it "marks dependencies as abstract when target module is polymorphic"
    (let [guidance {:component-rules []}
          graph {:nodes #{"my.app.impl.core" "my.app.port.api"}
                 :edges #{{:from "my.app.impl.core" :to "my.app.port.api"}}
                 :abstract-modules #{"my.app.port.api"}}
          classified (sut/classify-edges guidance graph)]
      (should= #{{:from "my.app.impl.core" :to "my.app.port.api" :type :abstract}}
               classified))))
