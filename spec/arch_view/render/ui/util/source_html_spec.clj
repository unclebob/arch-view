(ns arch-view.render.ui.util.source-html-spec
  (:require [clojure.string :as str]
            [arch-view.render.ui.util.source-html :as sut]
            [speclj.core :refer :all]))

(describe "source html"
  (it "escapes html-sensitive characters"
    (should= "&lt;a&amp;b&gt;" (sut/html-escape "<a&b>")))

  (it "colorizes strings, keywords, and comments"
    (let [html (sut/colorize-clojure-html "(println \"x\" :k) ; c")]
      (should= true (str/includes? html "class='str'"))
      (should= true (str/includes? html "class='kw'"))
      (should= true (str/includes? html "class='cmt'"))))

  (it "does not treat html escape entity semicolons as comments"
    (let [html (sut/colorize-clojure-html "(visibility-port/set-combat-visibility-port! (->MovementCombatVisibilityPort))")]
      (should= true (str/includes? html "-&gt;MovementCombatVisibilityPort"))
      (should= false (str/includes? html "-&gt;<span class='cmt'>;MovementCombatVisibilityPort"))))

  (it "renders tab-expanded source lines and wrapper html"
    (let [lines (sut/source-lines->html "\t:ok\n")
          doc (sut/source->html "demo" "\t:ok\n")]
      (should= true (str/includes? lines "class='ln'>1</td>"))
      (should= true (str/includes? lines "<pre>  <span class='kw'>:ok</span></pre>"))
      (should= true (str/includes? doc "<div class='hdr'>demo</div>")))))
