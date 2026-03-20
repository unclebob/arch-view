;; mutation-tested: 2026-03-08
(ns arch-view.render.ui.util.view-bootstrap)

(defn viewport-height-for-scene
  [scene content-height-for-scene]
  (let [content-height (if (seq (:layer-rects scene))
                         (content-height-for-scene scene)
                         400)]
    (int (min 900 (max 400 content-height)))))

(defn viewport-width-for-scene
  [scene content-width-for-scene]
  (int (max 1200 (content-width-for-scene scene))))

(defn initial-sketch-state
  [{:keys [scene architecture has-architecture? viewport-height viewport-width reload-architecture]}]
  {:scene scene
   :architecture architecture
   :reload-architecture reload-architecture
   :namespace-path (when has-architecture? [])
   :nav-stack []
   :declutter-mode :all
   :zoom 1.0
   :zoom-stack []
   :suppress-next-click? false
   :scroll-x 0.0
   :scroll-y 0.0
   :reanalyze-requested? false
   :reanalyze-status nil
   :reanalyze-started-at nil
   :dependency-tooltip-scroll 0.0
   :dependency-tooltip-key nil
   :dragging-scrollbar? false
   :drag-offset nil
   :routed-edges nil
   :viewport-height viewport-height
   :viewport-width viewport-width})
