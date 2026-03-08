(ns arch-view.render.ui.util.functional
  (:require [arch-view.render.ui.util.labels :as labels]
            [arch-view.render.ui.util.layout :as layout]
            [arch-view.render.ui.util.routing :as routing]))

(def layer-y layout/layer-y)
(def dominant-component layout/dominant-component)
(def module-positions-for-layer layout/module-positions-for-layer)

(def abbreviate-module-name labels/abbreviate-module-name)
(def strip-top-namespace labels/strip-top-namespace)
(def label-width labels/label-width)
(def rendered-label labels/rendered-label)
(def rendered-label-lines labels/rendered-label-lines)
(def split-label-lines labels/split-label-lines)
(def declutter-modes labels/declutter-modes)
(def next-declutter-mode labels/next-declutter-mode)
(def declutter-label labels/declutter-label)
(def needs-stagger? labels/needs-stagger?)
(def stagger-offset labels/stagger-offset)
(def apply-layer-stagger labels/apply-layer-stagger)

(def arrowhead-for layout/arrowhead-for)
(def track-width-for layout/track-width-for)
(def track-x-for layout/track-x-for)
(def dependency-pairs-by-layer layout/dependency-pairs-by-layer)
(def incoming-counts-by-layer layout/incoming-counts-by-layer)
(def edge-point layout/edge-point)
(def orientation layout/orientation)
(def segment-crosses? layout/segment-crosses?)
(def edge-cross? layout/edge-cross?)
(def assign-layer-slots layout/assign-layer-slots)
(def default-slot-scoring layout/default-slot-scoring)
(def build-scene layout/build-scene)

(def clamp-between routing/clamp-between)
(def rect-center routing/rect-center)
(def rect-edge-anchor routing/rect-edge-anchor)
(def layer-edge-drawables routing/layer-edge-drawables)
(def declutter-edge-drawables routing/declutter-edge-drawables)
(def apply-parallel-arrow-spacing routing/apply-parallel-arrow-spacing)

(defn- aggregate-layer-edges
  [scene module->layer]
  (routing/aggregate-layer-edges scene module->layer))

(defn- assign-lane
  [lanes edge]
  (routing/assign-lane lanes edge))

(defn- make-layer-base-edge
  [layer-rects layer->centers layer-pair type]
  (routing/make-layer-base-edge layer-rects layer->centers layer-pair type))
