(ns arch-view.render.ui.util.labels
  (:require [clojure.string :as str]))

(defn abbreviate-module-name
  [module]
  (let [parts (->> (str/split module #"\.")
                   (drop 1)
                   vec)
        parent (butlast parts)
        last-part (last parts)]
    (if (and (seq parent) last-part)
      (str (str/join "." (map #(subs % 0 1) parent))
           "."
           last-part)
      (or last-part module))))

(defn strip-top-namespace
  [module]
  (let [parts (str/split module #"\.")]
    (if (> (count parts) 1)
      (str/join "." (rest parts))
      module)))

(defn label-width
  [label]
  (* 7.0 (count (or label ""))))

(defn rendered-label
  [{:keys [display-label label]}]
  (or display-label label))

(def declutter-modes [:all :abstract :concrete])

(defn next-declutter-mode
  [mode]
  (let [idx (.indexOf ^java.util.List declutter-modes (or mode :all))
        current (if (neg? idx) nil idx)]
    (if (nil? current)
      :all
      (let [next-idx (mod (inc current) (count declutter-modes))]
        (nth declutter-modes next-idx)))))

(defn declutter-label
  [mode]
  (case mode
    :abstract "View: Abstract"
    :concrete "View: Concrete"
    "View: All"))

(defn- overlap?
  [a b]
  (< (Math/abs (double (- (:x a) (:x b))))
     (/ (+ (label-width (rendered-label a))
           (label-width (rendered-label b)))
        2.0)))

(defn needs-stagger?
  [modules]
  (boolean
    (some true?
          (map overlap?
               modules
               (rest modules)))))

(defn stagger-offset
  [idx]
  (let [step 10.0]
    (cond
      (even? idx) 0.0
      (odd? idx) step)))

(defn apply-layer-stagger
  [modules]
  (if (needs-stagger? modules)
    (map-indexed (fn [idx m]
                   (update m :y + (stagger-offset idx)))
                 modules)
    modules))
