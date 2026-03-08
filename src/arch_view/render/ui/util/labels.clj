(ns arch-view.render.ui.util.labels
  (:require [clojure.string :as str]))

(defn- strip-mixed-leaf-suffix
  [module]
  (if (and (string? module) (str/ends-with? module "|file"))
    (subs module 0 (- (count module) 5))
    module))

(defn- strip-source-extension
  [module]
  (if (string? module)
    (str/replace module #"\.(?:clj|cljc|cljs)$" "")
    module))

(defn abbreviate-module-name
  [module]
  (let [sanitized (strip-mixed-leaf-suffix module)
        no-ext (strip-source-extension sanitized)
        parts (->> (str/split no-ext #"\.")
                   (drop 1)
                   vec)
        parent (butlast parts)
        last-part (last parts)]
    (if (and (seq parent) last-part)
      (str (str/join "." (map #(subs % 0 1) parent))
           "."
           last-part)
      (or last-part no-ext))))

(defn strip-top-namespace
  [module]
  (let [parts (str/split module #"\.")]
    (if (> (count parts) 1)
      (str/join "." (rest parts))
      module)))

(defn label-width
  [label]
  (let [lines (str/split (or label "") #"\n" -1)
        widest (apply max 0 (map count lines))]
    (* 7.0 widest)))

(defn rendered-label
  [{:keys [display-label label]}]
  (or display-label label))

(def ^:private split-break-chars #{\. \- \_ \space})

(defn- split-candidate
  [label idx target max-chars]
  (let [ch (.charAt label idx)
        cut (if (contains? #{\- \_} ch) (inc idx) idx)
        n (count label)
        left (subs label 0 cut)
        right (-> (subs label cut n)
                  (str/replace #"^[\./]+" ""))
        ok? (and (<= (count left) max-chars)
                 (<= (count right) max-chars)
                 (not (str/blank? left))
                 (not (str/blank? right)))]
    (when ok?
      {:left left
       :right right
       :score (Math/abs (- idx target))})))

(defn- convenient-split
  [label max-chars]
  (let [n (count label)
        target (long (/ n 2))]
    (->> (range 1 (dec n))
         (keep (fn [idx]
                 (when (contains? split-break-chars (.charAt label idx))
                   (split-candidate label idx target max-chars))))
         (sort-by :score)
         first)))

(defn- fallback-split
  [label max-chars]
  (let [n (count label)
        fallback-cut (max 1 (min (dec n) max-chars))]
    {:left (subs label 0 fallback-cut)
     :right (subs label fallback-cut n)}))

(defn split-label-lines
  [label max-chars]
  (let [label (or label "")
        max-chars (long (or max-chars 0))
        n (count label)]
    (if (or (<= max-chars 0)
            (<= n max-chars))
      [label]
      (let [{:keys [left right]} (or (convenient-split label max-chars)
                                     (fallback-split label max-chars))]
        [(str/trim left) (str/trim right)]))))

(defn rendered-label-lines
  [{:keys [display-label label max-label-chars]}]
  (split-label-lines (or display-label label) max-label-chars))

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
