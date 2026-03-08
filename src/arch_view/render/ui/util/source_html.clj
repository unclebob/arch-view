(ns arch-view.render.ui.util.source-html
  (:require [clojure.string :as str])
  (:import [java.util.regex Pattern]))

(defn html-escape
  [s]
  (-> (or s "")
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

(defn colorize-clojure-html
  [source]
  (let [split-comment (fn [line]
                        (loop [idx 0
                               in-string? false
                               escaped? false]
                          (if (>= idx (count line))
                            [line nil]
                            (let [ch (.charAt line idx)]
                              (cond
                                (and (not in-string?) (= ch \;))
                                [(subs line 0 idx) (subs line idx)]

                                escaped?
                                (recur (inc idx) in-string? false)

                                (and in-string? (= ch \\))
                                (recur (inc idx) in-string? true)

                                (= ch \")
                                (recur (inc idx) (not in-string?) false)

                                :else
                                (recur (inc idx) in-string? false))))))
        [code-part comment-part] (split-comment (or source ""))
        escaped (html-escape code-part)
        string-pattern (Pattern/compile "\"([^\"\\\\]|\\\\.)*\"")
        keyword-pattern (Pattern/compile ":[a-zA-Z0-9\\-\\?!_\\./]+")
        apply-style (fn [text pattern class-name]
                      (let [matcher (.matcher pattern text)
                            sb (StringBuffer.)]
                        (loop []
                          (if (.find matcher)
                            (do
                              (.appendReplacement matcher sb (str "<span class='" class-name "'>$0</span>"))
                              (recur))
                            (do
                              (.appendTail matcher sb)
                              (str sb))))))]
    (str (-> escaped
             (apply-style string-pattern "str")
             (apply-style keyword-pattern "kw"))
         (when comment-part
           (str "<span class='cmt'>" (html-escape comment-part) "</span>")))))

(defn expand-tabs
  [line]
  (str/replace (or line "") "\t" "  "))

(defn source-lines->html
  [source]
  (let [lines (str/split (or source "") #"\r?\n" -1)]
    (->> lines
         (map-indexed (fn [idx line]
                        (let [line-html (colorize-clojure-html (expand-tabs line))
                              visible-line (if (str/blank? line-html) "&nbsp;" line-html)]
                          (str "<tr>"
                               "<td class='ln'>" (inc idx) "</td>"
                               "<td class='code'><pre>" visible-line "</pre></td>"
                               "</tr>"))))
         (apply str))))

(defn source->html
  [title source]
  (str "<html><head><style>"
       "body{margin:0;padding:0;background:#f8fafc;color:#111827;font-family:Menlo,Monaco,Consolas,monospace;}"
       ".hdr{padding:10px 12px;background:#e5e7eb;border-bottom:1px solid #cbd5e1;font-family:sans-serif;font-size:13px;}"
       ".src{padding:0;line-height:1.35;font-size:13px;}"
       ".src table{border-collapse:collapse;width:100%;}"
       ".ln{width:52px;padding:0 10px;background:#eef2f7;color:#6b7280;text-align:right;vertical-align:top;"
       "border-right:1px solid #d1d5db;user-select:none;}"
       ".code{padding:0 12px;vertical-align:top;}"
       ".code pre{margin:0;white-space:pre;tab-size:2;}"
       ".cmt{color:#6b7280;}"
       ".str{color:#b45309;}"
       ".kw{color:#1d4ed8;}"
       "</style></head><body>"
       "<div class='hdr'>" (html-escape title) "</div>"
       "<div class='src'><table>" (source-lines->html source) "</table></div>"
       "</body></html>"))
