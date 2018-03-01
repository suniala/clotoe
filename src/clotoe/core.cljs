(ns clotoe.core
  (:require [reagent.core :as r]))

(def grid-data (r/atom [[false false] [false true]]))

(defn set-pebble [col row]
  (fn [[[c00 c10] [c01 c11]]]
    ; TODO: set pebble
    [[c00 c10] [c01 c11]]))

(defn cell [data col row]
  [:div {:class (str "cell")
         :on-click #(swap! @data (set-pebble col row))}
   [:span (if ((@data row) col) "x" "o")]])

;(defn cell [data col row]
;  [:div {:class "cell"}
;   [:input {:type "checkbox" :checked ((@data row) col)}]])
;
(defn grid [data]
  [:div {:class "grid"}
   [cell data 0 0]
   [cell data 1 0]
   [cell data 0 1]
   [cell data 1 1]])

; c00 c10  ->  c01 c00
; c01 c11      c11 c10
(defn rot-right [[[c00 c10] [c01 c11]]]
  [[c01 c00] [c11 c10]])

; c00 c10  ->  c10 c11
; c01 c11      c00 c01
(defn rot-left [[[c00 c10] [c01 c11]]]
  [[c10 c11] [c00 c01]])

(defn rotate [label rot]
  [:input {:type     "button" :value label
           :on-click #(swap! grid-data rot)}])

(defn simple-example []
  [:div
   [grid grid-data]
   [rotate "<" rot-left]
   [rotate ">" rot-right]])

(defn ^:export run []
  (r/render [simple-example]
            (js/document.getElementById "app")))
