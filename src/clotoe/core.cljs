(ns clotoe.core
  (:require [reagent.core :as r]))

(def grid-data (r/atom (array-map :c00 false :c10 false
                                  :c01 false :c11 true)))

(defn set-pebble [cell-data]
  (fn [data]
    (assoc data cell-data true)))

(defn cell [data cell-data]
  [:div {:class (str "cell")
         :on-click #(swap! data (set-pebble cell-data))}
   [:span (if (cell-data @data) "x" "o")]])

(defn grid [data]
  [:div {:class "grid"}
   [cell data :c00]
   [cell data :c10]
   [cell data :c01]
   [cell data :c11]])

; c00 c10  ->  c01 c00
; c01 c11      c11 c10
(defn rot-right [data]
  (array-map :c00 (:c01 data) :c10 (:c00 data)
             :c01 (:c11 data) :c11 (:c10 data)))

; c00 c10  ->  c10 c11
; c01 c11      c00 c01
(defn rot-left [data]
  (array-map :c00 (:c10 data) :c10 (:c11 data)
             :c01 (:c00 data) :c11 (:c01 data)))

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
