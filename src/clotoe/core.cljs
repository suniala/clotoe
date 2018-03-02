(ns clotoe.core
  (:require [reagent.core :as r]
            [reagent.debug :as d]))

(def turn (r/atom {:color :white
                   :step  :place}))

(def grid-data (r/atom (array-map :c00 :blank :c10 :blank
                                  :c01 :blank :c11 :blank)))

(defn data-debug []
  [:div
   (d/prn @turn)
   (d/prn @grid-data)])

(defn next-step [current]
  (if (= :place (:step current))
    (assoc current :step :rotate)
    (assoc current :color (if (= :white (:color current)) :black :white)
                   :step :place)))

(defn set-pebble [cell-data]
  (fn [data]
    (let [curr-turn @turn]
      (assoc data cell-data (:color curr-turn)))))

(defn cell [data cell-data]
  [:div {:class    (str "cell")
         :on-click #((swap! data (set-pebble cell-data))
                      (swap! turn next-step))}
   [:span (let [pebble (cell-data @data)]
            (cond (= :white pebble) "w"
                  (= :black pebble) "b"
                  :else "-"))]])

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
           :on-click #((swap! grid-data rot)
                        (swap! turn next-step))}])

(defn turn-label []
  [:div
   "Turn: "
   @turn])

(defn simple-example []
  [:div
   [turn-label]
   [grid grid-data]
   [rotate "<" rot-left]
   [rotate ">" rot-right]
   [data-debug]])

(defn ^:export run []
  (r/render [simple-example]
            (js/document.getElementById "app")))
