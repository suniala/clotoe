(ns clotoe.core
  (:require [reagent.core :as r]
            [reagent.debug :as d]))

(def game-state (r/atom {:player :white
                         :step   :place
                         :board  {:c00 :blank :c10 :blank
                                  :c01 :blank :c11 :blank}}))

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

(defn trans-place-pebble [cell-accessor]
  (fn [game-state]
    (assoc game-state :step :rotate
                      :board (assoc (:board game-state) cell-accessor (:player game-state)))))

(defn trans-rotate [direction]
  (fn [game-state]
    (let [next-player (if (= :white (:player game-state)) :black :white)
          next-board (if (= :left direction)
                       (rot-left (:board game-state))
                       (rot-right (:board game-state)))]
      (assoc game-state :player next-player
                        :step :place
                        :board next-board))))

(defn game-place-pebble [cell-accessor]
  (swap! game-state (trans-place-pebble cell-accessor)))

(defn game-rotate [direction]
  (swap! game-state (trans-rotate direction)))

(defn data-debug []
  [:div
   (d/prn @game-state)])

(defn cell [board cell-accessor step]
  (let [pebble (cell-accessor board)]
    [:div {:class    (str "cell")
           :on-click #(if (and (= :place step)
                               (= :blank pebble))
                        (game-place-pebble cell-accessor)
                        nil)}
     [:span (cond (= :white pebble) "w"
                  (= :black pebble) "b"
                  :else "-")]]))

(defn grid [board step]
  [:div {:class "grid"}
   [cell board :c00 step]
   [cell board :c10 step]
   [cell board :c01 step]
   [cell board :c11 step]])

(defn rotate [label direction step]
  [:input {:type     "button" :value label
           :on-click #(if (= :rotate step)
                        (game-rotate direction)
                        nil)}])

(defn turn-label [player step]
  [:div
   "Turn: " player " " step])

(defn simple-example []
  (let [step (:step @game-state)]
    [:div
     [turn-label (:player @game-state) step]
     [grid (:board @game-state) step]
     [rotate "<" :left step]
     [rotate ">" :right step]
     [data-debug]]))

(defn ^:export run []
  (r/render [simple-example]
            (js/document.getElementById "app")))
