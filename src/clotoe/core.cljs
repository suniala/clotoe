(ns clotoe.core
  (:require [reagent.core :as r]
            [reagent.debug :as d]))

(defn init-quadrant []
  {:c00 :blank :c10 :blank
   :c01 :blank :c11 :blank})

(def game-state (r/atom {:player :white
                         :step   :place
                         :board  {:q00 (init-quadrant)
                                  :q10 (init-quadrant)
                                  :q01 (init-quadrant)
                                  :q11 (init-quadrant)}}))

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

(defn trans-place-pebble [quadrant-accessor cell-accessor]
  (fn [game-state]
    (let [quadrant (quadrant-accessor (:board game-state))
          next-quadrant (assoc quadrant cell-accessor (:player game-state))
          next-board (assoc (:board game-state) quadrant-accessor next-quadrant)]
      (assoc game-state :step :rotate
                        :board next-board))))

(defn trans-rotate [quadrant-accessor direction]
  (fn [game-state]
    (let [next-player (if (= :white (:player game-state)) :black :white)
          quadrant (quadrant-accessor (:board game-state))
          next-quadrant (if (= :left direction)
                       (rot-left quadrant)
                       (rot-right quadrant))
          next-board (assoc (:board game-state) quadrant-accessor next-quadrant)]
      (assoc game-state :player next-player
                        :step :place
                        :board next-board))))

(defn game-place-pebble [quadrant-accessor cell-accessor]
  (swap! game-state (trans-place-pebble quadrant-accessor cell-accessor)))

(defn game-rotate [quadrant-accessor direction]
  (swap! game-state (trans-rotate quadrant-accessor direction)))

(defn data-debug []
  [:div
   (d/prn @game-state)])

(defn cell [board quadrant-accessor cell-accessor step]
  (let [pebble (cell-accessor (quadrant-accessor board))]
    [:div {:class    (str "cell")
           :on-click #(if (and (= :place step)
                               (= :blank pebble))
                        (game-place-pebble quadrant-accessor cell-accessor)
                        nil)}
     [:span (cond (= :white pebble) "w"
                  (= :black pebble) "b"
                  :else "-")]]))

(defn rotate [quadrant-accessor label direction step]
  [:input {:type     "button" :value label
           :on-click #(if (= :rotate step)
                        (game-rotate quadrant-accessor direction)
                        nil)}])

(defn board-quadrant [board quadrant-accessor step]
  [:div {:class "grid"}
   [rotate quadrant-accessor "<" :left step]
   [rotate quadrant-accessor ">" :right step]
   [cell board quadrant-accessor :c00 step]
   [cell board quadrant-accessor :c10 step]
   [cell board quadrant-accessor :c01 step]
   [cell board quadrant-accessor :c11 step]])

(defn board-whole [board step]
  [:div
   [board-quadrant board :q00 step]
   [board-quadrant board :q01 step]
   [board-quadrant board :q10 step]
   [board-quadrant board :q11 step]])

(defn turn-label [player step]
  [:div
   "Turn: " player " " step])

(defn simple-example []
  (let [step (:step @game-state)]
    [:div
     [turn-label (:player @game-state) step]
     [board-whole (:board @game-state) step]
     [data-debug]]))

(defn ^:export run []
  (r/render [simple-example]
            (js/document.getElementById "app")))
