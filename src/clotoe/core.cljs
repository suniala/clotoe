(ns clotoe.core
  (:require [reagent.core :as r]
            [reagent.debug :as d]))

(defn init-quadrant []
  (let [cell-keys [:c00 :c10 :c20
                   :c01 :c11 :c21
                   :c02 :c12 :c22]
        cell-values (repeat (count cell-keys) :blank)]
    (zipmap cell-keys cell-values)))

(def game-state (r/atom {:player :white
                         :step   :place
                         :board  {:q00 (init-quadrant)
                                  :q10 (init-quadrant)
                                  :q01 (init-quadrant)
                                  :q11 (init-quadrant)}}))

(def lookup-rot-right {:c00 :c02, :c10 :c01, :c20 :c00
                       :c01 :c12, :c11 :c11, :c21 :c10
                       :c02 :c22, :c12 :c21, :c22 :c20})

(def lookup-rot-left
  (apply hash-map (flatten (map (fn [k] [(k lookup-rot-right) k]) (keys lookup-rot-right)))))

(defn trans-quadrant-rot [quadrant lookup]
  (let [xs-pairs (map (fn [k] [k (k lookup)]) (keys lookup))
        pairs (map (fn [[k xs]] [k (xs quadrant)]) xs-pairs)]
    (apply hash-map (flatten pairs))))

(defn trans-quadrant-rot-right [quadrant]
  (trans-quadrant-rot quadrant lookup-rot-right))

(defn trans-quadrant-rot-left [quadrant]
  (trans-quadrant-rot quadrant lookup-rot-left))

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
                          (trans-quadrant-rot-left quadrant)
                          (trans-quadrant-rot-right quadrant))
          next-board (assoc (:board game-state) quadrant-accessor next-quadrant)]
      (assoc game-state :player next-player
                        :step :place
                        :board next-board))))

(defn game-place-pebble [quadrant-accessor cell-accessor]
  (swap! game-state (trans-place-pebble quadrant-accessor cell-accessor)))

(defn game-rotate [quadrant-accessor direction]
  (swap! game-state (trans-rotate quadrant-accessor direction)))

(defn cell [board quadrant-accessor cell-accessor step]
  (let [pebble (cell-accessor (quadrant-accessor board))
        pebble-class (cond (= :white pebble) "white"
                           (= :black pebble) "black"
                           :else "blank")]
    [:div {:class    (str "cell")
           :on-click #(if (and (= :place step)
                               (= :blank pebble))
                        (game-place-pebble quadrant-accessor cell-accessor)
                        nil)}
     [:div {:class (str "pebble" " " pebble-class)}]]))

(defn rotate [quadrant-accessor direction step]
  (let [img-name (if (= :right direction) "right.png" "left.png")
        class (name direction)]
    [:div {:class (str "rotate " class)}
     [:img {:src      (str "img/" img-name)
            :on-click #(if (= :rotate step)
                         (game-rotate quadrant-accessor direction)
                         nil)}]]))

(defn board-quadrant [board quadrant-accessor step]
  [:div {:class (str "quadrant-container " (name quadrant-accessor))}
   [:div {:class (str "quadrant")}
    [rotate quadrant-accessor :left step]
    [rotate quadrant-accessor :right step]
    [cell board quadrant-accessor :c00 step]
    [cell board quadrant-accessor :c10 step]
    [cell board quadrant-accessor :c20 step]
    [cell board quadrant-accessor :c01 step]
    [cell board quadrant-accessor :c11 step]
    [cell board quadrant-accessor :c21 step]
    [cell board quadrant-accessor :c02 step]
    [cell board quadrant-accessor :c12 step]
    [cell board quadrant-accessor :c22 step]]])

(defn board-whole [board step]
  (let [hide-rotate-class (if (= :place step) "hide-rotate" "")]
    [:div {:class (str "board " hide-rotate-class)}
     [board-quadrant board :q00 step]
     [board-quadrant board :q10 step]
     [board-quadrant board :q01 step]
     [board-quadrant board :q11 step]]))

(defn turn-label [player step]
  (let [step-text (if (= :place step)
                    "Place a pebble"
                    "Rotate a quadrant")]
    [:div {:class "turn"}
     [:span {:class "player"}
      "Player: " player]
     [:span {:class "step"}
      step-text]]))

(defn simple-example []
  (let [step (:step @game-state)]
    [:div {:class "content"}
     [turn-label (:player @game-state) step]
     [board-whole (:board @game-state) step]]))

(defn ^:export run []
  (r/render [simple-example]
            (js/document.getElementById "app")))
