(ns clotoe.core
  (:require [reagent.core :as r]
            [reagent.debug :as d]))

(def min-straight 5)

(defn init-quadrant []
  (let [cell-keys [:c00 :c10 :c20
                   :c01 :c11 :c21
                   :c02 :c12 :c22]
        cell-values (repeat (count cell-keys) :blank)]
    (zipmap cell-keys cell-values)))

(def game-state (r/atom {:player :white
                         :step   :intro
                         :board  {:q00 (init-quadrant)
                                  :q10 (init-quadrant)
                                  :q01 (init-quadrant)
                                  :q11 (init-quadrant)}}))

; TODO: seriously, this is awkward, how about creating this programmatically?
(def coord-to-cell-xs [[[:q00 :c00] [:q00 :c10] [:q00 :c20] [:q10 :c00] [:q10 :c10] [:q10 :c20]]
                       [[:q00 :c01] [:q00 :c11] [:q00 :c21] [:q10 :c01] [:q10 :c11] [:q10 :c21]]
                       [[:q00 :c02] [:q00 :c12] [:q00 :c22] [:q10 :c02] [:q10 :c12] [:q10 :c22]]
                       [[:q01 :c00] [:q01 :c10] [:q01 :c20] [:q11 :c00] [:q11 :c10] [:q11 :c20]]
                       [[:q01 :c01] [:q01 :c11] [:q01 :c21] [:q11 :c01] [:q11 :c11] [:q11 :c21]]
                       [[:q01 :c02] [:q01 :c12] [:q01 :c22] [:q11 :c02] [:q11 :c12] [:q11 :c22]]])

; Note that we only need "half" of possibilities here as directions (along the straight) does not matter.
(def straight-iterators
  (let [make-iter
        (fn [iter-col iter-row]
          (fn win-iter
            ([col row]
             (win-iter min-straight col row))
            ([rem col row]
             (if (or (= 0 rem)
                     (< col 0)
                     (> col 5)
                     (< row 0)
                     (> row 5))
               nil
               (cons [col row] (win-iter (dec rem) (iter-col col) (iter-row row)))))))
        no-op (fn [arg] arg)]
    [(make-iter inc no-op)                                  ; right
     (make-iter inc inc)                                    ; down-right
     (make-iter no-op inc)                                  ; down
     (make-iter dec inc)                                    ; down-left
     ]))

(defn find-player-cells [player]
  (let [player-cell-or-nil
        (fn [player col row]
          (let [[lookup-q lookup-c] (get (get coord-to-cell-xs row) col)
                pebble (lookup-c (lookup-q (:board @game-state)))]
            (if (= player pebble) [col row] nil)))
        nested-ranges (map (fn [a] (map (fn [b] [a b]) (range 0 6))) (range 0 6))
        flattened-ranges (apply concat nested-ranges)
        nillified-cells (map (fn [[col row]] (player-cell-or-nil player col row)) flattened-ranges)
        player-cells (filter (complement nil?) nillified-cells)]
    player-cells))

; Note that this could be optimized further but seems to be fast enough in practice.
(defn win? [player]
  (let [player-cells (set (find-player-cells player))
        possible-straights (apply concat (map
                                           (fn [straight-it] (map
                                                               (fn [[col row]] (straight-it col row))
                                                               player-cells))
                                           straight-iterators))
        long-enough-straights (filter #(>= (count %) min-straight) possible-straights)
        player-has-cell? (fn [cell] (contains? player-cells cell))
        player-has-straight? (fn [straight] (every? player-has-cell? straight))
        matching-straights (filter player-has-straight? long-enough-straights)]
    (not (empty? matching-straights))))

(defn board-full? []
  (let [quadrant-keys (keys (:board @game-state))
        q-has-blanks? (fn [q-key] (some #(= % :blank) (vals (q-key (:board @game-state)))))
        qs-with-blanks (filter q-has-blanks? quadrant-keys)]
    (empty? qs-with-blanks)))

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

(defn trans-win [player]
  (fn [game-state]
    (assoc game-state :step :end
                      :winner player)))

(defn trans-tie []
  (fn [game-state]
    (assoc game-state :step :end
                      :winner nil)))

(defn trans-start-game []
  (fn [game-state]
    (assoc game-state :step :place)))

(defn game-start []
  (swap! game-state (trans-start-game)))

(defn game-place-pebble [quadrant-accessor cell-accessor]
  (swap! game-state (trans-place-pebble quadrant-accessor cell-accessor)))

(defn game-rotate [quadrant-accessor direction]
  (swap! game-state (trans-rotate quadrant-accessor direction))
  (let [winner (cond (win? :white) :white
                     (win? :black) :black
                     :else nil)]
    (if winner
      (swap! game-state (trans-win winner))
      (if (board-full?)
        (swap! game-state (trans-tie))
        nil))))

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
  (let [hide-rotate-class (if (not (= :rotate step)) "hide-rotate" "")]
    [:div {:class (str "board " hide-rotate-class)}
     [board-quadrant board :q00 step]
     [board-quadrant board :q10 step]
     [board-quadrant board :q01 step]
     [board-quadrant board :q11 step]]))

(defn turn-label [player step winner]
  (let [step-text (if (= :place step)
                    "Place a pebble"
                    "Rotate a quadrant")
        end-text (if (not (nil? winner)) (str (name winner) " won!") "it is a tie.")]
    [:div {:class "turn"}
     (if (= :end step)
       [:div
        [:span (str "Game ended, " end-text)]]
       [:div
        [:span {:class "player"}
         "Player: " player]
        [:span {:class "step"}
         step-text]])
     ]))

(defn clotoe []
  (let [step (:step @game-state)
        start-button (fn []
                       [:input {:type     "button" :value "Start game"
                                :on-click #(game-start)}])]
    [:div {:class "content"}
     (if (= :intro step)
       [:div {:class "intro"}
        [:h1 "Clo-Toe"]
        [:a {:href "https://github.com/suniala/clotoe"}
         [:img {:style {:position "absolute", :top 0, :right 0, :border 0}
                :src   "https://s3.amazonaws.com/github/ribbons/forkme_right_gray_6d6d6d.png"
                :alt   "Fork me on GitHub"}]]
        [:p "This is a tic-tac-toe like game with a twist: after each turn, you must rotate one of the board
        quadrants 90 degrees. First player to get 5 in a row wins."]
        [:p "Now would be a good time to call a friend as this is a two player game!"]
        [start-button]]
       [:div
        [turn-label (:player @game-state) step (:winner @game-state)]
        [board-whole (:board @game-state) step]]
       )]))

(defn ^:export run []
  (r/render [clotoe]
            (js/document.getElementById "app")))
