(ns clotoe.core
  (:require [reagent.core :as r]))

(def cell-xss [:c00 :c10 :c20
               :c01 :c11 :c21
               :c02 :c12 :c22])

(def quadrant-xss [:q00 :q10
                   :q01 :q11])

(defn init-quadrant []
  (let [cell-values (repeat (count cell-xss) :blank)]
    (zipmap cell-xss cell-values)))

(def game-state (r/atom {:player       :white
                         :step         :intro
                         :min-straight 2
                         :board        {:q00 (init-quadrant)
                                        :q10 (init-quadrant)
                                        :q01 (init-quadrant)
                                        :q11 (init-quadrant)}}))

(defn pebble-at-coord [board col row]
  (let [cell-xs-kw (fn [c r] (keyword (str \c c r)))
        [q-xs c-xs] (if (< col 3)
                      (if (< row 3)
                        [:q00 (cell-xs-kw col row)]
                        [:q01 (cell-xs-kw col (- row 3))]
                        )
                      (if (< row 3)
                        [:q10 (cell-xs-kw (- col 3) row)]
                        [:q11 (cell-xs-kw (- col 3) (- row 3))]))]
    (c-xs (q-xs board))))

; Note that we only need "half" of possibilities here as directions (along the straight) does not matter.
(defn straight-iterators [min-straight]
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
          (let [pebble (pebble-at-coord (:board @game-state) col row)]
            (if (= player pebble) [col row] nil)))
        nested-ranges (map (fn [a] (map (fn [b] [a b]) (range 0 6))) (range 0 6))
        flattened-ranges (apply concat nested-ranges)
        nillified-cells (map (fn [[col row]] (player-cell-or-nil player col row)) flattened-ranges)
        player-cells (filter (complement nil?) nillified-cells)]
    player-cells))

; Note that this could be optimized further but seems to be fast enough in practice.
(defn win? [player min-straight]
  (let [player-cells (set (find-player-cells player))
        straight-its (straight-iterators min-straight)
        possible-straights (apply concat (map
                                           (fn [straight-it] (map
                                                               (fn [[col row]] (straight-it col row))
                                                               player-cells))
                                           straight-its))
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

(defn trans-place-pebble [quadrant-xs cell-xs]
  (fn [game-state]
    (let [quadrant (quadrant-xs (:board game-state))
          next-quadrant (assoc quadrant cell-xs (:player game-state))
          next-board (assoc (:board game-state) quadrant-xs next-quadrant)]
      (assoc game-state :step :rotate
                        :board next-board))))

(defn trans-rotate [quadrant-xs direction]
  (fn [game-state]
    (let [next-player (if (= :white (:player game-state)) :black :white)
          quadrant (quadrant-xs (:board game-state))
          next-quadrant (if (= :left direction)
                          (trans-quadrant-rot-left quadrant)
                          (trans-quadrant-rot-right quadrant))
          next-board (assoc (:board game-state) quadrant-xs next-quadrant)]
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

(defn trans-start-game [min-straight]
  (fn [game-state]
    (assoc game-state :step :place
                      :min-straight min-straight)))

(defn game-start [min-straight]
  (swap! game-state (trans-start-game min-straight)))

(defn game-place-pebble [quadrant-xs cell-xs]
  (swap! game-state (trans-place-pebble quadrant-xs cell-xs)))

(defn game-rotate [quadrant-xs direction]
  (swap! game-state (trans-rotate quadrant-xs direction))
  (let [min-straight (:min-straight @game-state)
        winner (cond (win? :white min-straight) :white
                     (win? :black min-straight) :black
                     :else nil)]
    (if winner
      (swap! game-state (trans-win winner))
      (if (board-full?)
        (swap! game-state (trans-tie))
        nil))))

(defn cell [board quadrant-xs cell-xs step]
  (let [pebble (cell-xs (quadrant-xs board))
        pebble-class (cond (= :white pebble) "white"
                           (= :black pebble) "black"
                           :else "blank")]
    [:div {:class    (str "cell")
           :on-click #(if (and (= :place step)
                               (= :blank pebble))
                        (game-place-pebble quadrant-xs cell-xs)
                        nil)}
     [:div {:class (str "pebble" " " pebble-class)}]]))

(defn rotate [quadrant-xs direction step]
  (let [img-name (if (= :right direction) "right.png" "left.png")
        class (name direction)]
    [:div {:class (str "rotate " class)}
     [:img {:src      (str "img/" img-name)
            :on-click #(if (= :rotate step)
                         (game-rotate quadrant-xs direction)
                         nil)}]]))

(defn board-quadrant [board quadrant-xs step]
  [:div {:class (str "quadrant-container " (name quadrant-xs))}
   [:div {:class (str "quadrant")}
    [rotate quadrant-xs :left step]
    [rotate quadrant-xs :right step]
    (map (fn [cell-xs] [cell board quadrant-xs cell-xs step]) cell-xss)]])

(defn board-whole [board step]
  (let [hide-rotate-class (if (not (= :rotate step)) "hide-rotate" "")]
    [:div {:class (str "board " hide-rotate-class)}
     (map (fn [quadrant-xs] [board-quadrant board quadrant-xs step]) quadrant-xss)]))

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
        start-button (fn [min-straight]
                       [:input {:type     "button" :value (str "Start a game of " min-straight " in a row")
                                :on-click #(game-start min-straight)}])]
    [:div {:class "content"}
     (if (= :intro step)
       [:div {:class "intro"}
        [:h1 "Clo-Toe"]
        [:a {:href "https://github.com/suniala/clotoe"}
         [:img {:style {:position "absolute", :top 0, :right 0, :border 0}
                :src   "https://s3.amazonaws.com/github/ribbons/forkme_right_gray_6d6d6d.png"
                :alt   "Fork me on GitHub"}]]
        [:p "This is a tic-tac-toe like game with a twist: after each turn, you must rotate one of the board
        quadrants 90 degrees. First player to get enough pebbles in a row wins."]
        [:p "Now would be a good time to call a friend as this is a two player game!"]
        [start-button 3]
        [start-button 4]
        [start-button 5]]
       [:div
        [turn-label (:player @game-state) step (:winner @game-state)]
        [board-whole (:board @game-state) step]]
       )]))

(defn ^:export run []
  (r/render [clotoe]
            (js/document.getElementById "app")))
