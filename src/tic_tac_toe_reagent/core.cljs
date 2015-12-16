(ns tic-tac-toe-reagent.core
  (:require [reagent.core :as r :refer [atom]]))

(enable-console-print!)

(defn new-board []
  (zipmap (range 1 10) (repeat nil)))

(defonce app-state (atom {:board (new-board)
                          :turn "x"}))

(def board-style {:width 300
                  :height 300
                  :background-image "url(img/board.png)"
                  :background-position "center center"
                  :background-size "cover"
                  :background-repeat "no-repeat"})

(defn draw-piece! [canvas v]
  (let [ctx   (.getContext canvas "2d")
        mark  (if (nil? v)
                ""
                v)
        color (if (= "o" v)
                "red"
                "black")]
    (set! (.-fillStyle ctx) color)
    (set! (.-fillAlign ctx) "center")
    (set! (.-font ctx) "48px sans-serif")
    (.fillText ctx mark 40 60)))

(def next-turn {"x" "o"
                "o" "x"})

(defn draw? [board]
  (every? (comp not nil?) (vals board)))

(defn- matches? [board positions]
  (let [board-vals       (map #(map board %) positions)
        column-matches-x (map (fn [column-vals] (every? #(= "x" %) column-vals))
                              board-vals)
        column-matches-o (map (fn [column-vals] (every? #(= "o" %) column-vals))
                              board-vals)]
    (or (some true? column-matches-x)
        (some true? column-matches-o))))

(defn horizontal? [board]
  (let [positions (partition 3 (range 1 10))]
    (matches? board positions)))

(defn vertical? [board]
  (let [positions [[1 4 7]
                   [2 5 8]
                   [3 6 9]]]
    (matches? board positions)))

(defn diagonal? [board]
  (let [positions [[1 5 9]
                   [3 5 7]]]
    (matches? board positions)))

(defn winner? [board]
  (or (horizontal? board)
      (vertical? board)
      (diagonal? board)))

(defn restart-game []
  (swap! app-state assoc :board (new-board))
  (swap! app-state assoc :turn (next-turn (:turn @app-state))))

(defn piece [position v]
  (r/create-class
   {:component-did-update
    (fn [comp _ _]
      (let [canvas (r/dom-node comp)]
        (draw-piece! canvas (next-turn (:turn @app-state)))))
    :reagent-render
    (fn [position v]
      [:canvas {:width 100 :height 100
                :on-click (fn [e]
                            (swap! app-state update-in
                                   [:board position]
                                   (fn [_] (:turn @app-state)))
                            (swap! app-state assoc
                                   :turn (next-turn (:turn @app-state))))}])}))

(defn pieces []
  [:div {:style board-style}
   (for [[position v] (:board @app-state)]
     ^{:key position} [piece position v])])

(defn draw-dialog []
  [:div
   [:h1 "Draw"]
   [:button {:on-click restart-game} "Restart"]])

(defn winner []
  [:div
   [:h1 (str "Player " (next-turn (:turn @app-state)) " wins!")]
   [:button {:on-click restart-game} "Restart"]])

(defn board []
  [:div
   [:h1 "Tic Tac Toe"]
   (cond
     (winner? (:board @app-state)) [winner]
     (draw? (:board @app-state)) [draw-dialog]
     :else [pieces])])

(r/render-component [board]
                    (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
