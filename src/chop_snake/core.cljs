(ns chop-snake.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]))

;; Grid Functions

(defn translate [point ray]
  (let [[x y] point
        [dx dy] ray]
    [(+ x dx) (+ y dy)]))

(defn scale [ray scalar]
  (let [[dx dy] ray]
    [(* dx scalar)
     (* dy scalar)]))

(defn draw-square [mark grid point]
  (let [[x y] point] (assoc-in grid [y x] mark)))

(defn get-square [grid point]
  (let [[x y] point] (get-in grid [y x])))

;; Snake Game

(defrecord SnakeGame [world food snake energy direction status])

(def blank-mark 0)
(def food-mark 1)
(def snake-mark 2)

(defn snake-step [game]
  (let [{:keys [world food snake energy direction status]} game
        head (last snake)
        front (translate head direction)
        is-front? (fn [x] (= x front))]
    (cond (= status :dead)
            game
          (some is-front? food)
            (assoc game
                   :snake
                   (concat (rest snake) (list front))
                   :energy
                   (inc energy)
                   :food
                   (remove is-front? food))
          (or (some is-front? snake)
              (not (= blank-mark (get-square world front))))
            (assoc game :status :dead)
          :else
            (if (= energy 0)
              (assoc game :snake (concat (rest snake) (list front)))
              (assoc game :snake
                          (concat snake (list front))
                          :energy
                          (dec energy))))))

(defn snake-draw [game]
  (let [{:keys [world food snake]} game]
       (reduce (partial draw-square food-mark)
          (reduce (partial draw-square snake-mark)
                  world
                  snake)
          food)))

(defn snake-keyboard [game keypress]
  (update-in game
             [:direction]
             (fn [direction]
               (let [head (last (:snake game))
                     neck (last (butlast (:snake game)))
                     new-direction (cond (= keypress :up) [0 -1]
                                         (= keypress :right) [1 0]
                                         (= keypress :down) [0 1]
                                         (= keypress :left) [-1 0])]
                 (if (= neck (translate head new-direction))
                   direction
                   new-direction)))))

(enable-console-print!)

(def app-state (atom {}))

(defn pixel [value]
  (dom/div #js {:className (str "grid-square grid-color-" value)}
           ""))

(defn scan-line [row]
  (apply dom/div
         #js {:className "grid-row"}
         (map pixel row)))

(defn render-screen [buffer]
  (apply dom/div
         #js {:className "grid"}
         (map scan-line buffer)))

(defn square-grid [n x]
  (vec (repeat n
              (vec (repeat n x)))))

(defn parse-key-code [code]
  (cond (= code 38) :up
        (= code 39) :right
        (= code 40) :down
        (= code 37) :left))

(defn game-view [game owner]
  (reify
    om/IInitState
    (init-state [_]
      {:world-state (SnakeGame. (square-grid 16 0)
                         `([4 3] [5 3])
                         `([1 3])
                         0
                         [1 0]
                         :alive)
       :keyboard-input (chan)})
    om/IWillMount
    (will-mount [_]
        (go (loop []
              (let [keypress (<! (om/get-state owner :keyboard-input))]
                (om/update-state! owner
                                  :world-state
                                  (fn [world-state] (snake-keyboard world-state keypress)))
                (recur))))
        (js/setInterval (fn []
                          (om/update-state! owner :world-state snake-step))
                        250))

    om/IRenderState
    (render-state [this state]
      (dom/div #js {:className "game"
                    :tabIndex "0"
                    :onKeyDown (fn [e]
                                  (put! (:keyboard-input state)
                                        (parse-key-code (.-keyCode e))))}
               (if (= (get-in state [:world-state :status]) :dead)
                 (render-screen (square-grid 16 1))
                 (render-screen (snake-draw (:world-state state))))))))

(om/root
  (fn [app owner]
    (reify om/IRender
      (render [_]
        (dom/div nil
                 (om/build game-view {})))))
  app-state
  {:target (. js/document (getElementById "app"))})
