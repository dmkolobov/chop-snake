(ns chop-snake.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]))

(defn equals-to [x] (fn [y] (= x y)))

(defn in? [x coll] (some (equals-to x) coll))

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

(defn move-snake [snake position]
  (concat (rest snake) (list position)))

(defn grow-snake [snake position]
  (concat snake (list position)))

(defn step [game]
  (let [{world :world
         food :food
         snake :snake
         energy :energy
         direction :direction
         status :status} game
        head (last snake)
        front (translate head direction)]
    (cond (= status :dead)
            game
          (in? front food)
            (assoc game
                   :snake
                   (move-snake snake front)
                   :energy
                   (inc energy)
                   :food
                   (remove (equals-to front) food))
          (or (in? front snake) (not (= blank-mark (get-square world front))))
            (assoc game :status :dead)
          :else
            (if (= energy 0)
              (assoc game :snake (move-snake snake front))
              (assoc game :snake
                          (grow-snake snake front)
                          :energy
                          (dec energy))))))

(defn draw-game [game]
  (let [{:keys [world food snake]} game]
       (reduce (partial draw-square food-mark)
          (reduce (partial draw-square snake-mark)
                  world
                  snake)
          food)))

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
      {:game (SnakeGame. (square-grid 16 0)
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
                                  [:game :direction]
                                  (fn [direction]
                                    (let [new-direction (cond (= keypress :up)
                                                                [0 -1]
                                                              (= keypress :right)
                                                                [1 0]
                                                              (= keypress :down)
                                                                [0 1]
                                                              (= keypress :left)
                                                                [-1 0])
                                          snake (om/get-state owner [:game :snake])
                                          head (last snake)
                                          neck (last (butlast snake))]
                                      (if (= neck (translate head new-direction))
                                        direction
                                        new-direction))))
                (recur))))
        (js/setInterval (fn []
                          (om/update-state! owner :game step))
                        250))

    om/IRenderState
    (render-state [this state]
      (dom/div #js {:className "game scanlines"
                    :tabIndex "0"
                    :onKeyDown (fn [e]
                                  (put! (:keyboard-input state)
                                        (parse-key-code (.-keyCode e))))}
               (if (= (get-in state [:game :status]) :dead)
                 (render-screen (square-grid 16 1))
                 (render-screen (draw-game (:game state))))))))

(om/root
  (fn [app owner]
    (reify om/IRender
      (render [_]
        (dom/div nil
                 (om/build game-view {})))))
  app-state
  {:target (. js/document (getElementById "app"))})
