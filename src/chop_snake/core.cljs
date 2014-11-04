(ns chop-snake.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(defn equals-to [x] (fn [y] (= x y)))

(defn in? [x coll] (some (equals-to x) coll))

;; Grid Functions

(defn translate [point ray]
  (let [[x y] point
        [dx dy] ray]
    [(+ x dx) (+ y dy)]))

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

(defn game-view [game owner]
  (reify
    om/IInitState
    (init-state [_]
      {:game (SnakeGame. (square-grid 16 0)
                         `([4 3] [5 3])
                         `([1 3])
                         0
                         [1 0]
                         :alive)})
    om/IWillMount
    (will-mount [_]
        (js/setInterval (fn []
                          (om/update-state! owner
                                            :game
                                            step))
                        250))

    om/IRenderState
    (render-state [this state]
      (dom/div #js {:className "game"
                    :tabIndex "1"
                    :onKeyDown (fn [e]
                        (om/update-state! owner
                                          [:game :direction]
                                          (fn [direction]
                                            (cond (and (= (.-keyCode e) 38) (not= direction [0 1]))
                                                  [0 -1]

                                                  (and (= (.-keyCode e) 39) (not= direction [-1 0]))
                                                  [1 0]

                                                  (and (= (.-keyCode e) 40) (not= direction [0 -1]))
                                                  [0 1]

                                                  (and (= (.-keyCode e) 37) (not= direction [1 0]))
                                                  [-1 0]
                                                  
                                                  :else 
                                                  direction))))}
               (render-screen (draw-game (:game state)))))))

(om/root
  (fn [app owner]
    (reify om/IRender
      (render [_]
        (dom/div nil
                 (om/build game-view {})))))
  app-state
  {:target (. js/document (getElementById "app"))})
