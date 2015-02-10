(ns game-of-life-cljs.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require
   [cljs.core.async :refer [<! timeout]]
   [figwheel.client :as fw]))

(enable-console-print!)

(def reset? (atom false))
(def browser-height (.-height (.getElementById js/document "my-canvas")) )
(def browser-width (.-width (.getElementById js/document "my-canvas")))
(def x-len 10)
(def y-len 10)
(def num-x-squares (/ browser-width x-len))
(def num-y-squares (/ browser-height y-len))
(def false-color  ["#669999" "#407F7F"])
(def true-color ["#801515" "#550000"])
;; (def seed [[2, 1] [3, 1] [5, 1] [6, 1]
;;            [3, 2] [5, 2] [3, 3] [5, 3]
;;            [2, 4] [3, 4] [5, 4] [6, 4]])
(def seed-1 [[15 14] [15 15] [16 16] [15 16] [17 17] [18 18] [19 19] [20 20]])
(def seed-2 [[25 24] [25 25] [26 26] [25 26] [27 27] [28 28] [29 29] [30 30]])
(def seed-3 [[25 24] [25 25] [26 26] [25 26] [27 27] [28 28] [29 29] [30 30]])
(def seed seed-3)

(defn get-color [state]
  (let [palette (if (= true state) true-color false-color)]
    (nth palette (rand-int 2))))

;; define your app data so that it doesn't get over-written on reload
;; (defonce app-data (atom {}))

(defn draw-square [{:keys [x-pos y-pos state]}]
  (let [target (.getElementById js/document "my-canvas")
        context (.getContext target "2d")]
    (set! (.-fillStyle context) (get-color state))
    (.fillRect context x-pos y-pos x-len y-len)))

(defn create-empty-data []
  (let [board-width (.-width (.getElementById js/document "my-canvas"))
        board-height (.-height (.getElementById js/document "my-canvas"))
        squares-x (/ board-width x-len)
        squares-y (/ board-height y-len)]
    (into [] (for [y (range 0 browser-width y-len)]
               (into [] (for [x (range 0 browser-height x-len)]
                          (hash-map :x-pos x :y-pos y :state false)))))))

(defn seed-board [board seed]
    (reduce (fn [w [x y]]
              (let [col (get w y)]
                (assoc w y (assoc-in col [x :state] true))))
            board seed))

(defn cell [[x y] w]
  (if (or (< x 0)
          (< y 0)
          (>= x num-x-squares)
          (>= y num-y-squares))
    0
    (get-in w [y x :state])))

(defn neighbors [[x y] w]
  (apply + (map #(cell % w)
                [[x (inc y)] [x (dec y)]
                 [(inc x) y] [(dec x) y]
                 [(dec x) (dec y)] [(inc x) (inc y)]
                 [(inc x) (dec y)] [(dec x) (inc y)]])))

(defn evo-cell [[x y :as v] w]
  (let [alive? (= true (cell v w))
        neighbors (neighbors v w)
        existing (get-in w [y x])
        new-state 
        (cond
         (and alive? (< neighbors 2)) false
         (and alive? (> neighbors 3)) false
         (and alive? (or (= neighbors 2)
                         (= neighbors 3))) true
         (and (not alive?) (= neighbors 3)) true
         :else false)]
    (assoc existing :state new-state)))

(defn evo-world [w]
  (into [] (for [y (range num-y-squares)]
             (into [] (for [x (range num-x-squares)]
                        (evo-cell [x y] w))))))

(defn gen-n [w n]
  (if (zero? n)
    w
    (recur (evo-world w) (dec n))))

(defn draw-board [data]
  (doall
   (map draw-square
        (flatten data))))

(def board
  (atom (let [init (seed-board
                    (create-empty-data)
                    seed)]
          (draw-board init)
          init
          #_(println (evo-world @board)))))

(defn draw-loop [init n pause]
  (go-loop [init init n n]
    (if (or (zero? n)
            (true? @reset?))
      (do
        (reset! reset? false)
        (println "die!")
        nil)
      (do 
        (println "drawing")
        (<! (timeout pause))
        (draw-board init)
        (recur (evo-world init) (dec n))))))

(fw/start {
           :on-jsload (fn [] (do
                               (print "reloaded")
                               (reset! reset? true)))
           })

(draw-loop @board 10000 5)

;; (go (while true
;;       (. js/console (log "tick"))
;;       (let [c (chan)]
;;         )
;;       (<! (timeout 100000))
;;       (. js/console (log "?"))))


