(ns cljroguelike.core
  (:import
    (java.awt Color Dimension)
    (java.awt.event KeyListener)
    (java.awt.image BufferStrategy)
    (javax.swing JFrame JOptionPane JPanel)))

(import java.awt.Dimension)
(import java.awt.Toolkit)
(import java.awt.event.KeyListener)
(import javax.swing.JPanel)
(import javax.swing.JFrame)

(import java.awt.Color)
(import java.awt.Dimension)

(def *size* 500)
(def *t-width* 20)

(def keys-down (atom #{}))

;(defn map-generator [size]
;  (defn river-generator [size]
;    (defn helper [coll]
;      (let [[old-x old-y] (last coll)]
;        (if (or (= old-x 0) (= old-y 0) 
;                (= old-x (- size 1))
;                (= old-y (- size 1)))
;          nil
;
;          next-sq 
;    (let [start [(rand-int size) 0]]

(def map1
  [
    "01000000000000000000"
    "01000000000000000000"
    "00100000000000000000"
    "00010000000000000000"
    "00010000000000000000"
    "00001000000000000000"
    "00001000000000000000"
    "00001100000000000000"
    "00000100000001110000"
    "00000010001110000000"
    "00000001110000000000"
    "00000000100000000000"
    "00000000010000000000"
    "00000000010000000000"
    "00000000001000000000"
    "00000000001000000000"
    "00000000001100000000"
    "00000000000010000000"
    "00000000000000000000"
    "00000000000000000000"
    ])

(defn draw-tile [x-rel y-rel type gfx]
  (let [y-offs 25 x-offs 10]
    (.setColor gfx (if (= type \1) 
                     (java.awt.Color/BLUE)
                     (java.awt.Color/GREEN)))
    (.fillRect gfx (+ x-offs (* x-rel *t-width*))
                   (+ y-offs (* y-rel *t-width*))
                   *t-width* *t-width*)))

(defn render-map [gfx]
  (doseq [x (range (count map1))]
    (doseq [y (range (count (first map1)))]
      (let [ch (nth (nth map1 y) x)]
        (draw-tile x y ch gfx)))))

(defn render-game [gfx state]
  (.setColor gfx (java.awt.Color/BLACK))

  (.fillRect gfx 0 0 *size* *size*)
  (render-map gfx)

 
  ;(println (first state))
  (let [item (first state)]
    (draw-tile (:x item) (:y item) \1 gfx))

  (doseq [item state]
    (do 
      (draw-tile (:x item) (:y item) \1 gfx)))
  ;(for [item state]
  ;  (println (:x item) (:y item) "1" gfx))
  
  (for [item state]
    (draw-tile (:x item) (:y item) "1" gfx))
  )

(defn double-buffer-game [p state]
  (let [bfs (.getBufferStrategy p)
        gfx (.getDrawGraphics bfs)]
    (render-game gfx state)
    (.show bfs)
    (.sync (Toolkit/getDefaultToolkit))
))

(defn update-player [player]
  player)

(defmulti update (fn[x] (:type x)))

(defn in-bounds [x y]
  (and (< x (- (count map1) 1))
       (< y (- (count map1) 1))
       (> x 0)
       (> x 0)))

(defn wall [x y]
  (and (in-bounds x y)
       (= \1 (nth (nth map1 y) x))))

(defmethod update :player [player]
  (let [old-x (:x player)
        old-y (:y player)
        try-x (+ old-x (if (@keys-down 37) -1 0) (if (@keys-down 39) 1 0))
        try-y (+ old-y (if (@keys-down 38) -1 0) (if (@keys-down 40) 1 0))]
    (let [new-x (if (wall try-x try-y) old-x try-x)
          new-y (if (wall try-x try-y) old-y try-y)]
      {:y new-y
       :x new-x
       :type :player
      })))

(defmethod update :enemy [enemy]
  enemy)

(defn core-loop [frame state]
  (if (not (empty? @keys-down))
    (println @keys-down))

  (double-buffer-game frame state)
  (Thread/sleep 5)
  (.setTitle frame "HURF."))

(defn skeleton-loop [frame]
  (loop [state [{:type :player :x 0 :y 10}
                {:type :enemy  :x 0  :y 2}
                ]]
    (core-loop frame state)
    (recur (doall (map update state)))))

(defn -main[]
  (def panel
    (proxy [JPanel KeyListener] []
      (getPreferredSize [] (Dimension. *size* *size*))
      (keyPressed [e]
        (let [key-code (.getKeyCode e)]
          (swap! keys-down conj key-code)))
         
      (keyReleased [e]
        (let [key-code (.getKeyCode e)]
          (swap! keys-down disj key-code)))

      (keyTyped [e])))

  (doto panel
    (.setFocusable true)
    (.addKeyListener panel))

  (def frame (JFrame. "Test"))

  (doto frame
      (.add panel)
      (.pack)
      ; (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      ; This causes both SLIME and VimClojure to crash.
      (.createBufferStrategy 2)
      (.setVisible true))

  ; The bound-fn and future-call are necessary to have a working REPL that
  ; also can see println.

  ; (skeleton-loop frame))
  (skeleton-loop frame)
  )
  ;(def f (future-call (bound-fn [] (skeleton-loop frame)))))

(-main)

