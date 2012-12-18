;(ns test)

(use '(java.awt.Color) 
     '(java.awt.Image) 
     '(java.awt.image.BufferedImage))

(println (str "Hello" " " "Bob"))



(def theta 0.3)
(def primary 0)

(defn make-colour [a]
  (let [h (mod a 1)]
    (list
      (java.awt.Color/getHSBColor h 0.25 1)
      (java.awt.Color/getHSBColor h 0.75 1)
      (java.awt.Color/getHSBColor h 1 0.75)
      (java.awt.Color/getHSBColor h 1 0.25)
    )))

(defn invert [c]
  (new java.awt.Color (.getRed c) (.getGreen c) (.getBlue c)))

(def colours 
  (map make-colour (iterate (partial + theta) primary)))


(defn fmt [i]
  (let [a (first i) b (last i)]
  (str 
    "<span style=\"background: RGB(" 
    (.getRed a)
    ", "
    (.getBlue a)
    ", "
    (.getGreen a)
    "); \"> . . . . . . . . . . . </span>"

    "<span style=\"background: RGB(" 
    (.getRed b)
    ", "
    (.getBlue b)
    ", "
    (.getGreen b)
    "); \"> . . . . . . . . . . . </span>"
    "<br />" )))

(defn fmt-all [c]
  (apply str (map fmt (take 15 colours))))

(spit (new java.io.File "delme.html") (fmt-all colours))

; g.setFont(g.getFont().deriveFont(Font.BOLD, 25));

(defn new-img [x y] 
  (new java.awt.image.BufferedImage x y java.awt.image.BufferedImage/TYPE_INT_ARGB))

; TODO: measure actual render size of text and re-position accordingly
(defn draw-str [n cs max-x max-y] 
  (let [img (new-img max-x max-y)
        g (. img getGraphics)
        s (min max-x max-y)
        i (/ s 10)] 
      (doto g 
        (.setColor (nth cs 1))
        (.fillRect 0 0 max-x max-y)
        (.setColor (nth cs 3))
        (.setFont (.deriveFont (. g getFont) java.awt.Font/BOLD (float s)))
        (.drawString (str n) i (- max-y i)))
      img))

(def numerals 
  (map partial (repeat draw-str) (iterate inc 0)))

(def alphas 
  (map partial (repeat draw-str) (map char (iterate inc 65))))

(defn radians [n m] 
  (* (/ n m) (* 2 java.lang.Math/PI)))

(defn pol-ps [n, cx, cy]
  (map 
    #(identity 
      {:x (+ cx (* cx (java.lang.Math/sin (radians % n))))
       :y (+ cy (* cy (java.lang.Math/cos (radians % n)))) }) 
    (range n)))

(defn pol-rdr [{g :g pa :p} pb]
  (doto g (.drawLine (:x pa) (:y pa) (:x pb) (:y pb) )) 
  {:g g :p pb})

(defn draw-poly [n cs max-x max-y]
  (let [img (new-img max-x max-y)
        g (. img getGraphics)
        s (min max-x max-y)
        i (/ s 10)
        ps (pol-ps n (/ max-x 2) (/ max-y 2))] 
      (doto g 
        (.setColor (nth cs 1))
        (.fillRect 0 0 max-x max-y)
        (.setColor (nth cs 3)))
      (dorun (reduce pol-rdr {:g g :p (first ps)} (rest ps)))
  img))

(def polys 
  (map partial (repeat draw-poly) (iterate inc 2)))


(defn spit-img [img]
  (javax.imageio.ImageIO/write img "png" (new java.io.File "delme.png")))

;(def my-img ((first numerals) 50 50))

;(spit-img my-img)

; provide maximum dimensions
; return an image

(defn img-merge-v [a b] 
  (let [img (new-img (max (. a getWidth) (. b getWidth)) (+ (. a getHeight) (. b getHeight)))
        g (. img getGraphics)] 
       (doto g 
         (.drawImage a 0 0 nil)
         (.drawImage b 0 (. a getHeight) nil))
      img))

(defn img-merge-h [a b] 
  (let [img (new-img (+ (. a getWidth) (. b getWidth)) (max (. a getHeight) (. b getHeight)))
        g (. img getGraphics)] 
       (doto g 
         (.drawImage a 0 0 nil)
         (.drawImage b (. a getWidth) 0 nil))
      img))


(defn render-col [n mx my cs fs] 
  (reduce img-merge-v (map apply (take n fs) (repeat (list mx my)))))

(defn bob [n nms x y cs] 
  (reduce img-merge-v (map apply (take n nms) cs (repeat (list x y)))))

(defn babyshapes [n mx my cs & fss] 
  (reduce img-merge-h (map bob (repeat n) fss (repeat mx) (repeat my) (repeat cs))))

(spit-img (babyshapes 10 50 50 colours numerals alphas polys))
