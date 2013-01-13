(ns test)

(use '(java.awt.Color) 
     '(java.awt.Image) 
     '(java.awt.image.BufferedImage))



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


(defn new-img 
  ([] (new-img 1 1))
  ([x y] (new java.awt.image.BufferedImage x y java.awt.image.BufferedImage/TYPE_INT_ARGB)))


;; Text

(defn draw-str [n cs max-x max-y] 
  (let [txt (str n)
        tmp (. (new-img) getGraphics)
        f (.deriveFont (. tmp getFont) java.awt.Font/BOLD (float (* max-y 0.9)))
        fm (. tmp getFontMetrics f)
        y (- max-y (. fm getDescent))
        h (. fm getHeight)
        w (. fm stringWidth txt)
        indent (/ max-y 10)
        img-w (min max-x (+ w (* 2 indent)))
        img (new-img img-w max-y)
        g (. img getGraphics)]
      (println (str "> " txt \. h \. f \. indent \. img-w))
      (doto g 
        (.setColor (nth cs 3))
        (.setFont f)
        (.drawString txt indent y))
      img))

(def numerals 
  (map partial (repeat draw-str) (iterate inc 0)))

(def alphas 
  (map partial (repeat draw-str) (map char (iterate inc 65))))


;; Polygons

(defn radians [n m] 
  (* (/ n m) (* 2 java.lang.Math/PI)))

(defn poly-points [n, cx, cy]
  (let [cxi (* 0.8 cx) cyi (* 0.8 cx)]
    (map 
      #(identity 
        {:x (+ cx (* cxi (java.lang.Math/sin (- (radians % n) (radians 0.5 n)))))
         :y (+ cy (* cyi (java.lang.Math/cos (- (radians % n) (radians 0.5 n))))) }) 
      (range n))))

(defn poly-rdr [poly point]
  (. poly addPoint (:x point) (:y point))
  poly)

(defn draw-poly [n cs max-x max-y]
  (let [size (min max-x max-y)
        centre (/ size 2)
        img (new-img size size)
        g (. img getGraphics)
        ps (poly-points n centre centre)
        poly (reduce poly-rdr (new java.awt.Polygon) ps)] 
      (doto g 
        (.setColor (nth cs 2))
        (.setStroke (java.awt.BasicStroke. 1))
        (.fillPolygon poly)
        (.setColor (nth cs 3))
        (.drawPolygon poly)
      )
  img))

(def polys 
  (map partial (repeat draw-poly) (iterate inc 0)))


;; Binary

(def powers-of-two
  (iterate (partial * 2) 1))

(defn count-bits [n]
  (loop [c 0 n n]
    (if (> 1 n)
      c
      (recur (inc c) (/ n 2)))))

(defn draw-bit [g n idx size max-y]
  (let [indent (* size 0.1)
        s (- size (* indent 2))
        x (+ (* idx size) indent)
        y (- (/ max-y 2) (/ s 2))]
    (. g setStroke (java.awt.BasicStroke. 3))
    (if (bit-test n idx)
      (doto g (.fillRect x y s s) (.drawRect x y s s))
      (. g drawRect x y s s)
    )
  )
)

(defn draw-bit2 [g n b space size] 
  (let [x (- (* space (+ b 1.5)) (/ size 2))
        y (- space (/ size 2))]
    (if (bit-test n b)
      (doto g (.fillRect x y size size) (.drawRect x y size size))
      (. g drawRect x y size size)
  )))

(defn draw-binary [n cs max-x max-y]
  (let [
        ;bits (count-bits n)
        ;bit-size (min max-y (/ max-x (max 1 bits)))
        ;img (new-img (* (max 1 bits) bit-size) max-y)
        ;g (. img getGraphics)

        bits (count-bits n)
        space (/ max-y 2)
        indent (* space 0.1)
        size (- space indent indent)
        w (* space (+ bits 2))
        img (new-img w max-y)
        g (. img getGraphics)]
    (. g setColor (nth cs 3))
    (. g setStroke (java.awt.BasicStroke. 3))
;    (dorun (map draw-bit (repeat g) (repeat n) (range bits) (repeat bit-size) (repeat max-y)))
    (dorun (map draw-bit2 (repeat g) (repeat n) (range bits) (repeat space) (repeat size)))
    img
  )
)

(def binaries
  (map partial (repeat draw-binary) (iterate inc 0)))


;;  Dots

(defn draw-dots [n cs max-x max-y]
  (let [space (/ max-y 2)
        indent (* space 0.1)
        size (- space indent indent)
        w (* space (+ n 2))
        img (new-img w max-y)
        g (. img getGraphics)]
    (. g setColor (nth cs 3))
    (doseq [i (range 1 (inc n))] 
      (. g fillArc (+ (* space i) indent) (+ (/ max-y 4) indent) size size 0 360))
    img))

(def dots
  (map partial (repeat draw-dots) (iterate inc 0)))

;;  Assembly


(defn spit-img [img]
  (javax.imageio.ImageIO/write img "png" (new java.io.File "delme.png")))

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

(defn filled-back [img cs each]
  (let [w (.getWidth img)
        h (.getHeight img)
        back (new-img w h)
        g (. back getGraphics)]
    (dorun (map 
      #(doto g 
        (.setColor (nth %2 1))
        (.fillRect 0 %1 w (+ %1 each)))
      (range 0 h each)
      cs)) 
    back))

(defn babyshapes [n mx my cs & fss] 
  (let [img (reduce img-merge-h (map bob (repeat n) fss (repeat mx) (repeat my) (repeat cs)))
        back (filled-back img cs my)
        g (. back getGraphics)]
    (. g drawImage img 0 0 nil)
    back))

(spit-img (babyshapes 26 750 150 colours numerals alphas polys binaries dots))
