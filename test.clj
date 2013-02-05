(ns test)

(use '(java.awt.Color) 
     '(java.awt.Image) 
     '(java.awt.image.BufferedImage)
     '(java.awt.RenderingHints))


;; Colours

(def theta (/ (+ (* 26 1.5) 1) 26))
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

;; Util

(defn new-img 
  ([] (new-img 1 1))
  ([i] (new-img (. i getWidth) (. i getHeight)))
  ([x y] (new java.awt.image.BufferedImage x y java.awt.image.BufferedImage/TYPE_INT_ARGB)))

(defn gfx [img]
  (let [g (. img getGraphics)]
    (. g setRenderingHint java.awt.RenderingHints/KEY_ANTIALIASING java.awt.RenderingHints/VALUE_ANTIALIAS_ON)
    g))

;; g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

(defn spit-img [img]
  (javax.imageio.ImageIO/write img "png" (new java.io.File "delme.png")))

(defn long-tail [tail & items]
  (concat items (repeat tail)))

(defn align [a x]
  (with-meta x {:align a}))

(defn alignment [x]
  (:align (meta x)))

(defn align-right [x] (align "right" x))
(defn align-right? [x] (= (alignment x) "right"))


;; Text

(defn draw-str [n cs max-x max-y] 
  (let [txt (str n)
        tmp (. (new-img) getGraphics)
        f (.deriveFont (. tmp getFont) java.awt.Font/BOLD (float (* max-y 0.9)))
        fm (. tmp getFontMetrics f)
        y (- max-y (. fm getDescent))
        h (. fm getHeight)
        w (. fm stringWidth txt)
        indent (/ max-y 5)
        img-w (+ w (* 2 indent))
        img (new-img img-w max-y)
        g (gfx img)]
;;      (println (str "> " txt \. h \. f \. indent \. img-w))
      (doto g 
        (.setColor (nth cs 3))
        (.setFont f)
        (.drawString txt indent y))
      img))

(defn draw-str-seq [txt-seq]
  (map partial (repeat draw-str) txt-seq))

;; Text Sequences

(def numerals (align-right (draw-str-seq (iterate inc 0))))

(def uppers (draw-str-seq (map char (iterate inc 65))))
(def lowers (draw-str-seq (map char (iterate inc 97))))
(def letters 
  (draw-str-seq 
    (map str 
      (map char (iterate inc 65)) 
      (map char (iterate inc 97)))))

(def number-words 
  (draw-str-seq (long-tail " " 
    "Zero" "One" "Two" "Three" "Four" "Five" "Six" "Seven" "Eight" "Nine" "Ten"
    "Eleven" "Twelve" "Thirteen" "Fourteen" "Fifteen" "Sixteen" "Seventeen" "Eighteen" 
    "Nineteen" "Twenty" "Twenty-one" "Twenty-two" "Twenty-three" "Twenty-four" "Twenty-five")))

;; Roman numerals

(def rom-nums [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1])
(def rom-strs ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"])

(defn roman-loop [vals roms rem out] 
  (let [val (first vals)
        rom (first roms)]
    (cond 
      (not val) out
      (<= val rem) (recur vals roms (- rem val) (str out rom))
      :else (recur (rest vals) (rest roms) rem out))))

(defn to-roman [n] (roman-loop rom-nums rom-strs n ""))

(def romans (draw-str-seq (map to-roman (iterate inc 0))))

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
        g (gfx img)
        ps (poly-points n centre centre)
        poly (reduce poly-rdr (new java.awt.Polygon) ps)] 
      (doto g 
        (.setColor (nth cs 2))
        (.setStroke (java.awt.BasicStroke. 6))
        (.fillPolygon poly)
        (.setColor (nth cs 3))
        (.drawPolygon poly)
      )
  img))

(def no-ops (draw-str-seq (repeat " ")))

(def polys 
  (concat (take 3 no-ops) (map partial (repeat draw-poly) (iterate inc 3))))


;; Binary

(def powers-of-two
  (iterate (partial * 2) 1))

(defn count-bits [n]
  (loop [c 0 n n]
    (if (> 1 n)
      c
      (recur (inc c) (/ n 2)))))

(defn draw-bit [g n b space size] 
  (let [x (- (* space (+ b 1.5)) (/ size 2))
        y (- space (/ size 2))]
    (if (bit-test n b)
      (doto g (.fillRect x y size size) (.drawRect x y size size))
      (. g drawRect x y size size))))

(defn spin [img]
  (let [i (new-img img)
        g (gfx i)]
    (doto g 
      (.drawImage 
        img 
        (java.awt.geom.AffineTransform/getRotateInstance 
          (java.lang.Math/PI) 
          (/ (.getWidth i) 2) 
          (/ (.getHeight i) 2)) 
        nil))
    i))

(defn draw-binary [n cs max-x max-y]
  (let [bits (count-bits n)
        space (/ max-y 2)
        indent (* space 0.1)
        size (- space indent indent)
        w (* space (+ bits 2))
        img (new-img w max-y)
        g (gfx img)]
    (. g setColor (nth cs 3))
    (. g setStroke (java.awt.BasicStroke. 6))
    (dorun (map draw-bit (repeat g) (repeat n) (range bits) (repeat space) (repeat size)))
    (spin img)))

(def binaries
  (align-right (map partial (repeat draw-binary) (iterate inc 0))) )

;;  Dots

(defn draw-dots [n cs max-x max-y]
  (let [space (/ max-y 2)
        indent (* space 0.1)
        size (- space indent indent)
        w (* space (+ n 2))
        img (new-img w max-y)
        g (gfx img)]
    (. g setColor (nth cs 3))
    (doseq [i (range 1 (inc n))] 
      (. g fillArc (+ (* space i) indent) (+ (/ max-y 4) indent) size size 0 360))
    img))

(def dots
  (map partial (repeat draw-dots) (iterate inc 0)))


;;  Assembly

(defn merger-v [r]
  (fn [a b]
    (let [w (max (. a getWidth) (. b getWidth))
          h (+ (. a getHeight) (. b getHeight))
          img (new-img w h)
          g (. img getGraphics)
          x (if r #(- w (. % getWidth)) (fn [a] 0))] 
      (doto g 
        (.drawImage a (x a) 0 nil)
        (.drawImage b (x b) (. a getHeight) nil))
      img)))

(defn img-merge-v [a b] 
  (let [w (max (. a getWidth) (. b getWidth))
        h (+ (. a getHeight) (. b getHeight))
        img (new-img w h)
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

(defn bob [n nms x y cs r] 
  (reduce (merger-v (align-right? nms)) (map apply (take n nms) cs (repeat (list x y)))))

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
  (let [img (reduce img-merge-h 
              (map bob (repeat n) fss (repeat mx) (repeat my) (repeat cs) (cycle [true false])))
        back (filled-back img cs my)
        g (. back getGraphics)]
    (. g drawImage img 0 0 nil)
    back))

(spit-img (babyshapes 26 750 150 colours 
  numerals 
  letters 
  binaries 
  dots 
  polys
  number-words
  romans))


;; | || ||| |||| ++++ (with strikethrough on five)
;; Fibonacci?
;; Primes?
;; Some sort of fractal spirally thing??!!??!!??


