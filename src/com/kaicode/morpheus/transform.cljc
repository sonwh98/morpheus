(ns com.kaicode.morpheus.transform)

(defn skew-x [x]
  [ [1 (js/Math.tan x) 0]
    [0 1 0]
    [0 0 1]
    ])

(defn skew-y [y]
  [[1 0 0]
   [(js/Math.tan y) 1 0]
   [0 0 1]])

(defn skew [x y]
  (multiply (skew-x x)
            (skew-y y)))

(defn rotate [theta]
  [ [(js/Math.cos theta) (- (js/Math.sin theta)) 0]
    [(js/Math.sin theta) (js/Math.cos theta) 0]
    [0 0 1]])

(defn translate-x [x]
  [[1 0 x]
   [0 1 0]
   [0 0 1]])

(defn translate-y [y]
  [[1 0 0]
   [0 1 y]
   [0 0 1]])

(defn translate [x y]
  (multiply (translate-x x)
            (translate-y y)))

(defn scale-x [factor]
  [[factor 0 0]
   [0 1 0]
   [0 0 1]])

(defn scale-y [factor]
  [[1 0 0]
   [0 factor 0]
   [0 0 1]])

(defn scale [factor]
  (multiply (scale-x factor)
            (scale-y factor)))
