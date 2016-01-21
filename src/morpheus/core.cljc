(ns morpheus.core
  (:require [food-truck.util :as util]))

(defonce zero-vector [0 0 0])
(defonce zero-matrix [zero-vector
                      zero-vector
                      zero-vector])
(defonce identity-matrix [[1 0 0]
                          [0 1 0]
                          [0 0 1]])

(defn row
  "i th row of a matrix m"
  [m i]
  (nth m i))

(defn column
  "j th row of a matrix m"
  [m j]
  (mapv #(nth % j) m))

(defn dot-product [v1 v2]
  (let [v1-with-index (util/with-index v1)]
    (reduce + (for [[i v1-ith-element] v1-with-index
                    :let [v2-ith-element (nth v2 i)]]
                (* v1-ith-element v2-ith-element)))))

(defn multiply
  ([a] a)
  ([a b]
   (cond
     (and (number? a) (vector? b)) (vec (for [row b]
                                          (mapv #(* a %) row)))
     (and (number? b) (vector? a)) (multiply b a)
     :else (vec (for [[i a-row]  (util/with-index a)]
                  (vec (for [j (-> b first count range)
                             :let [b-column (column b j)]]
                         (dot-product a-row b-column)))))))
  ([a b & matrices]
   (let [r (into [(multiply a b)] matrices)]
     (reduce multiply r))))

(defn add [a b]
  (for [[i a-row] (util/with-index a)
        :let [b-row (nth b i)]]
    (for [[j a-val] (util/with-index a-row)
          :let [b-val (nth b-row j)]]
      (+  a-val b-val))))

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
