(ns com.kaicode.morpheus.matrix)

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

(defn- with-index
  "return the sequence with an index for every element.
  For example: (with-index [:a :b :c]) returns ([0 :a] [1 :b] [2 :c]).

  The use case for this method ariises with you need access to the index of element of a sequence
  in a for or a doseq"
  [a-seq]
  (map-indexed (fn [i element] [i element])
               a-seq))

(defn dot-product [v1 v2]
  (let [v1-with-index (with-index v1)]
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
     :else (vec (for [[i a-row]  (with-index a)]
                  (vec (for [j (-> b first count range)
                             :let [b-column (column b j)]]
                         (dot-product a-row b-column)))))))
  ([a b & matrices]
   (let [r (into [(multiply a b)] matrices)]
     (reduce multiply r))))

(defn add [a b]
  (for [[i a-row] (with-index a)
        :let [b-row (nth b i)]]
    (for [[j a-val] (with-index a-row)
          :let [b-val (nth b-row j)]]
      (+  a-val b-val))))
