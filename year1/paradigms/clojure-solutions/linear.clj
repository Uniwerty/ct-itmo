(defn v+ [v1 v2] (mapv + v1 v2))
(defn v- [v1 v2] (mapv - v1 v2))
(defn v* [v1 v2] (mapv * v1 v2))
(defn vd [v1 v2] (mapv / v1 v2))
(defn scalar [v1 v2] (apply + (v* v1 v2)))
(defn vect [v1 v2]
  (vector (- (* (first (rest v1)) (last v2))
             (* (last v1) (first (rest v2))))
          (- (* (last v1) (first v2))
             (* (first v1) (last v2)))
          (- (* (first v1) (first (rest v2)))
             (* (first (rest v1)) (first v2)))
          )
  )

(defn m+ [m1 m2] (mapv v+ m1 m2))
(defn m- [m1 m2] (mapv v- m1 m2))
(defn m* [m1 m2] (mapv v* m1 m2))
(defn md [m1 m2] (mapv vd m1 m2))
(defn v*s [v s] (mapv #(* %1 s) v))
(defn m*s [m s] (mapv #(v*s %1 s) m))

(defn transpose [m]
  (apply mapv vector m)
  )

(defn m*v [m v]
  (letfn [(mul [r m v]
            (if (empty? m)
              r
              (recur (conj r (apply + (v* (first m) v)))
                     (rest m)
                     v)
              )
            )]
    (mul [] m v)
    )
  )

(defn m*m [m1 m2]
  (letfn [(mul [r m1 m2]
            (if (empty? m2)
              r
              (recur (conj r (m*v m1 (first m2)))
                     m1
                     (rest m2))
              )
            )]
    (transpose (mul [] m1 (transpose m2)))
    )
  )

(defn s-op [f]
  (fn [v1 v2]
    (letfn [(sum [r v1 v2]
              (if (number? v1)
                (f v1 v2)
                (if (empty? v1)
                  r
                  (recur (conj r (sum [] (first v1) (first v2))) (rest v1) (rest v2))
                )
              ))]
      (sum [] v1 v2)
      )
    )
  )
(defn s+ [v1 v2] ((s-op +) v1 v2))
(defn s- [v1 v2] ((s-op -) v1 v2))
(defn s* [v1 v2] ((s-op *) v1 v2))
(defn sd [v1 v2] ((s-op /) v1 v2))