;; Chapter 1
(defn atom? [x]
  (not (list? x)))

;; Chapter 2
(defn lat? [l]
  (cond
   (empty? l) true
   (atom? (first l)) (lat? (rest l))
   :else false))

(defn member? [a lat]
  (cond
   (empty? lat) false
   (= (first lat) a) true
   :else (member? a (rest lat))))


