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

;; Chapter 3
(defn rember [a lat]
  (cond
   (empty? lat) lat
   (= (first lat) a) (rest lat)
   :else (cons (first lat) (rember a (rest lat)))))

(defn firsts [l]
  (cond
   (empty? l) l
   :else (cons (first (first l)) (firsts (rest l)))))

(defn insertR [new old lat]
  (cond
   (empty? lat) lat
   (= (first lat) old) (cons old (cons new (rest lat)))
   :else (cons (first lat) (insertR new old (rest lat)))))

(defn insertL [new old lat]
  (cond
   (empty? lat) lat
   (= (first lat) old) (cons new lat)
   :else (cons (first lat) (insertL new old (rest lat)))))

(defn subst [new old lat]
  (cond
   (empty? lat) lat
   (= (first lat) old) (cons new (rest lat))
   :else (cons (first lat) (subst new old (rest lat)))))

(defn subst2 [new old1 old2 lat]
  (cond
   (empty? lat) lat
   (or (= old1 (first lat)) (= old2 (first lat))) (cons new (rest lat))
   :else (cons (first lat) (subst2 new old1 old2 (rest lat)))))

(defn multirember [a lat]
  (cond
   (empty? lat) lat
   (= a (first lat)) (multirember a (rest lat))
   :else (cons (first lat) (multirember a (rest lat)))))

(defn multiinsertR [new old lat]
  (cond
   (empty? lat) lat
   (= old (first lat)) (cons old (cons new (multiinsertR new old (rest lat))))
   :else (cons (first lat) (multiinsertR new old (rest lat)))))

(defn multiinsertL [new old lat]
  (cond
   (empty? lat) lat
   (= old (first lat)) (cons new (cons old (multiinsertL new old (rest lat))))
   :else (cons (first lat) (multiinsertL new old (rest lat)))))

(defn multisubst [new old lat]
  (cond
   (empty? lat) lat
   (= old (first lat)) (cons new (multisubst new old (rest lat)))
   :else (cons (first lat) (multisubst new old (rest lat)))))
