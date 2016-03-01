(ns clj-priority-queue.core)

(defn create-priority-queue
  []
  (list))

(defn rank
  [[x r c]]
  r)

(defn root
  [[x r c]]
  x)

(defn link
  [[x1 r1 c1 :as t1] [x2 r2 c2 :as t2]]
  (if (< (compare x1 x2) 0)
    (list x1 (+ r1 1) (cons t2 c1))
    (list x2 (+ r2 1) (cons t1 c2))))

(defn ins
  [t ts]
  (if (empty? ts)
    (list t)
    (let [t* (first ts)
          ts* (rest ts)]
      (if (< (rank t) (rank t*))
        (cons t (cons t* ts*))
        (recur (link t t*) ts*)))))

(defn insert
  [pq x]
  (ins (list x 0 (list)) pq))

(defn find-min
  [pq]
  (cond
    (empty? pq) nil
    (= 1 (count pq)) (root (first pq))
    true
      (let [t (first pq)
            rx (root t)
            ts (rest pq)
            x (find-min ts)]
        (if (< (compare rx x) 0)
          rx
          x))))

(defn meld
  [ts1 ts2]
  (cond
    (empty? ts1) ts2
    (empty? ts2) ts1
    true
      (let [t1 (first ts1)
            ts1* (rest ts1)
            t2 (first ts2)
            ts2* (rest ts2)]
        (cond
          (< (rank t1) (rank t2)) (cons t1 (meld ts1* ts2))
          (< (rank t2) (rank t1)) (cons t2 (meld ts1 ts2*))
          true (ins (link t1 t2) (meld ts1* ts2*))))))

(defn get-min
  [ts]
  (cond 
    (= 1 (count ts)) [(first ts), ()]
    true (let [t* (first ts)
               ts* (rest ts)
               [t** ts**] (get-min ts*)]
           (if (< (compare (root t*) (root t**)) 0)
             [t* ts*]
             [t** (cons t* ts**)]))))
         

(defn delete-min
  [pq]
  (if (empty? pq)
    nil
    (let [[[x r c] ts] (get-min pq)]
      (meld (reverse c) ts))))


(defn push
  [pq item]
  (insert pq item))

(defn get-next
  [pq]
  (find-min pq))

(defn clear-next
  [pq]
  (delete-min pq))


;(defn push
;  [pm [item t priority :as k]]
;  (if-let [[v i l r] pm]
;    (if (< 0 (compare v priority))
;      (make-node v i (push l k) r)
;      (make-node v i l (push r k)))
;    (make-node priority [priority t item] nil nil)))
;
;(defn get-next
;  [pm]
;  (if-let [[v i l r] pm]
;;    (if (nil? l)
;      i
;      (recur l))))
;
;(defn clear-next
;  [pm]
;  (if-let [[v i l r] pm]
;    (if-let [vv ii ll rr] r)
;      (make-node v i l (recur r))
;      (make-node v i l nil)
;    (if (nil? r)
;      (i)
;      (recur r)))
;  (rest pm))
