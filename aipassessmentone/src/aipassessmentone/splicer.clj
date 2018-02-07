;Solution 1:
(def splice-out '(a b c d e)) ;defines a list "splice-out"
(concat ;changes vector into a list.
  (subvec (into [] splice-out) 1 3)) ;Returns a persistent vector of the items in vector fromstart (inclusive) to end (exclusive).


;Solution 2:
(defn easy-splice-out [lis n m]
  (drop n (take (+ m 1) lis)))
(easy-splice-out '(a b c d e) 1 3)


;Solution 3 Stage 1
;Our First Attempt Which Was Slightly wrong as it had too many brackets:
(defn splice-out-one [lis start end count1]
  (if (empty? lis)
    nil
    (if (and (>= count1 start) (<= count1 end))
      ((cons (first lis) (splice-out-one (rest lis) start end (+ count1 1))))
      ((cons nil (splice-out-one (rest lis) start end (+ count1 1)))))))


;Solution 3 Stage 2
; Removing the sets of brackets around the cons to get rid of the exception
; Will still be wrong as it producec (nil b c d nil)
(defn splice-out-two [lis start end count1]
  (if (empty? lis)
    nil
    (if (and (>= count1 start) (<= count1 end))
      (cons (first lis) (splice-out-two (rest lis) start end (+ count1 1)))
      (cons nil (splice-out-two (rest lis) start end (+ count1 1))))))


;Solution 3 Stage 3
;Removing the cons nil to skip the element instead of putting a nil in the new list
; This will now work
(defn splice-out-three [lis start end count1]
  (if (empty? lis)
    nil
    (if (and (>= count1 start) (<= count1 end))
      (cons (first lis) (splice-out-three (rest lis) start end (+ count1 1)))
      (splice-out-three (rest lis) start end (+ count1 1)))))


;Solution 3 Stage 4
; Making it a Variadic Functions adding extra arity
(defn splice-out-four
  ([lis start end] (splice-out-four lis start end 0))
  ([lis start end count1]
   (if (empty? lis)
     nil
     (if (and (>= count1 start) (<= count1 end))
       (cons (first lis) (splice-out-four (rest lis) start end (+ count1 1)))
       (splice-out-four (rest lis) start end (+ count1 1))))))

(splice-out-four '(a b c d e f g h i j k l m o n p q) 4 9 0)

;Solution 3 Stage 5
;Making it slightly more idiomatic (better more readable)
(defn splice-out-five
  ([lis start end] (splice-out-five lis start end 0))
  ([lis start end counter]
   (if (empty? lis)
     nil
     (if (<= start counter end)
       (cons (first lis) (splice-out-five (rest lis) start end (inc counter)))
       (splice-out-five (rest lis) start end (inc counter))))))

(splice-out-five '(a b c d e f g h i j k l m o n p q) 4 9 0)

;Solution 3 Stage 6
;; Currently it will keep transversing after the end, can fix this by adding a while in
(defn splice-out-six
  ([lis start end] (splice-out-six lis start end 0))
  ([lis start end counter]
   (when (<= counter end)
   (if (empty? lis)
     nil
     (if (<= start counter end)
       (cons (first lis) (splice-out-six (rest lis) start end (inc counter)))
       (splice-out-six (rest lis) start end (inc counter)))))))

(splice-out-six '(a b c d e f g h i j k l m o n p q) 4 9 0)




