;Solution 1:
(def splice-out '(a b c d e)) ;defines a list "splice-out"
(concat ;changes vector into a list.
  (subvec (into [] splice-out) 1 3)) ;Returns a persistent vector of the items in vector fromstart (inclusive) to end (exclusive).


;Solution 2:
; Drop Returns a lazy sequence of all but the first n items in coll
;Take Returns a lazy sequence of the first n items in coll, or all items if there are fewer than n
(defn easy-splice-out [lis n m]
      (drop n (take (+ m 1) lis)))
(easy-splice-out '(a b c d e) 1 3)


;Solution 3 Stage 1
;Our First Attempt Which Was Slightly wrong as it had too many brackets around the cons.
; four arguments are passed in. a list, start and end indexes and a counter
; Our recursive ending, base case is if the list if empty produce and empty list (nil)
; if the count is greater than or equal to the start and less than or equal to the end condition
; if the count is inbetween these values it will add that element to a new list
; otherwise it will put a nil in it's place.
; We then recursivly call the function and add one to the counter and keep the start and end the same.
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

; Solution 3 Stage 7
; changed the if statement to a cond statement and added the else in as well
(defn splice-out-seven
      ([lis start end] (splice-out-seven lis start end 0))
      ([lis start end counter]
        (cond
          (empty? lis) nil
          (> start end) nil
          (<= start counter end) (cons (first lis) (splice-out-seven (rest lis) start end (inc counter)))
          :else (splice-out-seven (rest lis) start end (inc counter)))))

(splice-out-seven '(a b c d e f g h i j k l m o n p q) 1 3 0)
(splice-out-seven '(a b c d e f g h i j k l m o n p q) 1 30)




