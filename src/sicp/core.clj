(ns sicp.core
  (:gen-class))

;; TODO
;; Exercise 1.22 - 28
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;Ben Bitdiddle test for dtermine whether applicative order or normal order
(defn p [] (p))

;Comment out as it would cause error
;(p)

(defn order-test [x y]
  (if (= x 0)
      0
      y))

(defn cond-order-test [x y]
  (cond (= x 0) 0
        :else (cond-order-test 0 y)))

;(cond-order-test 0 (p))
;(order-test 0 (p))

; The test could help to determine if the language is using applicative ord
; Applicative: StackOverFlowError, since we evaluate arg y first
; Normal: return 0, since we expand all then evaluate, we would never call (p)
; Thus clojure is applicative and evaluate all arg first?: 

; Exercise 1.6
(defn square [x]
  (* x x))

(defn abs [x]
  (cond
    (< x 0) (- 0 x)
    :else x))

(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn good-enough1? [guess x]
  (< (abs (- (square guess) x)) (/ guess 1000)))


(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(defn sqrt-iter2 [guess x]
  (if (good-enough1? guess x)
      guess
      (sqrt-iter2 (improve guess x)
                 x)))

;; To argue if is not needed (thus not special), we try to use cond
(defn new-if [predicate then else]
  (cond predicate then 
        :else else))

(defn new-if1 [pre then else]
  (if pre
      then
      else))

(defn sqrt-iter1 [guess x]
  (new-if1 (good-enough? guess x)
          guess
          (sqrt-iter1 (improve guess x)
                      x)))
; StackOverFlowError as new if is not special form and wont stop
;(sqrt-iter1 1.5 2)

;; define is special form too
;; If is special because after predicate it would only eval then or else and return their value, but not both. which the interpreter would eval the new-if like other procedures, eval all the args, therefore recursion wouldnt work
;; cond and if both special form, the important point lies in new-if being a procedure where operands are evaluated before applying the operator and thus the stopping condition wouldn't be met

;; Evercise 1.8
;; The good-enough? test used in computing square roots will not be very effective for finding the square roots of very small numbers 
;; small number like 0.00001 the smaller than will always be true
;; big number like 10000000 the smaller will always be false, due to floating point precision

(defn good-enough1? [guess x]
  (< (abs (- (square guess) x)) (/ guess 100000)))


;; could also omit x becuase x is bound variable in the def of sqrt, so the procedures good-enough? etc are in the scope of x. Lexcial scoping: free variable are looked up in the env in which the procedure was defined
(defn cubert [x]
  (defn cube [x]
    (* x x x))
  (defn good-enough-cube? [guess x]
    (< (abs (- (cube guess) x)) (/ guess 10000)))
  (defn improve-cube [guess x]
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (defn cuberoot-iter [guess x]
    (if (good-enough-cube? guess x)
        guess
        (cuberoot-iter (improve-cube guess x)
                      x)))
  (cuberoot-iter 1.0 x))

;; Exercise 1.9
(defn my-plus [a b]
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(defn my-plus1 [a b]
  (if (= a 0)
      b
      (my-plus1 (dec a) (inc b))))

(comment 
         (+ 4 5)
         (inc (+ 3 5))
         (inc (inc (+ 2 5)))
         ... ; thus recursive process

         (my-plus1 4 5)
         (my-plus1 3 6)
         (my-plus1 2 7)
         ... ; maintaining the state in the actual arg, thus iterative
         )

(defn A [x y]
  (cond 
    (= y 0) 0
    (= x 0) (* 2 y)
    (= y 1) 2
    :else (A (- x 1)
             (A x (- y 1)))))
;; what are the values of the following exp ?
(A 1 10)
(A 2 4)
(A 3 3)

(defn f [n] (A 0 n)) ; mupltiple of 2
(defn g [n] (A 1 n)) ; power of 2
(defn h [n] (A 2 n)) ; 2 to the power of n-1
(defn k [n] (* 5 n n)) ; trivial

; Tree recursion
(defn fib [n]
  (defn fib-iter [a b counting]
    (if (= counting 0)
        b
        (fib-iter (+ a b) a (- counting 1))))
  (fib-iter 1 0 n))

;; Counting change
(defn count-change [amount]
  (defn first-denomination [kinds-of-coins]
    (cond
      (= kinds-of-coins 1) 1
      (= kinds-of-coins 2) 5
      (= kinds-of-coins 3) 10
      (= kinds-of-coins 4) 25
      (= kinds-of-coins 5) 50))

  (defn cc [amount kinds-of-coins]
    (cond
      (= amount 0) 1
      (or (< amount 0) (= kinds-of-coins 0)) 0
      ;; first branch is the combination of not having the nth coin in change
      ;; second branch is the combinations of having the nth coin in change
      :else (+ (cc amount (- kinds-of-coins 1))
               (cc (- amount (first-denomination kinds-of-coins))
                   kinds-of-coins))))
  (cc amount 5))

(defn exe-eleven [n]
  (defn func-eleven [a b c counting]
    (cond
      (= counting 0) c
      :else (func-eleven (+ a
                          (* 2 b)
                          (* 3 c))
                        a
                        b
                        (- counting 1))))
  (func-eleven 2 1 0 n))

(exe-eleven 5)

;; Use let bindings and _ convention for an unused var to act as the name
;; to bind in order to add a println call directly into the let bindings
(defn exe-eleven-recur [counting]
  (cond
    (= counting 0) 0
    (= counting 1) 1
    (= counting 2) 2
    :else  
        (let [acc1 (exe-eleven-recur (- counting 3))
                _ (println "first term" acc1)
                acc2 (exe-eleven-recur (- counting 2))
                _ (println "second term" acc2) 
                acc3 (exe-eleven-recur (- counting 1))
                _ (println "third term" acc3)]
          (+ acc3
             (* 2 acc2)
             (* 3 acc1)))))


(exe-eleven-recur 5)

(deftes)

;; Exercise 1.12
;; println statement for debugging 
;; number -> list of number
(defn pascal-triangle [row col]
  (cond 
    (do (println "Branch 1")
        (or (< row 1) (< col 1))) 0
    (do (println "Branch 2")
        (= row 1)) 1
    (do (println "Branch 3")
        (= col row)) 1
    :else (+ (pascal-triangle (- row 1) (- col 1))
             (pascal-triangle (- row 1) col))))

(pascal-triangle 5 3)
         
(square 2)
;; Exercise 1.16
;; recursive process
(defn fast-expt [b n]
  (cond
    (= n 0) 1
    (even? n) (square (fast-expt b (/ n 2)))
    :else (* b (fast-expt b (- n 1)))))

;; key idea: invariant quantiy, ab^n remain unchanged from state to state
(defn fast-expt-iter [b n]
  (defn helper [a n]
    (cond
        (= n 1) a
        (even? n) (helper (* a (square b)) (/ n 2))
        :else (helper (* a b) (- n 1))))
  (helper 1 n))

;; Execise 1.17 18
(defn my-mult [base times]
  (defn my-double [n]
      (+ n n))
  (defn helper [accumulator n]
    (cond
      (= n 0) accumulator
      (even? n) (helper (my-double accumulator) (/ n 2)) 
      :else (helper (+ accumulator base) (- n 1))))
  (helper 0 times))

(fast-expt 3 3)
(fast-expt-iter 3 3)
(fast-expt-iter 3 4)
(fast-expt-iter 2 3)
;; Exercise 1.19
(defn fib-fast [n]
  (defn fib-iter-with-trans [a b p q number]
      (cond
        (= number 0) b
        (even? number)
          (fib-iter-with-trans a
                               b
                               (+ (square p) (square q))
                               (+ (square q) (* 2 (* p q)))
                               (/ number 2))
        :else (fib-iter-with-trans (+ (* b q) (* a q) (* a p))
                                   (+ (* b p) (* a q))
                                   p
                                   q
                                   (- number 1))))
  (fib-iter-with-trans 1 0 0 1 n))

(my-mult 3 3)
(my-mult 5 3)

(defn gcd [a b]
  (if (= b 0)
      a
      (gcd b (rem a b))))

(gcd 206 40)

;; Searching for divisors from 2 to to n, therefore order be linear n

(defn prime? [n]
  (defn divides? [a b]
    (= (rem a b) 0))
  (defn find-divisor [n test-divisor]
    (cond
      (> (square test-divisor) n) n
      (divides? n test-divisor) test-divisor
      :else (find-divisor n (+ 1 test-divisor))))  
  (defn smallest-divisor [n]
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(prime? 3)

(defn find-divisor [n test-divisor]
  (cond
    (> (square test-divisor) n) n
    (divides? n test-divisor) test-divisor
    :else (find-divisor n (+ 1 test-divisor))))  
(defn smallest-divisor [n]
  (find-divisor n 2))

;; Exercise 1.20
;; Applicative is more than normal order once 

;; get the remainder of a to the power exp being divided by m 

(defn expmod [base exp m]
  (rem (fast-expt base exp) m))

;; timing
(time (expmod 999999N 100000N 1000000N))

(defn expmod-without-fast-expt [base exp m]
  (cond
    (= exp 0) 1
    (even? exp) (rem (square (expmod-without-fast-expt base (/ exp 2) m)) m)
    :else (rem (* base (expmod-without-fast-expt base (- exp 1) m)) m)))

(time (expmod-without-fast-expt 999999N 100000N 1000000N))


(defn fermat-test [n]
  (defn try-it [a]
    (= (expmod a n n) a))
  (try-it (+ 1 (rand-int (- n 1)))))

(defn fast-prime? [p times]
  (cond
    (= times 0) true
    (fermat-test p) (fast-prime? p (- times 1))
    :else false))

(defn fermat-test-smaller-a [n mya]
  (defn try-it [a]
    (= (expmod a n n) a))
  (try-it (+ 1 (rand-int mya))))

(defn fast-prime-smaller-a? [p times mya]
  (cond
    (= times 0) true
    (fermat-test-smaller-a p mya) (fast-prime? p (- times 1) mya)
    :else false))

(defn prime-smaller-a? [p]
  (fast-prime-smaller-a? p 3 5))

(prime? 1000)

(fast-prime? 27 4)

(fast-prime? 3 2)
(fast-prime? 4 2)
(fermat-test 3)
(expmod 2 5 5)

;; Exercise 1.21
(smallest-divisor 127)   ;199
(smallest-divisor 199)   ;199
(smallest-divisor 1999)  ;1999
(smallest-divisor 19999) ;7

(time (smallest-divisor 10789))

;; Exercise 1.22
;; output a list, iter no., 
(defn search-for-primes [largeThan]
  ;; input: number be tested, output bool
  (defn prime? [p]
    (= p (smallest-divisor p)))
  ;; result is a list of prime '' and need to matain a iter state
  (defn help [iter primes]
    ;; if the primes list is not full yet (target is 3 now) 
    (cond
      ; terminating condition: a list of 3 prime 
      (= (count primes) 3) (do
                             (print primes)
                             (newline))
      :else
        (if (prime? iter)
            (help (+ iter 1) (cons iter primes))
            (help (+ iter 1) primes))))
  (time (help largeThan '())))

;; increase by 100x then time roughly increase by 10 times 
(search-for-primes 1000) ;"Elapsed time: 0.095853 msecs"
(search-for-primes 100000) ;"Elapsed time: 0.749 msecs"
(search-for-primes 10000000) ;"Elapsed time: 3.948665 msecs"
(search-for-primes 1000000000)

;; Exercise 1.23
(defn myNext [input]
  (if (= input 2)
      2
      (+ input 2)))

(defn find-divisor-alt [n test-divisor]
  (cond
    (> (square test-divisor) n) n
    (divides? n test-divisor) test-divisor
    :else (find-divisor-alt n (myNext test-divisor))))  

(defn smallest-divisor-alt [n]
  (find-divisor-alt n 2))

(defn search-for-primes-alt [largeThan]
  ;; input: number be tested, output bool
  (defn prime? [p]
    (= p (smallest-divisor-alt p)))
  ;; result is a list of prime '' and need to matain a iter state
  (defn help [iter primes]
    ;; if the primes list is not full yet (target is 3 now) 
    (cond
      ; terminating condition: a list of 3 prime 
      (= (count primes) 3) (do
                             (print primes)
                             (newline))
      :else
        (if (prime? iter)
            (help (+ iter 1) (cons iter primes))
            (help (+ iter 1) primes))))
  (time (help largeThan '())))

(search-for-primes-alt 1000) ; "Elapsed time: 0.102556 msecs"
(search-for-primes-alt 100000) ;"Elapsed time: 0.675177 msecs"
(search-for-primes-alt 1000000) ;"Elapsed time: 0.995185 msecs"
;; No difference, why?
;; myNext only reduce the complexity from n to 1/2n which is constant
;; factor should not matter unless input is very large[?]

;; Exercise 1.24
(defn search-for-primes-fast [largeThan]
  ;; input: number be tested, output bool
  (defn prime? [p]
    (= p (smallest-divisor p)))
  ;; result is a list of prime '' and need to matain a iter state
  (defn help [iter primes]
    ;; if the primes list is not full yet (target is 3 now) 
    (cond
      ; terminating condition: a list of 3 prime 
      (= (count primes) 3) (do
                             (print primes)
                             (newline))
      :else
        (if (fast-prime? iter 10)
            (help (+ iter 1) (cons iter primes))
            (help (+ iter 1) primes))))
  (time (help largeThan '())))

(defn fermat-test-without-fast-expt [n]
  (defn try-it [a]
    (= (expmod-without-fast-expt a n n) a))
  (try-it (+ 1 (rand-int (- n 1)))))

(defn fast-prime? [p times]
  (cond
    (= times 0) true
    (fermat-test-without-fast-expt p) (fast-prime? p (- times 1))
    :else false))

(search-for-primes-fast 1000) ; "Elapsed time: 0.131802 msecs"
(search-for-primes-fast 1000000) ; "Elapsed time: 1.372343 msecs"

(defn cube [x]
  (* x x x))

(defn sum [term a next b]
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(defn sum-cubes [a b]
  (sum cube a inc b))

(sum-cubes 1 10)

(defn plus2 [x]
  (+ x 2))

(sum cube 0 (fn [x] (+ x 2)) 8)

(defn simpson [f a b n]
  (def h (/ (- b a) n))
  (defn leading [k]
    (cond
      (= k 0) 1
      (= k n) 1
      (odd? k) 4
      :else 2))
  (defn simpson-next [k]
    (f (+ a (* h k))))
  (defn simpson-term [k]
    (* (leading k)
       (simpson-next k)))
  (* (sum simpson-term 0 inc n)
     (/ h 3)))

(simpson cube 0 1 1000)

;; Exercise 1.30
;; Iterative version of sum term

(defn sum-iter [term a mynext b]
  (defn iter [a result]
    (if (> a b)
        result
        (iter (mynext a) (+ (term a) result))))
   (iter a 0))

(defn sum-cubes [a b]
  (sum-iter cube a inc b))

(sum-cubes 1 (+ 2 8))

;; Exercise 1.31

(defn product-iter [term a mynext b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (mynext a) (* (term a) result))))
  (iter a 1))

(defn product-recur [term a mynext b]
  (if (> a b)
      1
      (* (term a) (product-recur term (mynext a) mynext b))))

(defn myFactorial [a]
  (product-recur (fn [x] x) 1 inc a)) 
;; sicp.core-test
(myFactorial 5)

;; Exercise 1.32

(defn accumulate [combiner null-value term a mynext b]
  (if (> a b)
      null-value
      (combiner (term a) (accumulate 
                           combiner null-value term
                           (mynext a) mynext b))))

(accumulate * 1 (fn [x] x) 1 inc 5)

;; Exercise 1.33
;; nested if is bad ?

(defn filtered-accumulate [combiner null-value term a mynext b predicate]
  (if (predicate a)
      (if (> a b) 
          null-value
          (combiner (term a) (filtered-accumulate combiner null-value
                                                  term (mynext a)
                                                  mynext b predicate)))
      (filtered-accumulate
        combiner null-value term (mynext a) mynext b predicate)))

;; (a)
(filtered-accumulate + 0 (fn [x] x) 1 inc 10 odd?)

;; Exercise 1.34
(defn f [g]
  (g 2))

(f square)

(f (fn [z] (* z (+ z 1))))

;; this will reduce to calling f with 2, meaning calling 2 with 2, so error

;; Fixed point
;; Exercise 1.36

(def tolerance 0.00001)
(defn fixed-point [f firstguess]
  (defn close-enough? [v1 v2]
    (< (abs (- v1 v2)) tolerance))
  (defn my-try [guess num-of-guess]
    (let [my-next (f guess)]
        (if (close-enough? guess my-next)
            ;;(do (println "The Answer: " my-next)
            my-next
            ;;(do (println "The next guess: " my-next)
            (my-try my-next (+ 1 num-of-guess)))))
      (my-try firstguess 1))

;; import math module from java
(import java.lang.Math)

(fixed-point (fn [x] (+ (Math/sin x) (Math/cos x))) 1.0)

;; 37th guess
(fixed-point (fn [x] (/ (Math/log 1000) (Math/log x))) 1.1)

;; 1.3.4 Procedures as Returned Values
(defn average-damp [f] 
  (fn [x] (average x (f x)))) 

(def returnedf (average-damp square))
(returnedf 10)

(defn sqrt [x]
  (fixed-point (average-damp (fn [y] (/ x y))) 1.0))

;; take 12 guesses
(sqrt 19999)

;; Newton's method
(def dx 0.00001)
(defn deriv [g]
  (fn [x] (/ (- (g (+ x dx)) (g x))
             dx)))
(defn newton-transfrom [g]
  (fn [x] (- x (/ (g x) ((deriv g) x)))))

(defn newton-method [g guess]
  (fixed-point (newton-transfrom g) guess))

(defn sqrt-newton [x]
  (newton-method (fn [y] (- (square y) x)) 1.0))

;; 12 guesses
(sqrt-newton 19999)

(defn fixed-point-of-transform [g transform guess]
  (fixed-point (transform g) guess))

;; Exercise 1.40
(defn cubic [a b c]
  (fn [x] (+ (cube x) (* a (square x)) (* b x) c)))

;; Exercise 1.41
(defn my-double [f]
  (fn [x] (f (f x))))

(((my-double (my-double my-double)) inc) 5)

;; Exercise 1.42
(defn compose [f g]
  (fn [x] (f (g x))))

;; 49
((compose square inc) 6)

;; Exercise 1.43
(defn repeated [f num-of-application]
  (defn helper-repeated [acc]
    (if (>= acc num-of-application)
        f
        (compose f (helper-repeated (+ 1 acc)))))
  (helper-repeated 1))

((repeated square 2) 5)

;; Exercise 1.44
(defn smoothing [f]
  (fn [x] (+ (f x) (f (+ x dx)) (f (- x dx)))))

(repeated smoothing 4)

;; Exercise 1.45
;; Exercise 1.46

;; Chapter 2
(defn make-rat [x y]
  (/ x y))

(defn numer [x] (first x))
(defn denom [x] (second x))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer x) (denom y)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defn make-rat [n d]
  (let [g (gcd n d)]
  (cons (/ n g) (cons (/ d g) '()))))

(defn print-rat [x]
    (newline)
    (print (numer x))
    (print "/")
    (print (denom x)))

(def one-half (make-rat 1 2))
(def one-third (make-rat 1 3))

(print-rat (add-rat one-third one-third))

;; Exercise 2.2
;; Point 
(defn make-point [x y]
  (cons x (cons y '())))

(defn x-point [x] (first x))
(defn y-point [x] (second x))

(defn make-segment [start end]
  (cons start (cons end '())))

(defn start-segment [segment] (first segment))
(defn end-segment [segment] (second segment))

(def point-x (make-point 1 2))
(def point-y (make-point 2 4))

(def test-segment (make-segment point-x point-y))

(defn midpoint-segment [segment]
  (make-point (/ (+ (x-point (start-segment segment))
                    (x-point (end-segment segment)))
                 2)
              (/ (+ (y-point (start-segment segment))
                    (y-point (end-segment segment)))
                 2)))

(defn print-point [p]
  (newline)
  (print "(")
  (print (x-point p))
  (print ",")
  (print (y-point p))
  (print ")"))

(midpoint-segment test-segment)

;; Exercise 2.3
;; First implementation
;; A rectangle could be represented by 4 points arranged clockwise
(defn make-rect [a b c d]
  (cons a (cons b (cons c (cons d '())))))

(defn firstpoint [rect] (first rect))
(defn secondpoint [rect] (first (rest rect)))
(defn thirdpoint [rect] (first (rest (rest rect)))) 
(defn fourthpoint [rect] (last rect))

;; point point -> num
(defn point-distance [x y]
  (sqrt (+ (square (- (x-point x) (x-point y)))
           (square (- (y-point x) (y-point y))))))

;; rect -> num
(defn perimeter [rect]
  (* (+ (point-distance (firstpoint rect)
                        (secondpoint rect))
        (point-distance (secondpoint rect)
                        (thirdpoint rect)))
     2))

(defn area [rect]
  (* (point-distance (firstpoint rect)
                     (secondpoint rect))
     (point-distance (secondpoint rect)
                     (thirdpoint rect)))
     )
(def first-point (make-point 0 2))
(def second-point (make-point 4 2))
(def third-point (make-point 4 0))
(def fourth-point (make-point 0 0))

;; A rectangle could also be represented by 4 segment
;; which the internal representation wouldn't be diff
(defn firstpoint [rect] (start-segment (first rect))) 
(defn secondpoint [rect] (end-segment (first rect))) 
(defn thirdpoint [rect] (start-segment (first (rest rect))))
(defn fourthpoint [rect] (end-segment (first (rest rect))))

;; Define cons car cdr
(defn mycon [x y]
  (defn dispatch [m]
    (cond
        (= m 0) x
        (= m 1) y
        :else (println "Error")))
  dispatch)

(defn car [z] (z 0))
(defn cdr [z] (z 1))

;; Alternative representation
;; This returns a procedure 
(defn mycons [x y]
  (fn [m] (m x y)))

;; car calls z with an arguement of lambda fx selecting the first element
(defn car [z]
  (z (fn [p q] p)))

;; Exercise 2.4
(defn cdr [z]
  (z (fn [p q] q)))

(car (mycons 1 2))

;; ((mycons 1 2) (fn [p q] p))
;; ((fn [m] (m 1 2)) (fn [p q] p))
;; ((fn [p q] p) 1 2)
;; 1

(cdr (mycons 1 2))

;; Exercise 2.5
;; represent a pair of non neg number using only num and arithemetic ops
;; if we represent the pair a & b as the integer which is product of 
;; e.g a=1, b=2, then we represent them as 6 (2*3)
(defn mycons [x y]
  (* (fast-expt 2 x)
     (fast-expt 3 y)))

(defn mycar [z]
  (defn helper-mycar [x acc]
    (if (= (rem x 2) 0)
        (helper-mycar (/ x 2) (+ acc 1))
        acc))
  (helper-mycar z 0))

(defn mycdr [z]
  (defn helper-mycdr [x acc]
    (if (= (rem x 3) 0)
        (helper-mycdr (/ x 2) (+ acc 1))
        acc))
  (helper-mycdr z 0))

;; Testing
(mycar (mycons 4 5))
(mycdr (mycons 4 5))

;; Exercise 2.6
;; lambda calculus
(def zero (fn [f] (fn [x] x)))
;; zero is a fx take another function that take x as an arg

(defn add-1 [n]
  (fn [f] (fn [x] (f ((n f) x)))))

(def lam-one (fn [f] (fn [x] (f x))))

(defn to-int [lam]
;; take the (+ x 1) as an arg and return another function which take 0 as arg
  ((lam (fn [x] (+ x 1))) 0))
;;      ----------------- -
;;      first arg: a fx   x
;; gives it an inc fx and call it with 0

(def lam-two (fn [f] (fn [x] (f (f x)))))

(to-int lam-two)
(to-int zero)
(to-int lam-one)

(defn plus-lam [m n]
  (fn [f] (fn [x] ((m f) ((n f) x)))))
(to-int (add-1 (add-1 zero)))

(to-int (plus-lam lam-two lam-one))

(defn mult-lam [m n]
  (fn [f] (fn [x] ((m (n f)) x))))
(to-int (plus-lam lam-two lam-one))
(to-int (mult-lam lam-two lam-two))

;; apply n to m: m takes a fx return a fx, so this would return a fx that takes a fx return a fx
(defn power-lam [n m]
  (fn [f] (fn [x] (((m n) f) x))))
(to-int (power-lam lam-two lam-two))

(defn make-interval [a b] 
  (cons a (cons b '())))

(defn upper-bound [inter]
  (if (> (first inter) (last inter))
      (first inter)
      (last inter))) 

(defn lower-bound [inter]
  (if (<= (first inter) (last inter))
      (first inter)
      (last inter))) 

;; Exercise 2.9
;; multiplication or division doesnt work for these 2 examples
(def intervalexp  (make-interval 1 2))
(def intervalexp2 (make-interval 3 4))

(upper-bound intervalexp)

;; smaller interval in front
(defn sub-interval [x y]
  (let [p1 (- (lower-bound y) (upper-bound x)) ;; if y is bigger
        p3 (- (upper-bound y) (lower-bound x))]
   (make-interval (min p1 p3)
                  (max p1 p3))))

(sub-interval intervalexp intervalexp2)

(defn div-interval [x y]
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (upper-bound x))))

(defn make-center-width [center width]
  (make-interval (- center width) (+ center width)))

(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(defn percent [inter]
  (/ (- (upper-bound inter) (lower-bound inter))
     (* 2 (center inter))))

(defn make-center-percent [center per]
  (make-interval (- center (* center per))
                 (+ center (* center per))))

(def percentexp1 (make-center-percent 100 0.01))
(def percentexp2 (make-center-percent 200 0.01))

;; Exercise 2.17
(defn last-pair [ls]
  (if (nil? (second (rest ls)))
      (first (rest ls))
      (last-pair (rest ls))))

(last-pair '(1 2))

;; Exercise 2.18
(comment
(defn myreverse [ls]
  (if (= nil (rest ls))
      (first ls)
      (cons (last-pair (rest ls)) (first ls))))
)

(myreverse '(1 2 3))

;; Represeting Seq
(defn list-ref [item n]
  (if (= n 0)
      (first item)
      (list-ref (rest item) (- n 1))))

(+ 1 2)
(defn no-more? [ls] (empty? ls))
(defn first-denomination [ls] (first ls))
(defn except-first-denomination [ls] (rest ls))
;; Exercise 2.19
(defn cc [amount coin-values]
 (cond  
       (= amount 0) 1
       (or (< amount 0) (no-more? coin-values)) 0
       :else (+ (cc amount
                    (except-first-denomination coin-values))
                (cc (- amount
                       (first-denomination coin-values))
                    coin-values))))


;; Exercise 2.20
(defn same-parity [& ls]
  (if (= 0 (rem (first ls) 2))
      (filter even? ls)
      (filter odd? ls)))

(same-parity 1 2 3 4 5)

(defn square-list [col]
  (map (fn [x] (* x x)) col)) 

(square-list '(2 3 4 5))
;; because recursive process return ans 'on the way out' while iterative process
;; do it 'on the way in': the example in lecture about how we can view processes
;; as people hire other people to do stuff
(defn for-each [x item]
  (if (empty? item)
      (println "end")
      (do
          (x (first item))
          (for-each x (rest item)))))

;; doesn't work
(for-each (fn [x] (println x)) '(1 2 3))

(first '(7))
;; 2.2.2
;;Lecture 9
;;Exercise 2.23

;; Exercise 2.25
(rest (first (rest (rest '(1 3 (5 7) 9)))))
;; TODO: check nested '( caveat
(first (first '((7))))

(def x (take 3 (range 1 4)))
(def y (take 3 (range 4 7)))

;; same as scheme
(list x y)
(concat x y)
(cons x y) ;; ((1 2 3) 4 5 6)

(defn myreverse [ls]
  (if (= nil (rest ls))
      (first ls)
      (cons (last-pair (rest ls)) (first ls))))

;; !!!
(defn deep-reverse [ls]
  (cond
        ;; edge case
        (empty? ls) ls
        ;; if not list, no need to reverse the list
        (not (list? (first ls))) (list (deep-reverse (rest ls)) (first ls))
        :else
          (list (deep-reverse (rest ls)) (deep-reverse (first ls)))))
(deep-reverse '((5 3)))
(println (deep-reverse (list (list 1 2) (list 3 4))))

        
;; !!!
(defn flattening [ls]
  (cond
    (empty? ls) ls
    (list? (first ls)) (cons (flattening (first ls)) (flattening (rest ls)))
    :else 
      (cons (first ls) (flattening (rest ls)))))

(def x (list (list 1 2) (list 3 4)))

(defn make-mobile [l r]
  (list l r))

(defn l-branch [mo]
  (first mo))

(defn r-branch [mo]
  (second mo))

(defn make-branch [len stru]
  (list len stru))

(defn branch-length [branch]
  (first branch))

(defn branch-structure [branch]
  (second branch))

;; 2.29
;; total weight : all the branch len * structure
;; mo -> float
(comment "
(defn total-weight [mo]
  (cond (empty? mo) 0
        (not (
      (+ (total-weight (branch-structure (l-branch mo)))
         (total-weight (branch-structure (r-branch mo)))))))
        :else
            0)
")

(def m2 (make-mobile 
             (make-branch 4 6) 
             (make-branch 2 
                          (make-mobile 
                           (make-branch 5 8) 
                           (make-branch 10 4)))))

(println m2)

(defn scale-tree [tree factor]
  (cond
    (do (println "basecase reached") (empty? tree)) []
    (do (println "count = 1") (not (vector? tree))) (* tree factor)
    :else
      (conj (scale-tree (first tree) factor)
            (scale-tree (rest tree) factor))))

(def t [1 [2 [3 4] 5] [6 7]])
(count t)
(scale-tree t 10)

(vector? (rest t)))

