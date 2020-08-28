(ns sicp.core-test
  (:require [clojure.test :refer :all]
            [sicp.core :refer :all]))

(deftest a-test
  (testing "Okay"
    (is (not (= 0 1)))))


(deftest twelve
  (testing "Pascal Triangle"
    (is (= (pascal-triangle 1 1) 1))
    (is (= (pascal-triangle 2 1) 1))
    (is (= (pascal-triangle 2 2) 1))
    (is (= (pascal-triangle 3 2) 2))
    (is (= (pascal-triangle 4 2) 3))
    (is (= (pascal-triangle 4 3) 3))
    (is (= (pascal-triangle 4 4) 1))
    (is (= (pascal-triangle 5 3) 6))
    ))

(deftest primeTest
  (testing "Searching for divisors primality"
    (is (= (prime? 3) true))
    (is (= (prime? 5) true))
    (is (= (prime? 4) false))
    (is (= (prime? 8) false))
    ))

(deftest myFactorialtest
  (testing "Fact"
    (is (= (myFactorial 5) 120))))

(deftest fastFib 
    (testing "Exercise 1.19: Fast Fib"
        (is (= (fib-fast 0) 0))))

(deftest product-recur-test
  (testing "Prodcut recursion"
    (is (= (product-recur (fn [x] x) 1 inc 5) 120))))

(comment

(deftest eleven
  (testing "Iterative version"
    (is (= (exe-eleven-recur 5) (exe-eleven 5)))
    (is (= (exe-eleven-recur 6) (exe-eleven 6)))
    (is (= (exe-eleven-recur 7) (exe-eleven 7)))
    (is (= (exe-eleven-recur 8) (exe-eleven 8)))
    ))

(deftest make-point-test
  (testing "Making Point"
    (is (= (make-point 1 2) '(1 2)))))

(deftest make-segment-test
  (testing "Making Segment"
    (is (= (make-segment  point-x point-y) '((1 2) (2 4))))))
)


