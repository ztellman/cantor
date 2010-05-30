;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:skip-wiki true} cantor.misc
  (:use [clojure.contrib.lazy-seqs :only (primes)]))

(defn prime-factors
  "Returns prime factors of a number"
  ([n]
     (loop [n (int n), primes primes, factors []]
       (let [factor (first primes)]
         (cond
          (= 1 n) factors
          (zero? (rem n factor)) (recur (int (/ n factor)) primes (conj factors factor))
          :else (recur n (next primes) factors))))))

(defn rectangle-factors
  "Returns two values [x y], where x*y == n.
   x and y should be relatively close to each other in value."
  [n]
  (let [factors   (prime-factors n)
        reordered (take (count factors) (interleave factors (reverse factors)))
        sqrt      (int (Math/sqrt n))
        divisor   (reduce #(if (>= sqrt (* %1 %2)) (* %1 %2) %1) 1 reordered)]
    [divisor (/ n divisor)]))

(defn radians
  "Transforms degrees to radians."
  [x]
  (* (/ Math/PI 180.0) (double x)))

(defn degrees
  "Transforms radians to degrees."
  [x]
  (* (/ 180.0 Math/PI) (double x)))

;;;