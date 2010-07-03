;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:author "Zachary Tellman"}
  cantor
  (:require [cantor
             [vector :as vec]
             [matrix :as mat]
             [misc :as misc]
             [range :as range]])
  (:use [clojure.contrib.def :only (defmacro-)]))

;;

(defmacro- import-fn [sym]
  (let [m (meta (eval sym))
        m (meta (intern (:ns m) (:name m)))
        n (:name m)
        arglists (:arglists m)
        doc (:doc m)]
    (list `def (with-meta n {:doc doc :arglists (list 'quote arglists)}) (eval sym))))

;; arithmetic operators

(defn add
  "Add together scalars or vectors of equal dimension."
  ([a] (vec/add a))
  ([a b] (vec/add a b))
  ([a b c] (add (add a b) c))
  ([a b c & rest] (add (add a b c) (apply add rest))))

(defn sub
  "Subtract scalars or vectors of equal dimension.

   (- a b c) is equivalent to (- (- a b) c)"
  ([a] (vec/sub a))
  ([a b] (vec/sub a b))
  ([a b c] (sub (sub a b) c))
  ([a b c & rest] (sub (sub a b c) (apply add rest))))

(defn mul
  "Multiplies together a list of scalars, or a vector followed by any combination of
   scalars and vectors of the same dimension."
  ([a] (vec/mul a))
  ([a b] (vec/mul a b))
  ([a b c] (mul (mul a b) c))
  ([a b c & rest] (mul (mul a b c) (apply mul rest))))

(defn div
  "Divides a list of scalars, or a vector followed by any combination of scalars and
   vectors of the same dimension.

   (/ a b c) = (/ (/ a b) c)"
  ([a] (vec/div a))
  ([a b] (vec/div a b))
  ([a b c] (div (div a b) c))
  ([a b c & rest] (div (div a b c) (apply mul rest))))

(import-fn #'vec/dot)
(import-fn #'vec/polar)
(import-fn #'vec/cartesian)

(defn lerp
  "Linear interpolation between a and b, where t=0 is a, and t=1 is b."
  [a b t]
  (add a (mul (sub b a) t)))

(defn length-squared
  "Calculates the length squared of v.  Significantly more efficient than (length v)."
  [v]
  (dot v v))

(defn length
  "Calculates the length of v."
  [v]
  (Math/sqrt (length-squared v)))

(defn normalize
  "Normalizes v, such that its direction remains the same, but its length is 1."
  [v]
  (div v (length v)))

;; vector

(import-fn vec/vec2)
(import-fn vec/vec3)
(import-fn vec/polar2)
(import-fn vec/polar3)

(import-fn vec/cartesian?)
(import-fn vec/polar?)
(import-fn #'vec/dimension)

;; range

(import-fn range/range?)
(import-fn range/interval)
(import-fn range/box2)
(import-fn range/box3)
(import-fn #'range/upper)
(import-fn #'range/lower)
(import-fn range/intersection)
(import-fn range/union)
(import-fn range/inside?)
(import-fn range/size)

;; geometry

(import-fn vec/cross)

(import-fn #'mat/transform-matrix)
(import-fn #'mat/transform-vector)
(import-fn mat/rotation-matrix)
(import-fn mat/identity-matrix)
(import-fn mat/translation-matrix)
(import-fn mat/scaling-matrix)
(import-fn mat/normal-matrix)

(import-fn vec/map*)
(import-fn vec/all?)

;; misc

(import-fn misc/prime-factors)
(import-fn misc/rectangle-factors)
(import-fn misc/radians)
(import-fn misc/degrees)

;;

(defmacro- extend-numbers [& body]
  `(do
     (extend-type java.lang.Double ~@body)
     (extend-type java.lang.Integer ~@body)
     (extend-type java.lang.Float ~@body)
     (extend-type clojure.lang.Ratio ~@body)))

(extend-numbers
 vec/Arithmetic
 (add
  ([a] a)
  ([a b] (+ a b)))
 (sub
  ([a] (- a))
  ([a b] (- a b)))
 (mul
  ([a] a)
  ([a b] (* a b)))
 (div
  ([a] a)
  ([a b] (/ a b))))

(extend-numbers
 vec/Tuple
 (map-
  ([n f] (f n))
  ([a b f] (f a b))
  ([a b rest f] (apply f (list* a b rest))))
 (all-
  ([n f] (f n))
  ([a b f] (f a b)))
 (dimension [_] 1))

(extend-numbers
 vec/Polar
 (cartesian [n] (cartesian (polar2 n 1))))
