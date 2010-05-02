;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns cantor
  (:use [clojure.contrib.def :only (defmacro-)]))

;;;

(defprotocol Arithmetic
  (add- [a] [a b])
  (sub- [a] [a b])
  (mul- [a] [a b])
  (div- [a] [a b]))

(defn add
  ([a] (add- a))
  ([a b] (add- a b))
  ([a b c] (->> a (add b) (add c)))
  ([a b c & rest] (add (add a b c) (apply add rest))))

(defn sub
  ([a] (sub- a))
  ([a b] (sub- a b))
  ([a b c] (-> a (sub b) (sub c)))
  ([a b c & rest] (sub (sub a b c) (apply add rest))))

(defn mul
  ([a] (mul- a))
  ([a b] (mul- a b))
  ([a b c] (->> a (mul b) (mul c)))
  ([a b c & rest] (mul (mul a b c) (apply mul rest))))

(defn div
  ([a] (div- a))
  ([a b] (div- a b))
  ([a b c] (-> a (div b) (div c)))
  ([a b c & rest] (div (div a b c) (apply mul rest))))

(defprotocol Cartesian
  (dot [a b])
  (polar [v])
  (map- [v f] [v f rest]))

(defprotocol Polar
  (cartesian [p]))

(defprotocol Matrix
  (transform-matrix [a b] "Returns the product of two matrices.")
  (transform-vector [m v] "Returns a vector transformed by the matrix."))

(defn map*
  ([f v] (map- v f))
  ([f v & rest] (map- v f rest)))

(defn lerp [a b t]
  (add a (mul (sub b a) t)))

(defn length-squared [v]
  (dot v v))

(defn length [v]
  (Math/sqrt (length-squared v)))

(defn normalize [v]
  (div v (length v)))

;;;

(require '[cantor
           [vector :as vec]
           [matrix :as mat]
           [misc :as misc]])

(def vec2 vec/vec2)
(def vec3 vec/vec3)
(def polar2 vec/polar2)
(def polar3 vec/polar3)

(def vec? vec/vec?)
(def vec2? vec/vec2?)
(def vec3? vec/vec3?)

(def polar? vec/polar?)
(def polar2? vec/polar2?)
(def polar3? vec/polar3?)

(def cross vec/cross)

(def rotation-matrix mat/rotation-matrix)
(def identity-matrix mat/identity-matrix)
(def translation-matrix mat/translation-matrix)
(def scaling-matrix mat/scaling-matrix)
(def normal-matrix mat/normal-matrix)

(def prime-factors misc/prime-factors)
(def rectangle-factors misc/rectangle-factors)
(def radians misc/radians)
(def degrees misc/degrees)

;;;

(defmacro- extend-numbers [& body]
  `(do
     (extend-type java.lang.Double ~@body)
     (extend-type java.lang.Integer ~@body)
     (extend-type java.lang.Float ~@body)
     (extend-type clojure.lang.Ratio ~@body)))

(extend-numbers
 Polar
 (cartesian [n] (cartesian (polar2 n 1))))

(extend-numbers
 Arithmetic
 (add- [a] a)
 (add- [a b] (+ a b))
 (sub- [a] (- a))
 (sub- [a b] (- a b))
 (mul- [a] a)
 (mul- [a b] (* a b))
 (div- [a] a)
 (div- [a b] (/ a b)))

