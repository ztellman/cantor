;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:skip-wiki true} cantor.matrix
  (:use [cantor misc]
        [clojure.walk :only (postwalk postwalk-replace)])
  (:import [cantor.vector Vec3 Vec2]))

;;;

(defmacro tag-vars [types body]
  (let [types (into {} (map (fn [[k v]] [k (with-meta k (merge (meta k) {:tag v}))]) types))]
    (->> body
         (postwalk-replace types)
         (postwalk #(if (vector? %) (postwalk-replace (zipmap (vals types) (keys types)) %) %)))))

;;;

(defprotocol Matrix
  (transform-matrix [a b] "Returns the product of two matrices.")
  (transform-vector [m v] "Returns a vector transformed by the matrix."))

;;;

(defrecord Matrix44
  [#^double m00 #^double m10 #^double m20 #^double m30
   #^double m01 #^double m11 #^double m21 #^double m31
   #^double m02 #^double m12 #^double m22 #^double m32
   #^double m03 #^double m13 #^double m23 #^double m33])

(tag-vars
 {a Matrix44, b Matrix44, m Matrix44}
 (defmacro transform-matrix44- []
   (let [index (fn [m x y] (list (symbol (str ".m" x y)) m))]
     (list*
      `Matrix44.
      (map
       (fn [[i j]]
         (list* `+ (map (fn [k] (list `* (index 'a k i) (index 'b j k))) (range 3))))
       (for [i (range 4) j (range 4)] [i j]))))))

(tag-vars
 {m Matrix44, v Vec3}
 (extend-type Matrix44
   Matrix
   (transform-vector
    [m v]
    (Vec3. (+ (* (.x v) (.m00 m)) (* (.y v) (.m10 m)) (* (.z v) (.m20 m)) (.m30 m))
           (+ (* (.x v) (.m01 m)) (* (.y v) (.m11 m)) (* (.z v) (.m21 m)) (.m31 m))
           (+ (* (.x v) (.m02 m)) (* (.y v) (.m12 m)) (* (.z v) (.m22 m)) (.m32 m))))
   (transform-matrix
    [a b]
    (transform-matrix44-))))

;;;

(defrecord Matrix33
  [#^double m00 #^double m10 #^double m20
   #^double m01 #^double m11 #^double m21
   #^double m02 #^double m12 #^double m22])

(tag-vars
 {a Matrix33, b Matrix33, m Matrix33}
 (defmacro transform-matrix33- []
   (let [index (fn [m x y] (list (symbol (str ".m" x y)) m))]
     (list*
      `Matrix33.
      (map
       (fn [[i j]]
         (list* `+ (map (fn [k] (list `* (index 'a k i) (index 'b j k))) (range 3))))
       (for [i (range 3) j (range 3)] [i j]))))))

(tag-vars
 {m Matrix33, v Vec2}
 (extend-type Matrix33
   Matrix
   (transform-vector
    [m v]
    (Vec2. (+ (* (.x v) (.m00 m)) (* (.y v) (.m10 m)) (.m20 m)) 
           (+ (* (.x v) (.m01 m)) (* (.y v) (.m11 m)) (.m21 m))))
   (transform-matrix
    [a b]
    (transform-matrix33-))))

;;;

(defn identity-matrix
  "Creates an identity matrix.  The default result is a 4x4 matrix."
  ([] (identity-matrix 3))
  ([dim]
     (condp = dim
	 2 (Matrix33.
	    1 0 0
	    0 1 0
	    0 0 1)
	 3 (Matrix44.
	    1 0 0 0
	    0 1 0 0
	    0 0 1 0
	    0 0 0 1))))

(defn translation-matrix
  "Returns a matrix which offsets vectors by the input values."
  ([x y]
     (Matrix33.
      1 0 x
      0 1 y
      0 0 1))
  ([x y z]
     (Matrix44.
      1 0 0 x
      0 1 0 y
      0 0 1 z
      0 0 0 1)))

(defn scaling-matrix
  "Returns a matrix which scales vectors by the input values."
  ([x y]
     (Matrix33.
      x 0 0
      0 y 0
      0 0 1))
  ([x y z]
     (Matrix44.
      x 0 0 0
      0 y 0 0
      0 0 z 0
      0 0 0 1)))

(tag-vars
 {m Matrix44}
 (defn normal-matrix
   "Returns a matrix which is stripped of all translations."
   [m]
   (Matrix44.
    (.m00 m) (.m10 m) (.m20 m) 0
    (.m01 m) (.m11 m) (.m21 m) 0
    (.m02 m) (.m12 m) (.m22 m) 0
    (.m03 m) (.m13 m) (.m23 m) 1)))

(defn rotation-matrix
  "Returns a matrix which either rotates 2-D vectors about the origin, or 3-D vectors about
   the specified axis."
  ([theta]
     (let [theta (radians theta)
	   s (Math/sin theta)
	   c (Math/cos theta)]
       (Matrix33.
	c     s 0
	(- s) c 0
	0     0 1)))
  ([theta x y z]
     (let [theta (radians theta)
	   s (Math/sin theta)
	   c (Math/cos theta)
	   t (- 1 c)]
       (Matrix44.
	(+ c (* t x x))        (- (* t x y) (* s z))   (+ (* t x z) (* s y))   0
	(+ (* t x y) (* s z))  (+ (* t y y) c)         (- (* t y z) (* s x))   0
	(- (* t x z) (* s y))  (+ (* t y z) (* s x))   (+ (* t z z) c)         0
	0                      0                       0                       1))))

