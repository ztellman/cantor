;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns
  ^{:skip-wiki true}
  cantor.range
  (:use [cantor.utils])
  (:require [cantor.vector :as vec])
  (:import [cantor.vector Vec2 Vec3]))

;;;

(defprotocol Range
  (#^vec/Tuple upper [r] "Returns the minima of the range.")
  (#^vec/Tuple lower [r] "Returns the maxima of the range.")
  (clone [r a b] "Returns a range of the same type with endpoints set to a and b"))

(defn overlap?
  "Returns true if the two ranges overlap."
  [a b]
  (and
   (vec/all? <= (upper a) (lower b))
   (vec/all? >= (lower a) (upper b))))

(defn intersection
  "Returns the intersection of the two ranges, or nil if they don't intersect."
  [a b]
  (when (overlap? a b)
    (clone a
      (vec/map* max (upper a) (upper b))
      (vec/map* min (lower a) (lower b)))))

(defn union
  "Returns the union of the two ranges."
  [a b]
  (clone a
    (vec/map* min (upper a) (upper b))
    (vec/map* max (lower a) (lower b))))

(defn inside?
  "Returns true if vector 'p' is inside range 'r'."
  [r p]
  (and
    (vec/all? >= (upper r) p)
    (vec/all? <= (lower r) p)))

(defn size
  "Returns the difference between the two extremes of the range."
  [r]
  (vec/sub (lower r) (upper r)))

(defn offset
  "Returns an interval offset by 'value'."
  [interval value]
  (clone interval
    (vec/add (upper interval) value)
    (vec/add (lower interval) value)))

(defn scale
  "Returns an interval scaled by 'value'."
  [interval value]
  (clone interval
    (vec/mul (upper interval) value)
    (vec/mul (lower interval) value)))

;;;

(deftype Interval [#^double a #^double b]
   Object
   (equals [_ r]
     (try
       (and
	 ;;(instance? Interval b)
	 (= a (.a #^Interval r))
	 (= b (.b #^Interval r)))
       (catch Exception e
	 false)))
   (hashCode [_]
     (+ (int a) (int b)))

   clojure.lang.Seqable
   (seq [_] (list a b))
   
   clojure.lang.Indexed
   (nth [this idx] (this idx))
   (count [_] 2)
   
   Range
   (upper [r] a)
   (lower [r] b)
   (clone [_ a b] (Interval. a b))
   
   clojure.lang.IFn
   (invoke [_ n]
     (condp = n
       0 a
       1 b)))

(deftype Box2 [#^Vec2 a #^Vec2 b]
  Object
  (equals [_ r]
    (try
      (and
	;;(instance? Box2 b)
	(= a (.a #^Box2 r))
	(= b (.b #^Box2 r)))
      (catch Exception e
	false)))
  (hashCode [_]
    (+ (.hashCode a) (.hashCode b)))

  clojure.lang.Seqable
  (seq [_] (list a b))

  clojure.lang.Indexed
  (nth [this idx] (this idx))
  (count [_] 2)
   
  Range
  (upper [r] a)
  (lower [r] b)
  (clone [_ a b] (Box2. a b))

  clojure.lang.IFn
  (invoke [_ n]
    (condp = n
      0 a
      1 b)))

(deftype Box3 [#^Vec3 a #^Vec3 b]
  Object
  (equals [_ r]
    (try
      (and
	;;(instance? Box2 b)
	(= a (.a #^Box3 r))
	(= b (.b #^Box3 r)))
      (catch Exception e
	false)))
  (hashCode [_]
	    (mod (+ (.hashCode a) (.hashCode b))
		 Integer/MAX_VALUE))

  clojure.lang.Seqable
  (seq [_] (list a b))

  clojure.lang.Indexed
  (nth [this idx] (this idx))
  (count [_] 2)
  
  Range
  (upper [r] a)
  (lower [r] b)
  (clone [_ a b] (Box3. a b))

  clojure.lang.IFn
  (invoke [_ n]
    (condp = n
      0 a
      1 b)))

;;;

(defn range?
  "Returns true if 'r' is a range."
  [r]
  (satisfies? Range r))

(defn interval
  "Creates a 1-D range."
  [a b]
  (Interval. a b))

(defn box2
  "Creates a 2-D range, or box."
  [ul lr]
  (Box2. ul lr))

(defn box3
  "Creates a 3-D range, or box."
  [ul lr]
  (Box3. ul lr))

;;;

(defmethod print-method cantor.range.Interval [r writer]
  (.write writer (str "[ lower=" (lower r) ", upper=" (upper r) " ]")))

(defmethod print-method cantor.range.Box2 [r writer]
  (.write writer (str "[ lower=" (lower r) ", upper=" (upper r) " ]")))

(defmethod print-method cantor.range.Box3 [r writer]
  (.write writer (str "[ lower=" (lower r) ", upper=" (upper r) " ]")))
