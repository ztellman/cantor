;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns cantor.vector
  (:use [cantor misc]
        [clojure.contrib.def :only (defmacro-)]
        [clojure.walk :only (postwalk postwalk-replace)])
  (:require [cantor :as core]))

;;

(defmacro- tag-vars [types body]
  (let [types (into {} (map (fn [[k v]] [k (with-meta k (merge (meta k) {:tag v}))]) types))]
    (->> body
         (postwalk-replace types)
         (postwalk #(if (vector? %) (postwalk-replace (zipmap (vals types) (keys types)) %) %)))))

(defrecord Polar2 [#^double theta #^double r]

  clojure.lang.IFn
  (invoke [_ n] (condp = n
                  0 theta
                  1 r)))

(tag-vars
 {v Vec2}
 (defrecord Vec2 [#^double x #^double y]

   clojure.lang.IFn
   (invoke [_ n] (condp = n
                   0 x
                   1 y))

   core/Arithmetic
   (add [_ v] (Vec2. (+ x (.x v)) (+ y (.y v))))
   (sub [_ v] (Vec2. (- x (.x v)) (- y (.y v))))
   (mul [_ b]
        (if (number? b)
          (let [b (double b)]
            (Vec2. (* x b) (* y b)))
          (let [v b]
            (Vec2. (* x (.x v)) (* y (.y v))))))
   (div [_ b]
        (if (number? b)
          (let [b (double b)]
            (Vec2. (/ x b) (/ y b)))
          (let [v b]
            (Vec2. (/ x (.x v)) (/ y (.y v))))))

   core/Cartesian
   (dot [_ v] (+ (* x (.x v)) (* y (.y v))))
   
   (map- [_ f]
          (Vec2. (double (f x)) (double (f y))))
   (map- [v f rest]
         (let [vs (cons v rest)]
           (Vec2. (double (apply f (map #(.x #^Vec2 %) vs)))
                  (double (apply f (map #(.y #^Vec2 %) vs))))))
   (polar [v] (Polar2. (degrees (Math/atan2 y x)) (core/length v)))))

(tag-vars
 {p Polar2}
 (extend-type Polar2
  core/Polar
  (cartesian [p]
   (let [theta (radians (.theta p))]
     (Vec2. (* (.r p) (Math/cos theta)) (* (.r p) (Math/sin theta)))))))

(defrecord Polar3 [#^double theta #^double phi #^double r]

  clojure.lang.IFn
  (invoke [_ n] (condp = n
                  0 theta
                  1 phi
                  2 r)))

(tag-vars
 {v Vec3}
 (defrecord Vec3 [#^double x #^double y #^double z]

   clojure.lang.IFn
   (invoke [_ n] (condp = n
                   0 x
                   1 y
                   2 z))
   
   core/Arithmetic
   (add [_ v] (Vec3. (+ x (.x v)) (+ y (.y v)) (+ z (.z v))))
   (sub [_ v] (Vec3. (- x (.x v)) (- y (.y v)) (- z (.z v))))
   (mul [_ b] (if (number? b)
                (let [b (double b)]
                  (Vec3. (* x b) (* y b) (* z b)))
                (let [v b]
                  (Vec3. (* x (.x v)) (* y (.y v)) (* z (.z v))))))
   (div [_ b] (if (number? b)
                (let [b (double b)]
                  (Vec3. (/ x b) (/ y b) (/ z b)))
                (let [v b]
                  (Vec3. (/ x (.x v)) (/ y (.y v)) (/ z (.z v))))))
   
   core/Cartesian
   (dot [_ v] (+ (* x (.x v)) (* y (.y v)) (* z (.z v))))
   
   (map- [_ f]
          (Vec3. (double (f x)) (double (f y)) (double (f z))))
   (map- [v f rest]
         (let [vs (cons v rest)]
           (Vec3. (double (apply f (map #(.x #^Vec3 %) vs)))
                  (double (apply f (map #(.y #^Vec3 %) vs)))
                  (double (apply f (map #(.z #^Vec3 %) vs))))))
   (polar [v]
          (let [len (core/length v)]
            (Polar3. (.theta (core/polar (Vec2. x z)))
                     (degrees (Math/asin (/ y len)))
                     len)))))

(tag-vars
 {p Polar3}
 (extend-type Polar3
  core/Polar
  (cartesian [p]
   (let [theta (radians (.theta p))
         phi (radians (- 90 (.phi p)))
         ts (Math/sin theta)
         tc (Math/cos theta)
         ps (Math/sin phi)
         pc (Math/cos phi)
         r (.r p)]
     (Vec3. (* r ps tc) (* r pc) (* r ps ts))))))

;;;

(defn vec2
  "Creates a 2-vector."
  [x y]
  (Vec2. x y))

(defn polar2
  "Creates a 2-D polar coordinate.
   (polar theta) => (polar theta 1)"
  ([theta] (polar2 theta 1))
  ([theta r] (Polar2. theta r)))

(defn vec3
  "Creates a 3-vector.
   (vec3 (vec2 x y) z) => (vec3 x y z)"
  ([#^Vec2 v z] (vec3 (.x v) (.y v) z))
  ([x y z] (Vec3. x y z)))

(defn polar3
  "Creates a 3-D polar coordinate.
   (polar3 theta phi) => (polar3 theta phi 1)"
  ([theta phi] (polar3 theta phi 1))
  ([theta phi r] (Polar3. theta phi r)))

(defn vec2?
  "Returns true if 'v' is a 2-vector"
  [v]
  (instance? Vec2 v))

(defn vec3?
  "Returns true if 'v' is a 2-vector"
  [v]
  (instance? Vec3 v))

(defn vec?
  "Returns true if 'v' is a vector."
  [v]
  (or (vec2? v) (vec3? v)))

(defn polar2?
  "Returns true if 'p' is a 2-vector in polar coordinates"
  [p]
  (instance? Polar2 p))

(defn polar3?
  "Returns true if 'p' is a 3-vector in polar coordinates"
  [p]
  (instance? Polar3 p))

(defn polar?
  "Returns true if 'p' is a polar coordinate."
  [p]
  (or (polar2? p) (polar3? p)))

(defn cross
  "Returns the cross product of two 3-vectors."
  [#^Vec3 a #^Vec3 b]
  (Vec3. (- (* (.y a) (.z b)) (* (.z a) (.y b)))
         (- (* (.z a) (.x b)) (* (.x a) (.z b)))
         (- (* (.x a) (.y b)) (* (.y a) (.x b)))))

;;;

(defmethod print-method cantor.vector.Vec3 [v writer]
  (.write writer (str "[ x=" (.x #^Vec3 v) ", y=" (.y #^Vec3 v) ", z=" (.z #^Vec3 v) " ]")))

(defmethod print-method cantor.vector.Polar3 [p writer]
  (.write writer (str "[ theta=" (.theta #^Polar3 p) ", phi=" (.phi #^Polar3 p) ", r=" (.r #^Polar3 p) " ]")))

(defmethod print-method cantor.vector.Vec2 [v writer]
  (.write writer (str "[ x=" (.x #^Vec2 v) ", y=" (.y #^Vec2 v) " ]")))

(defmethod print-method cantor.vector.Polar2 [p writer]
  (.write writer (str "[ theta=" (.theta #^Polar2 p) ", r=" (.r #^Polar2 p) " ]")))