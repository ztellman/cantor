;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns cantor.shape
  (:use [clojure.contrib.def :only (defmacro-)]
        [clojure.walk :only (postwalk postwalk-replace)])
  (:require [cantor.vector :as vec])
  (:import [cantor.vector Vec2 Vec3]))

;;;

(defmacro- tag-vars [types body]
  (let [types (into {} (map (fn [[k v]] [k (with-meta k (merge (meta k) {:tag v}))]) types))]
    (->> body
         (postwalk-replace types)
         (postwalk #(if (vector? %) (postwalk-replace (zipmap (vals types) (keys types)) %) %)))))

(defn map*
  "Same as map, but returns a vector of the same type as the input(s)."
  ([f v] (vec/map* v f))
  ([f v & rest] (vec/map* v f rest)))

(defn all*
  "Returns true if all components of the input vector satisfy 'f'."
  ([f a] (vec/all* a f))
  ([f a b] (vec/all* a b f)))

;;;

(defprotocol Shape
  (intersects? [a b]))

;;;

(defrecord Box2 [#^Vec2 ul #^Vec2 lr])

(tag-vars
 {a Box2, b Box2}
 (defn- box2-box2-intersects? [a b]
   (and
    (all* >= (.lr a) (.ul b))
    (all* <= (.ul a) (.lr b)))))

(extend-type Box2
  Shape
  (intersects? [a b]
    (if (instance? Box2 b)
      (box2-box2-intersects? a b)
      (intersects? b a))))

(tag-vars
 {a Box2, b Box2}
 (defn box2 [a b]
   (Box2. (map* min a b) (map* max a b))))

(defn box2?
  "Returns true if 'b' is a 2-D box."
  [b]
  (instance? Box2 b))

;;;

(defrecord Box3 [#^Vec3 ul #^Vec3 lr])

(tag-vars
 {a Box3, b Box3}
 (defn- box3-box3-intersects? [a b]
   (and
    (all* >= (.lr a) (.ul b))
    (all* <= (.ul a) (.lr b)))))

(extend-type Box3
  Shape
  (intersects? [a b]
    (if (isa? (type b) Box3)
      (box3-box3-intersects? a b)
      (intersects? b a))))

(tag-vars
 {a Box3, b Box3}
 (defn box3
   "Creates a 3-D box."
   [ul lr]
   (Box3. (map* min ul lr) (map* max ul lr))))

(defn box3?
  "Returns true if 'b' is a 3-D box."
  [b]
  (instance? Box3 b))

;;;

(defn box?
  "Returns true if 'b' is a box."
  [b]
  (or (box2? b) (box3? b)))