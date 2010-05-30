;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:skip-wiki true} cantor.vector
  (:use [clojure.contrib.def :only (defmacro-)]
        [cantor.misc :only (radians degrees)]
        [cantor.utils :only (with-tags)]))

;;;

(defprotocol Arithmetic
  (add [a] [a b])
  (sub [a] [a b])
  (mul [a] [a b])
  (div [a] [a b]))

(defprotocol Tuple
  (map- [t f] [a b f] [a b rest f])
  (all- [t f] [a b f])
  (dimension [t]))

(defprotocol Cartesian
  (dot [a b])
  (polar [v]))

(defprotocol Polar
  (cartesian [p]))

(defn map*
  "Same as map, but returns a tuple of the same type as the input(s)."
  ([f v] (map- v f))
  ([f a b] (map- a b f))
  ([f a b & rest] (map- a b rest f)))

(defn all?
  "Returns true if all components of the input tuple satisfy 'f'."
  ([f a] (all- a f))
  ([f a b] (all- a b f)))

;;;

(defrecord Polar2 [#^double theta #^double r]

  clojure.lang.IFn
  (invoke [_ n] (condp = n
                  0 theta
                  1 r)))

(with-tags {u Vec2, v Vec2}
  (defrecord Vec2 [#^double x #^double y]

    clojure.lang.IFn
    (invoke [_ n] (condp = n
                    0 x
                    1 y))

    Arithmetic
    (add [this] this)
    (add [_ v] (Vec2. (+ x (.x v)) (+ y (.y v))))
    (sub [this] (Vec2. (- x) (- y)))
    (sub [_ v] (Vec2. (- x (.x v)) (- y (.y v))))
    (mul [this] this)
    (mul [_ b]
          (if (number? b)
            (let [b (double b)]
              (Vec2. (* x b) (* y b)))
            (let [v b]
              (Vec2. (* x (.x v)) (* y (.y v))))))
    (div [this] this)
    (div [_ b]
          (if (number? b)
            (let [b (double b)]
              (Vec2. (/ x b) (/ y b)))
            (let [v b]
              (Vec2. (/ x (.x v)) (/ y (.y v))))))

    Tuple
    (map- [_ f]
          (Vec2. (double (f x)) (double (f y))))
    (map- [u v f]
          (Vec2. (f (.x u) (.x v))
                 (f (.y u) (.y v))))
    (map- [u v rest f]
          (let [vs (list* u v rest)]
            (Vec2. (apply f (map #(.x #^Vec2 %) vs))
                   (apply f (map #(.y #^Vec2 %) vs)))))
    (all- [_ f] (and (f x) (f y)))
    (all- [_ v f] (and (f x (.x v)) (f y (.y v))))
    (dimension [_] 2)
    
    Cartesian
    (dot [_ v] (+ (* x (.x v)) (* y (.y v))))
    (polar [v] (Polar2. (degrees (Math/atan2 y x)) (Math/sqrt (dot v v))))))

(with-tags {p Polar2}
 (extend-type Polar2
  Polar
  (cartesian [p]
   (let [theta (radians (.theta p))]
     (Vec2. (* (.r p) (Math/cos theta)) (* (.r p) (Math/sin theta)))))))

(defrecord Polar3 [#^double theta #^double phi #^double r]

  clojure.lang.IFn
  (invoke [_ n] (condp = n
                  0 theta
                  1 phi
                  2 r)))

(with-tags {u Vec3, v Vec3}
 (defrecord Vec3 [#^double x #^double y #^double z]

   clojure.lang.IFn
   (invoke [_ n] (condp = n
                   0 x
                   1 y
                   2 z))
   
   Arithmetic
   (add [this] this)
   (add [_ v] (Vec3. (+ x (.x v)) (+ y (.y v)) (+ z (.z v))))
   (sub [_] (Vec3. (- x) (- y) (- z)))
   (sub [_ v] (Vec3. (- x (.x v)) (- y (.y v)) (- z (.z v))))
   (mul [this] this)
   (mul [_ b]
         (if (number? b)
           (let [b (double b)]
             (Vec3. (* x b) (* y b) (* z b)))
           (let [v b]
             (Vec3. (* x (.x v)) (* y (.y v)) (* z (.z v))))))
   (div [this] this)
   (div [_ b]
         (if (number? b)
           (let [b (double b)]
             (Vec3. (/ x b) (/ y b) (/ z b)))
           (let [v b]
             (Vec3. (/ x (.x v)) (/ y (.y v)) (/ z (.z v))))))

   Tuple
   (map- [_ f]
         (Vec3. (double (f x)) (double (f y)) (double (f z))))
   (map- [u v f]
         (Vec3. (f (.x u) (.x v))
                (f (.y u) (.y v))
                (f (.z u) (.z v))))
   (map- [u v rest f]
         (let [vs (list* u v rest)]
           (Vec3. (double (apply f (map #(.x #^Vec3 %) vs)))
                  (double (apply f (map #(.y #^Vec3 %) vs)))
                  (double (apply f (map #(.z #^Vec3 %) vs))))))
   (all- [_ f] (and (f x) (f y) (f z)))
   (all- [_ v f] (and (f x (.x v)) (f y (.y v)) (f z (.z v))))
   (dimension [_] 3)
   
   Cartesian
   (dot [_ v] (+ (* x (.x v)) (* y (.y v)) (* z (.z v))))
   (polar [v]
          (let [len (Math/sqrt (dot v v))
                p (polar (Vec2. x z))]
            (Polar3. (.theta #^Polar2 p)
                     (degrees (Math/asin (/ y len)))
                     len)))))

(with-tags {p Polar3}
 (extend-type Polar3
  Polar
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

(defn cartesian?
  "Returns true is 'c' is a cartesian vector."
  [c]
  (instance? Cartesian c))

(defn polar?
  "Returns true if 'p' is a polar coordinate."
  [p]
  (instance? Polar p))

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