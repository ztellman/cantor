;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns cantor.test.range
  (:use [cantor]
        [clojure.test]
        [clojure.walk :only (postwalk-replace)]))

(def a1 (interval 0 1))
(def a2 (interval 2 3))
(def a3 (interval 0 3))

(def b1 (box2 (vec2 0 0) (vec2 1 1)))
(def b2 (box2 (vec2 2 2) (vec2 3 3)))
(def b3 (box2 (vec2 0 0) (vec2 3 3)))

(def c1 (box3 (vec3 0 0 0) (vec3 1 1 1)))
(def c2 (box3 (vec3 2 2 2) (vec3 3 3 3)))
(def c3 (box3 (vec3 0 0 0) (vec3 3 3 3)))

(defn create1 [x] x)
(defn create2 [x] (vec2 x x))
(defn create3 [x] (vec3 x x x))

(defmacro test-all [& body]
  `(do
     ~@(postwalk-replace {'r1 'a1, 'r2 'a2, 'r3 'a3, 'create 'create1} body)
     ~@(postwalk-replace {'r1 'b1, 'r2 'b2, 'r3 'b3, 'create 'create2} body)
     ~@(postwalk-replace {'r1 'c1, 'r2 'c2, 'r3 'c3, 'create 'create3} body)))

(deftest range-test

  (testing "Ranges"
    (test-all
     (is (inside? r1 (create 0.5)))
     (is (not (inside? r2 (create 0.5))))

     (is (= r1 (intersection r1 r3)))
     (is (= r2 (intersection r2 r3)))
     (is (= nil (intersection r1 r2)))

     (is (= r3 (union r1 r2))))))