;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns cantor.test.vector
  (:use [cantor]
        [clojure.test]
        [clojure.walk :only (postwalk-replace)]))

(def a1 (vec2 1 1))
(def a2 (vec2 2 2))
(def a3 (vec2 3 3))

(def b1 (vec3 1 1 1))
(def b2 (vec3 2 2 2))
(def b3 (vec3 3 3 3))

(def c1 (vec4 1 1 1 1))
(def c2 (vec4 2 2 2 2))
(def c3 (vec4 3 3 3 3))

(def n1 1)
(def n2 2)
(def n3 3)

(defmacro test-all [& body]
  `(do
     ~@(postwalk-replace {'v1 'a1, 'v2 'a2, 'v3 'a3} body)
     ~@(postwalk-replace {'v1 'b1, 'v2 'b2, 'v3 'b3} body)
     ~@(postwalk-replace {'v1 'c1, 'v2 'c2, 'v3 'c3} body)
     ~@(postwalk-replace {'v1 'n1, 'v2 'n2, 'v3 'n3} body)))

(deftest vector-test
  
  (testing "Addition"
    (test-all
     (is (= v3 (add v1 v2)))
     (is (= v3 (add v1 v1 v1)))))

  (testing "Subtraction"
    (test-all
     (is (= v1 (sub v3 v2)))
     (is (= v1 (sub v3 v1 v1)))))

  (testing "Multiplication"
    (test-all
     (is (= v2 (mul v1 v2)))
     (is (= v2 (mul v1 2)))
     (is (= v2 (mul v1 2 v1)))
     (is (= v2 (mul v1 2 1)))))

  (testing "Division"
    (test-all
     (is (= v1 (div v2 v2)))
     (is (= v1 (div v2 2)))
     (is (= v1 (div v2 2 v1)))
     (is (= v1 (div v2 2 1)))))

  (testing "Length"
    (is (= 5 (length (vec2 3 4))))
    (is (= 5 (length (vec3 0 3 4))))))

