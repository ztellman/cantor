;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns cantor.test
  (:use [cantor])
  (:use [clojure.test]))

(deftest vector-test

  (def a2 (vec2 1 1))
  (def b2 (vec2 2 2))

  (def a3 (vec3 1 1 1))
  (def b3 (vec3 2 2 2))
  
  (testing "Addition"
    (is (= (vec2 3 3) (add a2 b2)))
    (is (= (vec3 3 3 3) (add a3 b3))))

  (testing "Subtraction"
    (is (= a2 (sub b2 a2)))
    (is (= a3 (sub b3 a3))))

  (testing "Multiplication"
    (is (= b2 (mul a2 b2)))
    (is (= b3 (mul a3 b3))))

  (testing "Division"
    (is (= (vec2 0.5 0.5) (div a2 b2)))
    (is (= (vec3 0.5 0.5 0.5) (div a3 b3))))

  (is (= 5 (length (vec2 3 4))))
  (is (= 5 (length (vec3 0 3 )))))
