(ns ascd.test.engine
  (:use ascd.engine)
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.operators)
  (:refer-clojure :exclude [* - + == /])(:refer-clojure :exclude [* - + == /])
  (:use [clojure.test]))

(set-current-implementation :vectorz)

(def ship
  {:id 1
   :player { :name 'Name }
   :shots (list
            { :distance 1
              :max-distance 2
              :position (vec [1 2])
              :dir (vec [0 1])
              :removed false })
   :energy 10
   :diameter 10
   :velocity (vec [1 0])
   :heading (vec [1 0])
   :position (vec [4 4])
   })

(deftest test-test-matrix
  (is (=
        (+ (matrix [1 2]) (matrix [1 2]))
        (matrix [2 4]))))

(deftest test-test-vec
  (is (=
        (+ (vec [1 2]) (vec [1 2]))
        (vec [2 4]))))

(deftest test-ship-id
  (is (= (:id ship) 1)))

(deftest test-ship-update
  (is (= (:position (update-position ship)) (vec [5 4]))))

(deftest test-ship-update-wrap
  (is (= (vec [4 14])
        (:position (update-position
                     (update-in ship [:velocity] := (vec [0 610])))))))

(deftest test-on-boundaries
  (is (not (out-of-bounds [1 1] [0 0]))))

(deftest test-inside-boundaries
  (is (not (out-of-bounds [2 2] [1 1]))))

(deftest test-outside-boundaries
  (is (out-of-bounds [1 1] [1 2])))

(deftest test-negative-outside-boundaries
  (is (out-of-bounds [1 1] [-1 1])))

(deftest test-wrap-vector
  (is (= (vec [1 0]) (wrap-vector (vec [2 2]) (vec [-1 2])))))

(deftest test-shot-distance
  (is (= 1.0 (:distance (update-distance {:distance 0 :position (vec [0 0]) :velocity (vec [1 0])})))))

(deftest test-shot-wrap
  (is (let [moved-shot (move-shot {:distance 0 :max-distance 500
                                   :removed false :position (vec [0 0])
                                   :velocity (vec [-790 610])})]
        (and (= (:position moved-shot) (vec [10, 10]))
          (= (:removed moved-shot) true)))))

(deftest test-hit
  (is (player-hit?
        {:position (vec [0 0]) :diameter 10}
        {:position (vec [9 0])})))

(deftest test-miss
  (is (not (player-hit?
             {:position (vec [0 0]) :diameter 10}
             {:position (vec [9.9 9.9])}))))

(deftest test-collect-hits
  (is (= [{:position (:position ship) :hit (:id ship)}]
        (collect-hits ship
        [{:position (:position ship)}
         {:position (+ (:position ship) (vec [10 10]))}]))))

(deftest test-inflict-damage
  (is (= {:dirty true :energy 5} (inflict-damage {:dirty false :energy 10} 5))))

(deftest test-next-state
  (is (= {:ships [ship]}) (next-state {:ships [ship]})))
