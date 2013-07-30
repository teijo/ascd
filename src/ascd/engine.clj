(ns ascd.engine
  (:use clojure.core.matrix)
  (:use clojure.core.matrix.operators)
  (:refer-clojure :exclude [* - + == /])(:refer-clojure :exclude [* - + == /])
  (:use [clojure.test]))

(set-current-implementation :vectorz)


(def SETTINGS
  { :channel 0
    :dump false
    :max-energy 10
    :max-velocity 1.5
    :player { :name nil }
    :server "127.0.0.1"
    :framerate 60
    :tickrate 100
    :state-throttle 100
    :window-dimensions [800 600]
    :acceleration {  :value nil
                     :base 0.04
                     :step 0.01 }
    :turn {          :value nil
                     :base 0.04
                     :step 0.01 }
    :shot-velocity { :value nil
                     :base 4.0
                     :step 0.5 }
    :shot-delay {    :value nil
                     :base 10
                     :step -3 }
    :shot-range {    :value nil
                     :base 600
                     :step 100 }
    :ship-size {     :value nil
                     :base 15
                     :step -4 }
    })

(defn update-ship-position [ship]
  (update-in ship [:position] + (:velocity ship)))

(defn wrap-vector [boxing v]
  (map #(apply mod %) (map vector v boxing)))

(defn update-shot [shot]
  (update-in shot [:position] + (:dir shot)))

(defn out-of-bounds [area position]
  (or (< (first position) 0)
    (< (second position) 0)
    (> (first position) (first area))
    (> (second position) (second area))))
