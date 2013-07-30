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

(defn wrap-vector [boxing v]
  (map #(apply mod %) (map vector v boxing)))

(defn out-of-bounds [area position]
  (or (< (first position) 0)
    (< (second position) 0)
    (> (first position) (first area))
    (> (second position) (second area))))

(defn update-ship-position [ship]
  (let [updated (update-in ship [:position] + (:velocity ship))]
    (if (out-of-bounds (:window-dimensions SETTINGS) (:position updated))
      (update-in updated [:position] := (wrap-vector (:window-dimensions SETTINGS) (:position updated)))
      updated)))

(defn update-distance [shot]
  (update-in shot [:distance] + (length (:dir shot))))

(defn position [shot]
  (update-in shot [:position] + (:dir shot)))

(defn update-shot [shot]
  (let [updated (position (update-distance shot))]))

(defn update-shots [shots]
  (map update-shot (filter #(not (:removed %)) shots)))

(defn update-ship [ship]
  (let [moved (update-ship-position ship)]
    (update-in moved [:shots] := (update-shots (:shots moved)))))
