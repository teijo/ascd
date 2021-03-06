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

(defn spawn-ship []
  {:id 1
   :dirty true
   :name 'Name
   :shots []
   :energy 10
   :diameter 10
   :velocity (vec [-0.5 0.25])
   :heading (vec [1.0 0.5])
   :position (/ (vec (:window-dimensions SETTINGS)) 2)
   })

(defn update-all [object key-values]
  (reduce #(update-in % [(second %2)] (first %2) (last %2)) object key-values))

(defn wrap-vector [boxing v]
  (map #(apply mod %) (map vector v boxing)))

(defn out-of-bounds [area position]
  (or (< (first position) 0)
    (< (second position) 0)
    (> (first position) (first area))
    (> (second position) (second area))))

(defn update-position [ship]
  (let [updated (update-in ship [:position] + (:velocity ship))]
    (if (out-of-bounds (:window-dimensions SETTINGS) (:position updated))
      (update-in updated [:position] := (wrap-vector (:window-dimensions SETTINGS) (:position updated)))
      updated)))

(defn update-distance [shot]
  (update-in shot [:distance] + (length (:velocity shot))))

(defn move-shot [shot]
  (let [updated (update-position (update-distance shot))]
    (if (> (:distance updated) (:max-distance updated))
      (update-in updated [:removed] := true)
      updated)))

(defn update-shots [shots]
  (map move-shot (filter #(not (:removed %)) shots)))

(defn update-ship [ship]
  (let [moved (update-position ship)]
    (update-in moved [:shots] := (update-shots (:shots moved)))))

(defn player-hit? [ship shot]
  (< (length (- (:position shot) (:position ship)))
    (:diameter ship)))

(defn collect-hits [ship shots]
  (map #(update-in % [:hit] := (:id ship))
    (filter #(player-hit? ship %) shots)))

(defn inflict-damage [ship damage]
  (update-all ship [[- :energy damage]
                    [(some-fn true?) :dirty (> damage 0)]]))

(defn filter-out-by-id [ships ship]
  (filter #(not= (:id ship) (:id %)) ships))

(defn collect-all-hits [ships]
  (map (fn [player]
         (map (fn [enemy]
                (collect-hits player (:shots enemy)))
           (filter-out-by-id ships player))
         ) ships))

(defn update-damage [ships hits]
  (map (fn [ship] (inflict-damage ship (length (filter #(= (:hit %) (:id ship)) hits)))) ships))

(defn next-state [state]
  (let [moved-ships (map update-ship (:ships state))
        hits (collect-all-hits moved-ships)
        damaged-ships (update-damage moved-ships hits)]
    {:ships damaged-ships}))
