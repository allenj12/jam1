(ns jam1.core
  (:use [overtone.core]
        [overtone.inst.sampled-piano]
        [overtone.inst.drum]
        [overtone.inst.synth]))

(defn quantize
  [midi field]
  (let [nt (note midi)]
    (if (some #{nt} field)
      nt
      (loop [x 1]
        (let [up (int (+ nt x))
              down (int (- nt x))]
          (cond
           (some #{up} field) up
           (some #{down} field) down
           :else (recur (inc x))))))))

(def scl (scale-field :E :aeolian))

(def lpitches [:G3 :G3 :A3 :B3])

(def metro (metronome 120))

(def root (atom :E3))

(defn right-hand
  [beat dur]
  (at (metro beat)
      (sampled-piano  (quantize
                       (int (cosr
                             0
                             (+ (note @root) 24)
                             (cosr 0 5 3 1/2)
                             7/3))
                       scl))
      (when (< (rand) 0.4)
        (sampled-piano  (quantize
                         (int (+ 7 (cosr
                                    0
                                    (+ (note @root) 24)
                                    (cosr 0 5 3 1/2)
                                    7/3)))
                         scl))))
  (apply-by (metro (+ beat (* 0.5 dur))) right-hand (+ beat dur) dur []))

(defn left-hand
  [beat notes dur]
  (let [n      (first notes)
        notes  (next notes)]
    (when n
     (when (= 0 (mod beat 8))
       (reset! root (rand-nth
                  (remove
                   #(= @root %) '(:E3 :D3 :C3)))))
      (at (/ (metro beat) 2)
          (sampled-piano (note @root)))
      (at (metro beat)
          (sampled-piano (note n)))
      (apply-by (metro (+ beat (* 0.5 dur))) left-hand (+ beat dur) notes 1 []))))

(do
  (left-hand (metro) (cycle lpitches) 1)
  (right-hand (metro) 1/4))

(stop)
