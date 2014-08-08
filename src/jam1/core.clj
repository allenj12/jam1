(ns jam1.core
  (:use [overtone.core]
        [overtone.inst.sampled-piano]
        [overtone.inst.drum]))

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
  [nome]
  (let [beat (nome)]
    (at (nome beat)
        (sampled-piano  (quantize
                         (int (cosr
                               0
                               (+ (note @root) 24)
                               (cosr 0 5 3 1/2)
                               7/3))
                                 scl)))
    (apply-by (nome (inc beat)) right-hand nome [])))

(defn left-hand
  [nome notes]
  (let [n      (first notes)
        notes  (next notes)
        beat (nome)]
    (when n
     (when (= 0 (mod beat 8))
       (reset! root (rand-nth
                  (remove
                   #(= @root %) '(:E3 :D3 :C3)))))
      (at (/ (nome beat) 2)
          (sampled-piano (note @root)))
      (at (nome beat)
(scale-field :c :aeolian)          (sampled-piano (note n)))
      (apply-by (nome (inc beat)) left-hand nome notes []))))

(do
  (left-hand metro (cycle lpitches))
  (right-hand metro)
  )

(stop)
