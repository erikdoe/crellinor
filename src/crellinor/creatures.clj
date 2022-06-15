(ns crellinor.creatures
  (:require [clojure.core.strint :refer [<<]]
            [crellinor.parameters :refer [get-param]]
            [crellinor.utils :refer [log]]
            ))


; -- setting up plants

(defn new-plant []
  {:ep (get-param :plant-start-ep)
   }
  )


; -- setting up creatures

(defn new-creature [program]
  {:program program
   :bearing 0
   :ep      (get-param :creature-start-ep)
   :pc      0
   :cc      0
   })

(defn add-birth-data [creature [x y] t]
  (merge
    creature
    {:id        (apply str (interpose "-" [t x y]))
     :bcycle    t
     :lastprocd t
     }))


; -- functions for creatures

(defn age [creature]
  (- (:lastprocd creature) (:bcycle creature)))

(defn mature? [creature]
  (> (age creature) (+ (get-param :creature-start-ep) (get-param :eat-ep))))

(defn inc-attr [creature attr n]
  (assoc creature attr (+ (get creature attr) n)))

(defn dec-attr [creature attr]
  (assoc creature attr (dec (get creature attr))))

(defn get-pc [creature]
  (:pc creature))

(defn set-pc [creature new-pc]
  (assoc creature :pc new-pc))

(defn inc-pc [creature n]
  (assoc creature :pc (mod (+ (:pc creature) n) (count (:program creature)))))

(defn inc-pc-to-section [creature]
  (let [in-section (rem (:pc creature) (get-param :psection-length))]
    (if (> in-section 0)
      (inc-pc creature (- (get-param :psection-length) in-section))
      creature)
    ))

(defn dec-pc-to-section [creature]
  (let [section (quot (:pc creature) (get-param :psection-length))]
    (set-pc creature (* section (get-param :psection-length)))
    ))

(defn current-instr [creature]
  (nth (:program creature) (:pc creature)))


; -- output

(defn format-program [program]
  (apply str
         (map #(format "%02d%s; " %1 (apply str %2))
              (iterate #(+ % (get-param :psection-length)) 0)
              (partition-all (get-param :psection-length) program))))
