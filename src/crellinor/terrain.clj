(ns crellinor.terrain
  (:require [crellinor.utils :refer [sq srand-int]]
            [crellinor.parameters :refer [get-param]]
            ))


; -- helper functions

(defn- pos->idx [[x y]]
  (+ x (* y (get-param :world-size))))

(defn- idx->pos [idx]
  [(rem idx (get-param :world-size)) (quot idx (get-param :world-size))])


; -- setting up the terrain

(def terrain [])

(def locations-with-creature
  (ref #{}))

(defn reset-terrain! []
  (def terrain (->> (range (sq (get-param :world-size)))
                    (map #(ref {:pos (idx->pos %)}))
                    vec))
  (dosync
    (ref-set locations-with-creature #{})))


; -- high level functions to query and change terrain

(defn- ref-at [pos]
  (nth terrain (pos->idx pos)))

(defn- data-at [pos]
  (deref (ref-at pos)))


(defn plant-at [pos]
  (:plant (data-at pos)))

(defn creature-at [pos]
  (:c0 (data-at pos)))


(defn- plant-with-pos [loc-data]
  (if-let [plant (:plant loc-data)]
    (assoc plant :pos (:pos loc-data))))

(defn all-plants []
  (filter some? (map #(plant-with-pos (deref %)) terrain)))


(defn- creature-with-pos-from-loc [loc-data]
  (if-let [c (:c0 loc-data)]
    (assoc c :pos (:pos loc-data))))

(defn all-creatures-from-terrain []
  (filter some? (map #(creature-with-pos-from-loc (deref %)) terrain)))

(defn all-creatures []
  (map #(:c0 (deref %)) (deref locations-with-creature)))


(defn upsert-creature-at! [pos creature]
  (let [lref (ref-at pos)]
    (alter locations-with-creature #(conj % lref))
    (alter lref #(assoc % :c0 creature))))

(defn remove-creature-at! [pos]
  (let [lref (ref-at pos)]
    (alter locations-with-creature #(disj % lref))
    (alter lref #(dissoc % :c0))))

(defn
  move-creature! [creature old-pos new-pos]
  (if (nil? (:c0 (data-at new-pos)))
    (do
      (remove-creature-at! old-pos)
      (upsert-creature-at! new-pos creature))
    ))


(defn upsert-plant-at! [pos plant]
  (let [lref (ref-at pos)]
    (alter lref #(assoc % :plant plant))))

(defn remove-plant-at! [pos]
  (let [lref (ref-at pos)]
    (alter lref #(dissoc % :plant))))


; -- calculating positions


(defn rand-pos []
  [(srand-int (get-param :world-size)) (srand-int (get-param :world-size))])

(defn- dpos [bearing n]
  (cond
    (= 0 bearing) [0 (* -1 n)]
    (= 90 bearing) [n 0]
    (= 180 bearing) [0 n]
    (= 270 bearing) [(* -1 n) 0]
    ))

(defn- add-to-pos [[x0 y0] [dx dy]]
  [(mod (+ x0 dx) (get-param :world-size))
   (mod (+ y0 dy) (get-param :world-size))])

(defn pos-ahead
  ([pos bearing]
   (add-to-pos pos (dpos bearing 1)))
  ([pos bearing count]
   (map #(add-to-pos pos (dpos bearing %)) (range 1 (inc count)))))

(defn free-pos-near [pos]
  (first
    (filter #(nil? (creature-at %))
            (map #(add-to-pos pos (dpos % 1))
                 [0 90 180 270]))))


; -- debugging output

(defn- str-loc [loc-data]
  (if (some? (:c0 loc-data))
    "*"
    (if (some? (:plant loc-data))
      "."
      " ")))

(defn- str-rows [coll n]
  (if (>= n (count coll))
    (apply str coll)
    (str (apply str (take n coll)) "\n"
         (str-rows (drop n coll) n))))

(defn- str-terrain [loc-refs]
  (str-rows (map #(str-loc (deref %)) loc-refs) (get-param :world-size)))

(defn- str-creature [creature]
  (str (:id creature) ": " creature))

(defn- str-creatures [creatures]
  (apply str (interpose "\n" (map str-creature creatures))))

(defn ascii-map []
  (str
    (str-terrain terrain)
    "\n\n"
    (str-creatures (all-creatures))
    "\n"
    ))

