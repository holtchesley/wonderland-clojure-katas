(ns magic-square.puzzle
  (require [clojure.core.logic :as l]
           [clojure.core.logic.fd :as fd]))

(defn reduce-plus
  [coll sum]
  (loop [s-coll coll
         pyr []]
    (if (= 2 (count s-coll))
      (conj pyr [(first s-coll) (second s-coll) sum])
      (let [mpyr (->> s-coll
                      (partition 2 2 [0])
                      (map (fn [[a b]] [a b (l/lvar)])))]
        (recur (map #(nth % 2) mpyr)
               (into pyr mpyr))))))

(defn magic-squares-of-size
  [n]
  (let [num (* n n)
        magic-const (/ (* n (+ 1 (* n n))) 2)
        mag-dom (apply fd/domain (apply vector (conj (range 1 (inc num)) magic-const)))
        vars (repeatedly num l/lvar)
        rows (into [] (map vec (partition n vars)))
        rows-trips (into [] (mapcat #(reduce-plus % magic-const) rows))
        cols (apply map vector rows)
        cols-trips (into [] (mapcat #(reduce-plus % magic-const) cols))
        diag-1 (map-indexed #(nth %2 %1) rows)
        diag-2 (map-indexed #(nth %2 (- (dec n) %1)) rows)
        diags-trips (into (into [] (reduce-plus diag-1 magic-const)) (reduce-plus diag-2 magic-const))
        ]
    (->> (l/run 1 [q]
                 (l/everyg #(fd/in % mag-dom) vars)
                 (fd/distinct vars)
                 (l/everyg #(apply fd/+ %) rows-trips)
                 (l/everyg #(apply fd/+ %) cols-trips)
                 (l/everyg #(apply fd/+ %) diags-trips)
                 (l/== q vars))
         first
         (partition n)
         (map #(apply vector %))
         (apply vector))
    ))



(def values 3)

(defn magic-square [values]
  (magic-squares-of-size values))
