(ns alphabet-cipher.coder
  (require [clojure.core.logic :as l]
           [clojure.core.logic.pldb :as pldb]))

(pldb/db-rel encodes k m e)

(def alphabet
  (into [] (map char (range
                      (int \a)
                      (inc (int \z))))))

(defn make-cipher
  [alphabet]
  (let [alphabet-size (count alphabet)
        fa (int (first alphabet))
        backref (fn [i] (char (+ fa i)))
        ]
    (apply pldb/db
           (for [m (range 0 alphabet-size)
                 k (range 0 alphabet-size)]
             [encodes (backref m) (backref k) (nth alphabet (mod (+ k m) alphabet-size))]))))

(def cipher (make-cipher alphabet))

(l/defne mappo
  [relation colla collb result]
  ([_ [] [] []])
  ([_ [a . ar] [b . br] [r . rr]]
     (relation a b r)
     (mappo relation ar br rr)))

(l/defne matching-cycle-helper
  [cycle-coll rest-coll to-match trunc-coll]
  ([_ _ [] []])
  ([_ [c . cr] [m . mr] [c . tr]] (matching-cycle-helper cycle-coll cr mr tr))
  ([_ [] _ _] (matching-cycle-helper cycle-coll cycle-coll to-match trunc-coll)))

(l/defne matching-cycle
  [cycle-coll to-match trunc-coll]
  ([_ _ _] (matching-cycle-helper cycle-coll cycle-coll to-match trunc-coll)))



(defn encodes-string
  [keyword message encoding]
  (let [kt (seq keyword)
        m (if (l/lvar? message) message (seq message))
        e (if (l/lvar? encoding) encoding (seq encoding))
        t (if (l/lvar? e) m e)]
    (pldb/with-db
      cipher
      (l/all
       (l/fresh
        [k]
        (matching-cycle kt t k)
        (mappo encodes k m e))))))

(defn encode
  [keyword message]
  (apply str (first
              (pldb/with-db cipher
                (l/run 1
                       [q]
                       (encodes-string keyword message q))))))

(defn decode [keyword message]
  (apply str
         (first
          (pldb/with-db cipher
            (l/run 1
                   [q]
                   (encodes-string keyword q message))))))
