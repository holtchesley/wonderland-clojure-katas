(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]))

(pldb/db-rel word-jump a b)

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(l/defne one-off
  [aw bw]
  ([[a] [b]] (l/!= a b))
  ([[a . ar] [b . ar]] (l/!= a b))
  ([[a . ar] [a . br]] (one-off ar br)))

(defn word-pairing
  [x]
  (let [seqqed (map seq x)]
    (map (fn [[a b]] [(apply str a) (apply str b)])
         (l/run* [q]
                 (l/fresh [a b]
                          (l/== q [a b])
                          (l/membero a seqqed)
                          (l/membero b seqqed)
                          (one-off a b))))))

(def pairtionary
  (->> words
       (group-by count)
       vals
       (map word-pairing)
       (filter (comp not empty?))
       (mapcat identity)
       (map (fn [[a b]] [word-jump a b]))
       (apply pldb/db)
       ))

(l/defne doublet-steps
  [start goal steps]
  ([_ _ [goal]] (word-jump start goal))
  ([_ _ [a . ar]]
     (word-jump start a)
     (doublet-steps a goal ar)))


(defn doublets [word1 word2]
  (let [rest-steps (first (pldb/with-db pairtionary (l/run 1 [q] (doublet-steps word1 word2 q))))]
    (if (nil? rest-steps)
      []
      (apply vector
             (conj rest-steps word1)))))
