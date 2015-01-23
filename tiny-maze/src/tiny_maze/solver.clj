(ns tiny-maze.solver
  (require [clojure.core.logic :as l]
           [clojure.core.logic.fd :as fd]))


(def ex-maze [[:S 0 1]
              [1  0 1]
              [1  0 :E]])

(defn get-path-spots
  [maze]
  (for [x (range (count maze))
        y (range (count (first maze)))
        :when (#{0 :S :E} (get-in maze [x y]))]
    [x y]))

(defn get-spec-spot
  [maze path t]
  (for [c path
        :when (= t (get-in maze c))]
    c))


(l/defne blind-step
  [width height here there]
  ([_ _ [x y] [nx y]] (fd/+ x 1 nx) (fd/in nx width))
  ([_ _ [x y] [nx y]] (fd/+ nx 1 x) (fd/in nx width))
  ([_ _ [x y] [x ny]] (fd/+ y 1 ny) (fd/in ny height))
  ([_ _ [x y] [x ny]] (fd/+ ny 1 y) (fd/in ny height)))

(defn valid-step
  [path-spots width height c1 c2]
  (l/all
     (blind-step width height c1 c2)
     (l/membero c2 path-spots)))

(l/defne valid-path
  [path-spots width height start end path]
  ([_ _ _ _ _ [start end]] (valid-step path-spots width height start end))
  ([_ _ _ _ _ [start p . pr]]
     (valid-step path-spots width height start p)
     (l/fresh [pa]
              (l/appendo [p] pr pa)
              (valid-path path-spots width height p end pa))))

(defn solve-maze
  [maze]
  (let [path-spots (get-path-spots maze)
        start (first (get-spec-spot maze path-spots :S))
        end (first (get-spec-spot maze path-spots :E))
        width (apply fd/domain (range (count (first maze))))
        height (apply fd/domain (range (count maze)))]
    (->> (l/run 1 [q] (valid-path path-spots width height start end q))
         first
         (reduce #(assoc-in %1 %2 :x) maze))))
