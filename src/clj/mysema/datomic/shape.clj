(ns mysema.datomic.shape
  (:require [clojure.edn :as edn]
            [slingshot.slingshot :refer [throw+ try+]])
  )


(defn read-shape
  [shape-map k str]
  (when str
    (let [shapes (try+
                   (edn/read-string str)
                   (catch Object _
                     (throw+ {:invalid-shape-string str})))]

      (when-not (coll? shapes)
        (throw+ {:shape-not-collection shapes}))

      ;; For now just go through all values through input
      ;; TODO Add hierachies if needed
      {k (for [shape-name shapes
               :let [shape (shape-name shape-map)]
               :when shape]
           shape)})))


;;TODO Dunno is this very useful at all?
(defn combine-shapes
  [ks & shape-maps]
  (->> (select-keys (apply merge shape-maps) ks)
       vals
       (apply concat)))