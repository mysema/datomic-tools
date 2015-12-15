(ns mysema.datomic.core
  (:require [clojure.core.async :as a :refer (>!! <! >! go-loop)]
            [datomic.api :as d]))

(defn datom
  [db attr id]
  (first (d/datoms db :avet attr id)))

(defn entid
  "Returns entity id from given identity attribute value"
  [db attr id]
  (:e (datom db attr id)))

(defn ent-timestamp
  "The timestamp of the entity transaction"
  [db eid]
  (let [txid (:tx (first (d/datoms db :eavt eid)))]
    (:v (first (d/datoms db :eavt txid :db/txInstant)))))


(defn add-timestamp
  [db k m]
  (if (:db/id m)
    (assoc m k (ent-timestamp db (:db/id m)))
    m))

(defn pull-ts
  ([db shape id] (pull-ts db shape id :modified))
  ([db shape id k]
   (->> (d/pull db shape id)
        (add-timestamp db k))))

(defn pull-many-ts
  ([db shape eids] (pull-many-ts db shape eids :modified))
  ([db shape eids k]
   (map (partial add-timestamp db k)
        (d/pull-many db shape eids))))


(defn as-required-ref
  "Helper to form required reference"
  ([k v] (as-required-ref k identity v))
  ([k f v] {:db/id [k f (v)]}))

(defn as-required-refs
  ([k vals] (as-required-refs k identity vals))
  ([k f vals] (map (partial as-required-ref k f) vals)))

;; probably not needed
;; use {:db/id lookup-ref} instead, will throw on transaction, if
;; lookupref is not found
(defn required-entid
  "Throws if entityid is not found"
  [db attr id]
  (if-let [eid (entid db attr id)]
    eid
    (throw (ex-info "Required entity is not found" {attr id}))))

(defn refids
  "Returns all reference ids for the given entitys attribute"
  [db eid attr]
  (map :v (d/datoms db :eavt eid attr)))

;; probably not needed
(defn upsertid
  "Return either the entity id or tempid, if identity value is not present yet"
  [db part attr id]
  (or (entid db attr id) (d/tempid part)))

(defn upsert-ref-id
  "Returns either ref id or tempid"
  [db part eid attr]
  (if-let [refeid (first (refids db eid attr))]
    refeid
    (d/tempid part)))

(defn to-lang-str
  "Encodes the language to the end of the string."
  [lang s]
  (str s "@" (if lang (name lang) "__")))

(defn from-lang-str
  "Decodes the language part from the end of the string.
   Return a vector of [lang str]"
  ([^String s] (from-lang-str nil s))
  ([missing-lang ^String s]
   (when s
     (let [cut-point (- (.length s) 3)]
       (when (>= cut-point 0)
         (let [lang (.substring s cut-point)]
           (cond
             (not (.startsWith lang "@")) [missing-lang s]
             (= lang "@__") [missing-lang (.substring s 0 cut-point)]
             :else [(.substring lang 1) (.substring s 0 cut-point)])

           ))))))
         ;[(let [lang (.substring s (+ cut-point 1))]
         ;   (if (= lang "__")
         ;     missing-lang
         ;     lang))
         ; (.substring s 0 cut-point)])))))

(defn as-lang-strings
  "Returns given map {lang str} as collection of str's
   where each string contains the lang in the end of the string."
  [m]
  (for [[k v] m] (to-lang-str k v)))

(defn as-enum [value]
  {:db/id    (d/tempid :enums)
   :db/ident value})

(defn as-attr [{:keys [name type card doc component fulltext index unique nohistory]}]
  (let [card (or (when card (keyword "db.cardinality" (clojure.core/name card)))
                 :db.cardinality/one)
        component (or component false)
        fulltext (or fulltext false)
        index (or index false)
        nohistory (or nohistory false)
        attr {:db/id                 (d/tempid :db.part/db)
              :db/ident              name
              :db/valueType          (keyword "db.type" (clojure.core/name type))
              :db/cardinality        card
              :db/isComponent        component
              :db/fulltext           fulltext
              :db/index              index
              :db/noHistory          nohistory
              :db.install/_attribute :db.part/db}]
    (as-> attr a
          (if doc
            (assoc a :db/doc doc)
            a)
          (if unique
            (assoc a :db/unique (keyword "db.unique" (clojure.core/name unique)))
            a))))

(defn as-db-function [f-var]
  {:db/id    (d/tempid :functions)
   :db/doc   (:doc (meta @f-var))
   :db/ident (:name (meta f-var))
   :db/fn    @f-var})


(defn prefixname-to-iri
  "Returns the prefix names and prefix IRI's as a map of prefixname to iri."
  [db]
  (into {} (d/q '[:find ?a ?i :where [?e :uri/prefixName ?a] [?e :uri/prefixIRI ?i]] db)))

(defn iri-to-prefixname
  "Returns the prefix names and prefix IRI's as a map of iri to name."
  [db]
  (into {} (d/q '[:find ?i ?a :where [?e :uri/prefixName ?a] [?e :uri/prefixIRI ?i]] db)))


(defn insert-schema [conn schema]
  @(d/transact conn (map as-attr schema)))

(defn insert-enums [conn enums]
  @(d/transact conn (map as-enum enums)))

(defn insert-partitions [conn partitions]
  @(d/transact conn (for [part partitions]
                      {:db/id                 (d/tempid :db.part/db)
                       :db/ident              part
                       :db.install/_partition :db.part/db}
                      )))
;
;(def upsert-refs
;  (with-meta
;    (d/function '{:lang   :clojure
;                  :params [db e a v]
;                  :code   (if-not (empty? v)
;                            (let [refid (first (map :v (d/datoms db :eavt e a)))
;                                  add-id (fn [m] (assoc m :db/id refid))]
;                              [{:db/id e
;                                a      (if refid            ; Add reference only if it exist
;                                         (if (map? v)
;                                           (add-id v)
;                                           (map add-id v))
;                                         v)}])
;                            [])})
;    {:doc (str "Associates current ref id for the given reference value map")}))

(def replace-entities
  (with-meta
    (d/function '{:lang   :clojure
                  :params [db tempid e a v]
                  :code   (if-not v                         ;; for nil value do nothing
                            []
                            (concat
                              ;; Handle inverse property
                              (let [[index a datom-k] (if (.startsWith (name a) "_")
                                                [:vaet (keyword (namespace a) (subs (name a) 1)) :e]
                                                [:eavt a :v])]
                                (for [datom (d/datoms db index e a)]
                                  [:db.fn/retractEntity (datom-k datom)]))
                              (if-not (empty? v)
                                [{:db/id (or tempid e)
                                  a      v}]
                                [])))})
    {:doc (str "Returns retract data for the entities given attribute refers to and adds given values.")}))


(def remove-entities
  (with-meta
    (d/function '{:lang   :clojure
                  :params [db tempid e a]
                  :code   (let [[index a datom-k] (if (.startsWith (name a) "_")
                                                    [:vaet (keyword (namespace a) (subs (name a) 1)) :e]
                                                    [:eavt a :v])]
                            (for [datom (d/datoms db index e a)]
                              [:db.fn/retractEntity (datom-k datom)]))})
    {:doc (str "Returns retract data for the entities given attribute refers currently.")}))



(def replace-refs
  (with-meta
    (d/function
      '{:lang   :clojure
        :params [db tempid e a ref-ident v]
        :code   (if-not v
                  ;; For nil do nothing
                  []
                  (let [eid (:db/id (d/entity db e))
                        refids-to-remove (when eid
                                           (let [;; Get only references that are through ref-ident
                                                 old-refids (map #(:e (first (d/datoms db :eavt (:v %) ref-ident)))
                                                                 (d/datoms db :eavt eid a))
                                                 ;; Find out which new references already exists and should be left as-is
                                                 new-refids (remove nil?
                                                                    (map #(:db/id (d/entity db [ref-ident %]))
                                                                         (map ref-ident v)))]
                                             (clojure.set/difference (set old-refids) (set new-refids))))
                        new-refs [{:db/id (or tempid e) a v}]]

                    (if eid
                      (concat new-refs
                              (map (fn [v] [:db/retract eid a v]) refids-to-remove))
                      new-refs)))})
    {:doc "Removes all old attribute entity joins and replaces it with given values."}))

(def replace-values
  (with-meta
    (d/function '{:lang   :clojure
                  :params [db tempid e a v]
                  :code   (let [eid (:db/id (d/entity db e)) ;; handles lookup-ref
                                vals-to-remove (when eid
                                                 (clojure.set/difference
                                                   (set (map :v (d/datoms db :eavt eid a)))
                                                   ;; Allow v to be either single value or multivalue
                                                   ;; v inside transactor is ArrayList!
                                                   (set (if (instance? java.lang.Iterable v)
                                                          v
                                                          [v]))))
                                ;; for inserting values either use tempid
                                ;; or if tempid is null real entityid, which can be null
                                ;; and will throw exception from tx
                                new-vals (if v
                                           [{:db/id (or tempid eid) a v}]
                                           [])
                                ]             ;; nil v will just empty all values
                            (if eid
                              (concat new-vals
                                      (map (fn [v] [:db/retract eid a v]) vals-to-remove))
                              new-vals))})
    {:doc "Removes all attribute values and replaces it with given values."}))

;(def replace-all
;  (with-meta
;    (d/function '{:lang   :clojure
;                  :params [db e a v]
;                  :code   (let [attr-eid (-> (d/datoms db :aevt :db/ident a) first :e)
;                                {:keys [db/isComponent db/cardinality db/valueType]} (d/entity db attr-eid)
;                                upsert-func (cond
;                                              isComponent :replace-components
;                                              (and (= valueType :db.type/ref)
;                                                   (= cardinality :db.cardinality/many)) :replace-refs
;                                              (= cardinality :db.cardinality/many) :replace-coll)]
;                            (if upsert-func
;                              [[upsert-func e a v]]
;                              (throw
;                                (ex-info "Use upsert-function only for components, many cardinal refs or collections"
;                                         {:e e :a a}))))})
;    {:doc "Checks what kind of attribute is and uses proper replace function to replace attribute values."}))


(defn insert-functions [conn]
  @(d/transact conn (map as-db-function [;#'upsert-refs
                                         #'replace-entities
                                         #'remove-entities
                                         #'replace-refs
                                         #'replace-values
                                         ;#'replace-all
                                         ])))


(defn tx-pipeline
  "Transacts data from from-ch. Returns a map with:
     :result, a return channel getting {:error t} or {:completed n}
     :stop, a fn you can use to terminate early."
  [conn conc from-ch]
  (let [to-ch (a/chan 100)
        done-ch (a/chan)
        transact-data (fn [data]
                        (try
                          (let [r @(d/transact-async conn data)]
                            ;(println "tr ready")
                            r)

                          ; if exception in a transaction
                          ; will close channels and put error
                          ; on done channel.
                          (catch Throwable t
                            ;(.printStackTrace t)
                            (a/close! from-ch)
                            (a/close! to-ch)
                            (>!! done-ch {:error t}))))]

    ; go block prints a '.' after every 1000 transactions, puts completed
    ; report on done channel when no value left to be taken.
    (go-loop [total 1]
      (if-let [_ (a/<! to-ch)]
        (do
          (if (zero? (mod total 100))
            (print "*")
            (print "."))
          (flush)
          (recur (inc total)))
        (>! done-ch {:completed (- total 1)})))

    ; pipeline that uses transducer form of map to transact data taken from
    ; from-ch and puts results on to-ch
    (a/pipeline-blocking conc to-ch (map transact-data) from-ch)

    ; returns done channel and a function that you can use
    ; for early termination.
    {:result done-ch
     :stop   (fn [] (a/close! to-ch))}))
