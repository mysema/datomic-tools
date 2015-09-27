(ns mysema.datomic.core-test
  (:require [clojure.test :refer :all]
            [datomic.api :as d :refer [q]]
            [mysema.datomic.core :as dt]))

;;TODO refactor tapahtumatieto away from the tests
;
;(def db-uri "datomic:mem://unit-testing")
;(defn conn [] (d/connect db-uri))
;(defn db [] (d/db (conn)))
;
;(defn clean-before [f]
;  (d/delete-database db-uri)
;  (bt/create-system db-uri)
;  (f))
;
;(defn transact [d] @(d/transact (conn) d))
;
;(defn eventid [id] (dt/entid (db) :so.event/id id))
;(defn personid [id] (dt/entid (db) :so.person/id id))
;
;(def sameas-vals '[:find [?v ...] :where [_ :so/sameAs ?v]])
;(def email-vals '[:find [?v ...] :where [_ :so.person/email ?v]])
;(def rdftype-vals '[:find [?i ...]
;                    :where [_ :rdf/type ?v]
;                    [?v :db/ident ?i]])
;(def member-refs '[:find [?i ...]
;                   :where
;                   [_ :so/memberOf ?p]
;                   [?p :so.person/id ?i]])
;(def geo-vals '[:find [?v ...]
;                :where [_ :so.geocoordinates/latitude ?v]])
;
;
;(use-fixtures :each clean-before)
;
;(defmacro assert-qry [qry res]
;  `(is (= (set (q ~qry (db))) (set ~res))))
;
;(deftest test-replace-values
;
;  ;; 0
;  (transact [{:db/id (d/tempid :events -1) :so.event/id "e1"}
;             [:replace-values (d/tempid :events -1) nil :so/sameAs ["a"]]])
;  (assert-qry sameas-vals ["a"])
;
;  ;; 1
;  (transact [[:replace-values nil [:so.event/id "e1"] :so/sameAs ["eka" "toka"]]])
;  (assert-qry sameas-vals ["eka" "toka"])
;
;  ;; 2
;  (transact [[:replace-values nil (eventid "e1") :so/sameAs ["eka"]]])
;  (assert-qry sameas-vals ["eka"])
;
;  ;; 3
;  (transact [[:replace-values nil (eventid "e1") :so/sameAs ["kolmas"]]])
;  (assert-qry sameas-vals ["kolmas"])
;
;  ;; 4
;  (transact [[:replace-values nil (eventid "e1") :so/sameAs nil]])
;  ;; Nil does nothing
;  (assert-qry sameas-vals ["kolmas"])
;
;  ;; 5
;  (transact [[:replace-values nil (eventid "e1") :so/sameAs []]])
;  ;; Empty coll clears
;  (assert-qry sameas-vals [])
;
;  ;; Single values
;  (transact [[:replace-values nil (eventid "e1") :so.person/email "mail1"]])
;  (assert-qry email-vals ["mail1"])
;
;  (transact [[:replace-values nil (eventid "e1") :so.person/email "mail2"]])
;  (assert-qry email-vals ["mail2"])
;
;  (transact [[:replace-values nil (eventid "e1") :so.person/email nil]])
;  (assert-qry email-vals ["mail2"])
;
;  ;; is this needed?
;  ;(transact [[:replace-values (eventid "e1")
;  ;            :so.person/email :db/retract]])
;  ;(assert-qry email-vals [])
;
;  )
;
;(comment
;
;  (d/touch (d/entity (db) [:so.event/id "e1"]))
;
;  (dt/replace-refs (db) nil [:so.event/id "e1"]
;                   :rdf/type :db/ident [{:db/ident :rdf.type/Event}])
;
;  )
;
;(deftest test-replace-refs-enum
;
;  (transact [{:db/id       (d/tempid :events)
;              :so.event/id "e1"}])
;
;  (assert-qry rdftype-vals [])
;
;  ;; 1
;  (transact [[:replace-refs nil (eventid "e1")
;              :rdf/type :db/ident [{:db/ident :rdf.type/Event} {:db/ident :rdf.type/Place}]]])
;  (assert-qry rdftype-vals [:rdf.type/Event :rdf.type/Place])
;
;  ;; 2
;  (transact [[:replace-refs nil (eventid "e1")
;              :rdf/type :db/ident [{:db/ident :rdf.type/Event}]]])
;  (assert-qry rdftype-vals [:rdf.type/Event])
;
;  ;;3
;  (transact [[:replace-refs nil (eventid "e1")
;              :rdf/type :db/ident [{:db/ident :rdf.type/Person}]]])
;  (assert-qry rdftype-vals [:rdf.type/Person])
;
;  ;;4
;  (transact [[:replace-refs nil (eventid "e1")
;              :rdf/type :db/ident [{:db/ident :rdf.type/Person}]]])
;  (assert-qry rdftype-vals [:rdf.type/Person])
;
;  ;;5
;  (transact [[:replace-refs nil (eventid "e1")
;              :rdf/type :db/ident nil]])
;  (assert-qry rdftype-vals [:rdf.type/Person])
;
;  ;;6
;  (transact [[:replace-refs nil (eventid "e1")
;              :rdf/type :db/ident []]])
;  (assert-qry rdftype-vals [])
;
;  )
;
;(deftest test-replace-refs
;
;  (transact [{:db/id (d/tempid :events) :so.event/id "e1"}])
;  (transact [{:db/id (d/tempid :events) :so.person/id "p1"}])
;  (transact [{:db/id (d/tempid :events) :so.person/id "p2"}])
;  (transact [{:db/id (d/tempid :events) :so.person/id "p2"}])
;
;  (assert-qry member-refs [])
;
;  ;; 1
;  (transact [[:replace-refs nil (eventid "e1")
;              :so/memberOf :so.person/id [{:so.person/id "p1"} {:so.person/id "p2"}]]])
;  (assert-qry member-refs ["p1" "p2"])
;
;  ;; 2
;  (transact [[:replace-refs nil (eventid "e1")
;              :so/memberOf :so.person/id [{:so.person/id "p1"}]]])
;  (assert-qry member-refs ["p1"])
;
;  ;; 3
;  (transact [[:replace-refs nil (eventid "e1")
;              :so/memberOf :so.person/id [{:so.person/id "p3"}]]])
;  (assert-qry member-refs ["p3"])
;
;  ;; 4
;  (transact [[:replace-refs nil (eventid "e1")
;              :so/memberOf :so.person/id []]])
;  (assert-qry member-refs [])
;
;  ;; 5
;  (transact [[:replace-refs nil (eventid "e1")
;              :so/memberOf :so.person/id [{:db/id (personid "p1")}]]])
;  (assert-qry member-refs ["p1"])
;
;  ;; 6
;  (transact [[:replace-refs nil (eventid "e1")
;              :so/memberOf :so.person/id nil]])
;  (assert-qry member-refs ["p1"])
;
;  ;; 7 Adding new entity at the same time
;  (transact [[:replace-refs nil (eventid "e1")
;              :so/memberOf :so.person/id [{:so.person/id "p4"}]]])
;  (assert-qry member-refs ["p4"])
;
;  ;; 8 explicit tempid
;  (transact [[:replace-refs nil [:so.event/id "e1"]
;              :so/memberOf :so.person/id [{:db/id        (d/tempid :persons)
;                                           :so.person/id "p1"}]]])
;
;  (assert-qry member-refs ["p1"])
;
;  ;; 8.1 replacing the same through lookupref works
;  (transact [[:replace-refs nil [:so.event/id "e1"]
;              :so/memberOf :so.person/id [{:db/id        [:so.person/id "p1"]
;                                           :so.person/id "p1"}]]])
;
;  (assert-qry member-refs ["p1"])
;
;  ;; 9 If lookup ref is not found, exception is thrown
;  (is (thrown? Exception
;               (transact [[:replace-refs nil (eventid "e1")
;                           :so/memberOf :so.person/id [{:db/id        [:so.person/id "not-p1"]
;                                                        :so.person/id "not-p1"}]]])))
;
;  (assert-qry member-refs ["p1"])
;  )
;
;(defmacro assert-lang-val
;  [s lang]
;  `(let [[rlang# rs#] (dt/from-lang-str (dt/to-lang-str ~lang ~s))]
;     (is (= (= ~s rs#)) "String is not same")
;     (is (= ~lang rlang#) "Lang is not same")))
;
;(deftest lang-strings
;
;  (assert-lang-val "a" "fi")
;  (assert-lang-val "a" nil)
;  (assert-lang-val nil nil)
;  (assert-lang-val "" "fi")
;  (assert-lang-val "" nil)
;
;  )


;; Not sure is this needed at all
;
;(deftest test-replace-entities
;
;  (transact [{:db/id (d/tempid :events) :so.event/id "e1"}])
;
;  (assert-qry geo-vals [])
;
;  ;; 1
;  (transact [[:replace-entities (eventid "e1")
;              :so.place/geo {:so.geocoordinates/latitude 123}]])
;  (assert-qry geo-vals [123])
;
;  ;; 2
;  (transact [[:replace-entities (eventid "e1")
;              :so/description {:text/en "en"}]])
;  (assert-qry geo-vals ["en"])
;
;  ;; 3
;  (transact [[:replace-entities (eventid "e1")
;              :so/description {:text/sv "sv"}]])
;  (assert-qry geo-vals ["sv"])
;
;  ;; 4
;  (transact [[:replace-entities (eventid "e1")
;              :so/description nil]])
;  (assert-qry geo-vals ["sv"])
;
;  ;; 5
;  (transact [[:replace-entities (eventid "e1")
;              :so/description {}]])
;  (assert-qry geo-vals [])
;
;  )
