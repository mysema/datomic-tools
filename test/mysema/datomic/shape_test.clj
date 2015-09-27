(ns mysema.datomic.shape-test
  (:require
    [clojure.test :refer :all]
    [mysema.datomic.shape :refer :all]))


(def shape-config
  '{name :so.name})

(def shapes {:default [:id]})

(defmacro set= [s1 s2]
  `(is (= (set ~s1) (set ~s2))))

(deftest shape-1

  (testing "transforming the public shape to actual Datomic shape"

    (is (= {:ext [:so.name]}
           (read-shape shape-config :ext "[name]")))

    (is (= {:ext [:so.name]}
           (read-shape shape-config :ext "[name foobar]")))

    (is (= nil
           (read-shape shape-config :ext nil)))

    )

  (testing "shape combinations"

    (set= [:name :id]
          (combine-shapes [:a :b] {:b [:name]} {:a [:id]}))

    (set= [:so.name :id]
          (combine-shapes [:ext :default]
                          (read-shape shape-config :ext "[name]")
                          shapes))

    (set= [:id]
          (combine-shapes [:default] shapes))

    (set= [:id]
          (set (combine-shapes [:default :notexisting] shapes)))

    (set= [:id]
          (combine-shapes [:default :notexisting] shapes nil))

    )

  )



