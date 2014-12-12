(ns lwfp1.core-test
  (:require [clojure.test :refer :all]
            [lwfp1.core :refer :all]))

(deftest test-Clasterization
  (testing "Clasterization"
    (let [Coll (-main "бабочка.txt" "-e")]
      (is (= (count Coll) 1))
    )
  )
)

(deftest test-Load
  (testing "Load"
    (let [Coll (LoadDataFromFile "бабочка.txt")]
      (is (= (count Coll) 15))
    )
  )
)

(deftest test-CreatePoint
  (testing "CreatePoint"
    (let [Pt (CreatePoint [1 2 3])]
      (is (= (count (:coords Pt)) 3))
    )
  )
)