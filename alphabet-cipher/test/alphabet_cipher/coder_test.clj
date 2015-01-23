(ns alphabet-cipher.coder-test
  (:require [clojure.test :refer :all]
            [alphabet-cipher.coder :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ct]))

(def alphabet-strings
  )


(deftest test-encode
  (testing "can encode give a secret keyword"
    (is (= "hmkbxebpxpmyllyrxiiqtoltfgzzv"
           (encode "vigilance" "meetmeontuesdayeveningatseven")))
    (is (= "egsgqwtahuiljgs"
           (encode "scones" "meetmebythetree")))))


(deftest extra
  (testing "bindings?"
    (is (let [x alphabet-strings]
          (= 1 1)))))

(ct/defspec decode-is-inverse-of-encode
  100
  (prop/for-all [key (gen/fmap #(apply str %)
                               (gen/not-empty
                                (gen/vector (gen/elements alphabet))))
                 message (gen/fmap #(apply str %)
                                   (gen/not-empty
                                    (gen/vector (gen/elements alphabet))))]
                (= message (decode key (encode key message)))))

(ct/defspec encode-is-inverse-of-decode
  100
  (prop/for-all [key (gen/fmap #(apply str %)
                               (gen/not-empty
                                (gen/vector (gen/elements alphabet))))
                 message (gen/fmap #(apply str %)
                                   (gen/not-empty
                                    (gen/vector (gen/elements alphabet))))]
                (= message (encode key (decode key message)))))

(deftest test-decode
  (testing "can decode an cyrpted message given a secret keyword"
    (is (= "meetmeontuesdayeveningatseven"
           (decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv")))
    (is (= "meetmebythetree"
           (decode "scones" "egsgqwtahuiljgs")))))
