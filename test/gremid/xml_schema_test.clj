(ns gremid.xml-schema-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is]]
   [gremid.xml-schema :as gxs])
  (:import
   (java.io File)))

(deftest validate-tei-goethe-faust
  (is (empty? (gxs/rng-validate
               (gxs/->rng-schema (io/resource "tei/tei_all.rng"))
               (io/resource "tei/dta_goethe_faust01_1808.xml")))))

(defn temp-file
  [suffix]
  (doto (File/createTempFile "gremid.xml-schema." suffix)
    (.deleteOnExit)))

(deftest compile-and-validate-dwds
  (let [xml-source   (io/resource "dwds/template.xml")
        rng-file     (temp-file ".rng")
        sch-xsl-file (temp-file ".sch.xsl")]
    (gxs/rnc->rng "test/dwds/DWDSWB.rnc" (str rng-file))
    (gxs/rng->sch-xsl rng-file sch-xsl-file)
    (is (seq (gxs/rng-validate (gxs/->rng-schema rng-file) xml-source)))
    (is (empty? (gxs/sch-validate (gxs/->xslt sch-xsl-file) xml-source)))))
