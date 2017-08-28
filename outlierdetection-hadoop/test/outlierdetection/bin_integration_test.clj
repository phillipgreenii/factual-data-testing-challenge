(ns outlierdetection.bin-integration-test
  (:require [clojure.test :refer :all]
            [outlierdetection.bin :refer :all]
            [outlierdetection.attribute :refer :all]))



(deftest bin-integration-test
  (testing "bin-with-all-extractors"
    (let [popular-threshold 95
          standard-deviations-threshold 3
          extractors [
          (outlierdetection.attribute.LengthExtractor. popular-threshold)
          (outlierdetection.attribute.HasLeadingSpaceExtractor. popular-threshold)
          (outlierdetection.attribute.HasTrailingSpaceExtractor. popular-threshold)
          (outlierdetection.attribute.DurationExtractor. standard-deviations-threshold)
          (outlierdetection.attribute.NumberExtractor. standard-deviations-threshold)
          (outlierdetection.attribute.NaiveCharacterTypeCompositionExtractor. popular-threshold)
          (outlierdetection.attribute.CharacterTypeCompositionExtractor. popular-threshold)]
        data (seq 
          (concat (map str (range 40000 50000))
                  [" 43532"
                   "23234 "
                   "00001"
                   "123456"
                   "1234!"
                   "abcde"]))]
    (is (= (clojure.string/split-lines (extract-outliers-as-string extractors data))
            ["    10001  43532 99.97% of inputs have length of 5 AND 99.99% of inputs have no leading space AND 99.96% of inputs consists of digit AND 99.96% of inputs consists of decimal digit number" 
             "    10002 23234  99.97% of inputs have length of 5 AND 99.99% of inputs have no trailing space AND 99.96% of inputs consists of digit AND 99.96% of inputs consists of decimal digit number" 
             "    10003 00001 99.98% of inputs fall within [35927.938973, 54077.751289]" 
             "    10004 123456 99.97% of inputs have length of 5 AND 99.98% of inputs fall within [35927.938973, 54077.751289]" 
             "    10005 1234! 99.96% of inputs consists of digit AND 99.96% of inputs consists of decimal digit number" 
             "    10006 abcde 99.96% of inputs consists of digit AND 99.96% of inputs consists of decimal digit number"]))))

  (testing "bin-with-marathon-times"
    (let [extractors [
          (outlierdetection.attribute.DurationExtractor. 5)]]
      (with-open [rdr (clojure.java.io/reader "test/outlierdetection/bostonmarathon-2012-results.txt")]
        (let [data (line-seq rdr)]
          (is (= (clojure.string/split-lines (extract-outliers-as-string extractors data))
                  ["    18908 09:30:16 100.00% of inputs fall within [0:08:54, 8:28:21]"] ))))))

  (testing "bin-with-english-words"
    (let [popular-threshold 95
          standard-deviations-threshold 3
          extractors [
          (outlierdetection.attribute.LengthExtractor. popular-threshold)
          (outlierdetection.attribute.HasLeadingSpaceExtractor. popular-threshold)
          (outlierdetection.attribute.HasTrailingSpaceExtractor. popular-threshold)
          (outlierdetection.attribute.DurationExtractor. standard-deviations-threshold)
          (outlierdetection.attribute.NumberExtractor. standard-deviations-threshold)
          (outlierdetection.attribute.NaiveCharacterTypeCompositionExtractor. popular-threshold)
          (outlierdetection.attribute.CharacterTypeCompositionExtractor. popular-threshold)]]
      (with-open [rdr (clojure.java.io/reader "test/outlierdetection/english-words-partial.txt")]
        (let [data (line-seq rdr)]
          (is (= (clojure.string/split-lines (extract-outliers-as-string extractors data))
                  [""] ))))))


  (testing "bin-with-addresses"
    (let [popular-threshold 95
          standard-deviations-threshold 3
          extractors [
          (outlierdetection.attribute.LengthExtractor. popular-threshold)
          (outlierdetection.attribute.HasLeadingSpaceExtractor. popular-threshold)
          (outlierdetection.attribute.HasTrailingSpaceExtractor. popular-threshold)
          (outlierdetection.attribute.UsAddressExtractor. popular-threshold)
          (outlierdetection.attribute.CharacterTypeCompositionExtractor. popular-threshold)]]
      (with-open [rdr (clojure.java.io/reader "test/outlierdetection/addresses.txt")]
        (let [data (line-seq rdr)]
          (is (= (clojure.string/split-lines (extract-outliers-as-string extractors data))
                  ["      310 6306 CONEJO RANCHO MURIETA, CA 95683 99.70% of inputs do consist of a US address" 
                   "      339 4030 BROADWAY SACRAMENTO, CA 95817 99.70% of inputs do consist of a US address" 
                   "      748 1813 AVENIDA MARTINA ROSEVILLE, CA 95747 99.70% of inputs do consist of a US address"] ))))))


  (testing "bin-with-addresses-parsing"
    (let [popular-threshold 95
          standard-deviations-threshold 3
          extractors [
          (outlierdetection.attribute.LengthExtractor. popular-threshold)
          (outlierdetection.attribute.HasLeadingSpaceExtractor. popular-threshold)
          (outlierdetection.attribute.HasTrailingSpaceExtractor. popular-threshold)
          (outlierdetection.attribute.ParsingUsAddressExtractor. popular-threshold)
          (outlierdetection.attribute.CharacterTypeCompositionExtractor. popular-threshold)]]
      (with-open [rdr (clojure.java.io/reader "test/outlierdetection/addresses.txt")]
        (let [data (line-seq rdr)]
          (is (= (clojure.string/split-lines (extract-outliers-as-string extractors data))
                  ["      310 6306 CONEJO RANCHO MURIETA, CA 95683 99.70% of inputs do consist of a US address" 
                   "      339 4030 BROADWAY SACRAMENTO, CA 95817 99.70% of inputs do consist of a US address" 
                   "      748 1813 AVENIDA MARTINA ROSEVILLE, CA 95747 99.70% of inputs do consist of a US address"] ))))))
  )


