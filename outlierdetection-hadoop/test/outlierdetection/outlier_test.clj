(ns outlierdetection.outlier-test
  (:require [clojure.test :refer :all]
            [outlierdetection.outlier :refer :all]
            [outlierdetection.attribute :refer :all]))

(deftype DummyExistsOutlierExtractor []
  AttributeExtractor
  (id [this] 
    (symbol "outlierdetection.outlier-test" "dummy-all"))
  (extract [this s]
    s)
  (generate-detector [this attributes-coll]
    (let [outlier-detector 
          (build-discrete-outlier-detector 
            50
            (extract-values-by-key attributes-coll (id this)))
          value-extractor (id this)]
      (comp outlier-detector value-extractor))))


(deftype DummyNilOutlierExtractor []
  AttributeExtractor
  (id [this] 
    (symbol "outlierdetection.outlier-test" "dummy-none"))
  (extract [this s]
    nil)
  (generate-detector [this attributes-coll]
    (let [outlier-detector 
          (build-discrete-outlier-detector 
            50
            (extract-values-by-key attributes-coll (id this)))
          value-extractor (id this)]
      (comp outlier-detector value-extractor))))

(deftest outlier-test
  (testing "extract-attributes-coll-from-line"
    (let [extractors [(DummyExistsOutlierExtractor.) 
                      (DummyNilOutlierExtractor. )]
          line-offset 1
          line "abcd"]
      (is (= (extract-attributes-coll-from-line extractors line-offset line) 
             {'outlierdetection.outlier-test/dummy-all  "abcd", 
              'outlierdetection.outlier-test/dummy-none nil, 
              :line                                 "abcd", 
              :line-number                          2}))))

  (testing "generate-outlier-detector"
    (let [extractors [(DummyExistsOutlierExtractor.) 
                      (DummyNilOutlierExtractor. )]
          attributes-coll (list  {'outlierdetection.outlier-test/dummy-all  "abcd", 
                                  'outlierdetection.outlier-test/dummy-none nil, 
                                  :line                                 "abcd", 
                                  :line-number                          1}
                                 {'outlierdetection.outlier-test/dummy-all  "efgh", 
                                  'outlierdetection.outlier-test/dummy-none nil, 
                                  :line                                 "efgh", 
                                  :line-number                          2}
                                 {'outlierdetection.outlier-test/dummy-all  "efgh", 
                                  'outlierdetection.outlier-test/dummy-none nil, 
                                  :line                                 "efgh", 
                                  :line-number                          3})
          outlier-detector (generate-outlier-detector extractors attributes-coll)]
      (is (= (outlier-detector "abcd") '("66.67% of inputs are efgh") )))) 

  (testing "detect-outliers"
    (let [extractors [(DummyExistsOutlierExtractor.) 
                      (DummyNilOutlierExtractor. )] 
          data ["abcd"
                "abcd"
                "abcd"
                "efgh"]
          outliers (detect-outliers extractors data)]
      (is (= outliers (list (outlierdetection.outlier.Outlier. "efgh" 4 '("75.00% of inputs are abcd"))))))))
