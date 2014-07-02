(ns outlierdetection.bin
  (:gen-class)
  (:require [outlierdetection.attribute :refer :all ])
  (:require [outlierdetection.outlier :refer :all ]))

(defn extract-outliers-as-string [extractors data]
  (pretty-format-outliers (detect-outliers extractors data)))

(defn -main [& args]
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

    (with-open [rdr (clojure.java.io/reader (first args))]
      (let [data (line-seq rdr)]
        (println
          (extract-outliers-as-string extractors data))))))