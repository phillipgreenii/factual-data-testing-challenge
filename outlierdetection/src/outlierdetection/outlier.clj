(ns outlierdetection.outlier
  (:gen-class)
  (:require [outlierdetection.attribute :refer :all ]))

(defn extract-attributes-coll-from-line [extractors line-offset line]
  (apply hash-map
    (flatten 
      (concat (list :line line
                    :line-number (inc line-offset))
              (map #(list (id %) 
                          (extract % line))
                    extractors)))))

(defn generate-outlier-detector [extractors attributes-coll]
  (let [detectors (map #(generate-detector % attributes-coll) extractors)]
    (fn [attributes]
      (let [outlier-detections (filter (complement nil?) (map #(% attributes) detectors))]
        (when (not-empty outlier-detections)
          outlier-detections)))))

(defrecord Outlier [line line-number messages])


(defn- attributes-coll2outliers [outlier-detector attributes-coll]
  (let [outliers (map #(Outlier. (:line %) (:line-number %) (outlier-detector %)) attributes-coll)]
    (filter (comp (complement nil?) :messages)
            outliers)))

(defn detect-outliers [extractors data]
  (let [attributes-coll (map-indexed (partial extract-attributes-coll-from-line extractors) data)
        outlier-detector (generate-outlier-detector extractors attributes-coll)
        outliers (attributes-coll2outliers outlier-detector attributes-coll)]
    outliers))

(defn pretty-format-outliers [outliers]
  (if (seq outliers)
    (clojure.string/join "\n"
      (map #(format "%9d %s %s" (:line-number %) (:line %) (clojure.string/join " AND " (:messages %))) outliers))
    ""))
