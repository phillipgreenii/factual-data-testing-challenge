(ns outlierdetection.attribute
  (:require [incanter.stats :as s] ))

(defprotocol AttributeExtractor
  "Extracts an Attribute from a String"
  (id [this] "Returns a symbol to identify the attribute")
  (extract [this s] "Extracts the attribute from the string")
  (generate-detector [this attributes-coll] "Generates detector function based upon extracted attributes"))

(defn build-normal-outlier-detector [standard-deviations-threshold values & {:keys [format-callback], 
                                                  :or {format-callback #(format "%.2f%% of inputs fall within [%f, %f]" %1 %2 %3 %4)}}]
  (if-let [clean-values (seq (filter (complement nil?) values))]
    (let [n (count clean-values)
          mean (s/mean clean-values)
          sd (s/sd clean-values)
          lower-limit (- mean (* standard-deviations-threshold sd))
          upper-limit (+ mean (* standard-deviations-threshold sd))
          inlier? #(<= lower-limit % upper-limit)
          n-valid (count (filter inlier? clean-values))
          valid-percentage (* (/ n-valid n) 100.0)]
      (fn [value]
        (when value 
          (when-not (inlier? value)
            (format-callback valid-percentage lower-limit upper-limit value)))))
    (constantly nil)))

(defn build-discrete-outlier-detector [popular-threshold values & {:keys [format-callback], 
                                                    :or   {format-callback #(format "%.2f%% of inputs are %s" %1 %2 %3)}}]
  (let [n (count values)
        frequencies (frequencies values)
        [value-popular n-popular] (apply (partial max-key second) (seq frequencies))
        popular-percentage (* (/ n-popular n) 100.0)]
    (if (>= popular-percentage popular-threshold)
        (fn [value]
            (when-not (= value-popular value)
              (format-callback popular-percentage value-popular value)))
        (constantly nil))))

(defn extract-values-by-key [attributes-coll key]
  (map #(get % key) attributes-coll))

(let [remove-leading-colon #(clojure.string/replace % #"^:" "")
      replace-hyphen-with-space #(clojure.string/replace % #"-" " ")]
  (defn- format-symbols [symbols]
    (clojure.string/join ", "
      (map  (fn [s]
              (-> s str 
                    remove-leading-colon
                    replace-hyphen-with-space
                    ))
        symbols))))

(deftype LengthExtractor [popular-threshold]
  AttributeExtractor
  (id [this] 
    (symbol "outlierdetection.attribute" "length"))
  (extract [this string]
    (count string))
  (generate-detector [this attributes-coll]
    (let [outlier-detector 
          (build-discrete-outlier-detector 
            popular-threshold
            (extract-values-by-key attributes-coll (id this))
            :format-callback (fn [popular-percentage value-popular _]
                              (format "%.2f%% of inputs have length of %s" popular-percentage value-popular)))
          value-extractor (id this)]
      (comp outlier-detector value-extractor))))


(deftype HasLeadingSpaceExtractor [popular-threshold]
  AttributeExtractor
  (id [this] 
    (symbol "outlierdetection.attribute" "has-leading-space"))
  (extract [this string]
    (boolean (re-find #"^\s+" string)))
  (generate-detector [this attributes-coll]
    (let [outlier-detector 
          (build-discrete-outlier-detector 
            popular-threshold
            (extract-values-by-key attributes-coll (id this))
            :format-callback (fn [popular-percentage value-popular _]
                              (format "%.2f%% of inputs have %s" popular-percentage (if value-popular "leading space" "no leading space"))))
          value-extractor (id this)]
      (comp outlier-detector value-extractor))))

(deftype HasTrailingSpaceExtractor [popular-threshold]
  AttributeExtractor
  (id [this] 
    (symbol "outlierdetection.attribute" "has-trailing-space"))
  (extract [this string]
    (boolean (re-find #"\s+$" string)))
  (generate-detector [this attributes-coll]
    (let [outlier-detector 
          (build-discrete-outlier-detector 
            popular-threshold
            (extract-values-by-key attributes-coll (id this))
            :format-callback (fn [popular-percentage value-popular _]
                              (format "%.2f%% of inputs have %s" popular-percentage (if value-popular "trailing space" "no trailing space"))))
          value-extractor (id this)]
      (comp outlier-detector value-extractor))))


(defn 
  ^{:source "based on solution from https://stackoverflow.com/questions/2640169/whats-the-easiest-way-to-parse-numbers-in-clojure"}
  parse-number
  "Reads a number from a string. Returns nil if not a number."
  [s]
  (if (re-find #"^-?\d+\.?\d*$" s)
    (read-string s)))

(deftype NumberExtractor [standard-deviations-threshold]
  AttributeExtractor
  (id [this] 
    (symbol "outlierdetection.attribute" "numerical-value"))
  (extract [this string]
    (parse-number string))
  (generate-detector [this attributes-coll]
    (let [outlier-detector 
          (build-normal-outlier-detector
            standard-deviations-threshold
            (extract-values-by-key attributes-coll (id this)))
          value-extractor (id this)]
      (comp outlier-detector value-extractor))))     


(defn duration-total-seconds2str [total-seconds]
  (let [sign (Integer/signum total-seconds)
        positive-total-seconds (Math/abs total-seconds)
        hours  (int (quot positive-total-seconds 3600) )
        minutes (int (quot (rem positive-total-seconds 3600) 60))
        seconds (int (rem (rem positive-total-seconds 3600) 60))]
    (format "%d:%02d:%02d" (* sign hours) minutes seconds)))


(defn duration-str2total-seconds [s]
  (let [[hours minutes seconds :as parts] 
    (map #(Integer/parseInt %)  
      (rest (re-matches #"(-?\d+):(\d+):(\d+)" s)))]
    (when (not-empty parts)
      (* (Integer/signum hours)
         (+ seconds 
           (* 60 
             (+ minutes 
               (* 60 
                  (Math/abs hours)))))))))

(deftype DurationExtractor [standard-deviations-threshold]
  AttributeExtractor
  (id [this] 
    (symbol "outlierdetection.attribute" "time-duration"))
  (extract [this string]
    (duration-str2total-seconds string))
  (generate-detector [this attributes-coll]
    (let [outlier-detector 
          (build-normal-outlier-detector
            standard-deviations-threshold
            (extract-values-by-key attributes-coll (id this))
            :format-callback (fn [valid-percentage lower-limit upper-limit value] 
              (format "%.2f%% of inputs fall within [%s, %s]" 
                valid-percentage 
                (duration-total-seconds2str lower-limit) 
                (duration-total-seconds2str upper-limit) 
                value)))
          value-extractor (id this)]
      (comp outlier-detector value-extractor))))



(let [character-composition-types (hash-map 
                   :white-space #(Character/isWhitespace %)
                   :digit #(Character/isDigit %)
                   :iso-control #(Character/isISOControl %)
                   :upper-case #(Character/isUpperCase %)
                   :lower-case #(Character/isLowerCase %)
                   :symbol (fn [c] (not (or (Character/isLetterOrDigit c)
                                            (Character/isWhitespace c)
                                            (Character/isISOControl c)))))]
  (defn extract-naive-character-type-composition [s]
    (into #{}
      (for [[k p] character-composition-types 
            :when (some p s)] 
        k))))

(deftype NaiveCharacterTypeCompositionExtractor [popular-threshold]
  AttributeExtractor
  (id [this] 
    (symbol "outlierdetection.attribute" "naive-character-type-composition"))
  (extract [this string]
    (extract-naive-character-type-composition string))
  (generate-detector [this attributes-coll]
    (let [outlier-detector 
          (build-discrete-outlier-detector 
            popular-threshold
            (extract-values-by-key attributes-coll (id this))
            :format-callback (fn [popular-percentage value-popular _]
                              (format "%.2f%% of inputs consists of %s" popular-percentage (format-symbols value-popular))))
          value-extractor (id this)]
      (comp outlier-detector value-extractor))))



(let [character-type-lookup ^{:source "http://docs.oracle.com/javase/7/docs/api/java/lang/Character.html#getType(char)"}
        [:unassigned                ; 0
         :upper-case                ; 1
         :lower-case                ; 2
         :title-case                ; 3
         :modifier-letter           ; 4
         :other-letter              ; 5
         :non-spacing-mark          ; 6
         :enclosing-mark            ; 7
         :combining-spacing-mark    ; 8
         :decimal-digit-number      ; 9
         :letter-number             ;10
         :other-number              ;11
         :space-separator           ;12
         :line-separator            ;13
         :paragraph-separator       ;14
         :control                   ;15
         :format                    ;16
         nil ; slot unused          ;17 
         :private-use               ;18
         :surrogate                 ;19
         :dash-punctuation          ;20
         :start-punctuation         ;21
         :end-punctuation           ;22
         :connector-punctuation     ;23
         :other-punctuation         ;24
         :math-symbol               ;25 
         :currency-symbol           ;26 
         :modifier-symbol           ;27
         :other-symbol              ;28
         :initial-quote-punctuation ;29
         :final-quote-punctuation]] ;30
  (defn extract-character-type-composition [s]
    (into #{}
      (map (partial get character-type-lookup)
        (map #(Character/getType %) s)))))

(deftype CharacterTypeCompositionExtractor [popular-threshold]
  AttributeExtractor
  (id [this] 
    (symbol "outlierdetection.attribute" "character-type-composition"))
  (extract [this string]
    (extract-character-type-composition string))
  (generate-detector [this attributes-coll]
    (let [outlier-detector 
          (build-discrete-outlier-detector 
            popular-threshold
            (extract-values-by-key attributes-coll (id this))
            :format-callback (fn [popular-percentage value-popular _]
                              (format "%.2f%% of inputs consists of %s" popular-percentage (format-symbols value-popular))))
          value-extractor (id this)]
      (comp outlier-detector value-extractor))))
