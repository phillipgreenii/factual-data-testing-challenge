(ns outlierdetection.attribute
  (:require [incanter.stats :as i])
  (:require [clojure.string :as s])
  (:require [instaparse.core :as insta]) )


(defprotocol AttributeExtractor
  "Extracts an Attribute from a String"
  (id [this] "Returns a symbol to identify the attribute")
  (extract [this s] "Extracts the attribute from the string")
  (generate-detector [this attributes-coll] "Generates detector function based upon extracted attributes"))

(defn build-normal-outlier-detector [standard-deviations-threshold values & {:keys [format-callback], 
                                                  :or {format-callback #(format "%.2f%% of inputs fall within [%f, %f]" %1 %2 %3 %4)}}]
  (if-let [clean-values (seq (filter (complement nil?) values))]
    (let [n (count clean-values)
          mean (i/mean clean-values)
          sd (i/sd clean-values)
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

(let [remove-leading-colon #(s/replace % #"^:" "")
      replace-hyphen-with-space #(s/replace % #"-" " ")]
  (defn- format-symbols [symbols]
    (s/join ", "
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




(def
 ^{:const true 
   :source "https://www.usps.com/send/official-abbreviations.htm"}
  state-abbreviation-lookup
  (hash-map 
    "AL" "AL"
    "ALABAMA" "AL"
    "AK" "AK"
    "ALASKA" "AK"
    "AS" "AS"
    "AMERICAN SAMOA" "AS"
    "AZ" "AZ"
    "ARIZONA" "AZ"
    "AR" "AR"
    "ARKANSAS" "AR"
    "CA" "CA"
    "CALIFORNIA" "CA"
    "CO" "CO"
    "COLORADO" "CO"
    "CT" "CT"
    "CONNECTICUT" "CT"
    "DE" "DE"
    "DELAWARE" "DE"
    "DC" "DC"
    "DISTRICT OF COLUMBIA" "DC"
    "FM" "FM"
    "FEDERATED STATES OF MICRONESIA" "FM"
    "FL" "FL"
    "FLORIDA" "FL"
    "GA" "GA"
    "GEORGIA" "GA"
    "GU" "GU"
    "GUAM GU" "GU"
    "HI" "HI"
    "HAWAII" "HI"
    "ID" "ID"
    "IDAHO" "ID"
    "IL" "IL"
    "ILLINOIS" "IL"
    "IN" "IN"
    "INDIANA" "IN"
    "IA" "IA"
    "IOWA" "IA"
    "KS" "KS"
    "KANSAS" "KS"
    "KY" "KY"
    "KENTUCKY" "KY"
    "LA" "LA"
    "LOUISIANA" "LA"
    "ME" "ME"
    "MAINE" "ME"
    "MH" "MH"
    "MARSHALL ISLANDS" "MH"
    "MD" "MD"
    "MARYLAND" "MD"
    "MA" "MA"
    "MASSACHUSETTS" "MA"
    "MI" "MI"
    "MICHIGAN" "MI"
    "MN" "MN"
    "MINNESOTA" "MN"
    "MS" "MS"
    "MISSISSIPPI" "MS"
    "MO" "MO"
    "MISSOURI" "MO"
    "MT" "MT"
    "MONTANA" "MT"
    "NE" "NE"
    "NEBRASKA" "NE"
    "NV" "NV"
    "NEVADA" "NV"
    "NH" "NH"
    "NEW HAMPSHIRE" "NH"
    "NJ" "NJ"
    "NEW JERSEY" "NJ"
    "NM" "NM"
    "NEW MEXICO" "NM"
    "NY" "NY"
    "NEW YORK" "NY"
    "NC" "NC"
    "NORTH CAROLINA" "NC"
    "ND" "ND"
    "NORTH DAKOTA" "ND"
    "MP" "MP"
    "NORTHERN MARIANA ISLANDS" "MP"
    "OH" "OH"
    "OHIO" "OH"
    "OK" "OK"
    "OKLAHOMA" "OK"
    "OR" "OR"
    "OREGON" "OR"
    "PW" "PW"
    "PALAU" "PW"
    "PA" "PA"
    "PENNSYLVANIA" "PA"
    "PR" "PR"
    "PUERTO RICO" "PR"
    "RI" "RI"
    "RHODE ISLAND" "RI"
    "SC" "SC"
    "SOUTH CAROLINA" "SC"
    "SD" "SD"
    "SOUTH DAKOTA" "SD"
    "TN" "TN"
    "TENNESSEE" "TN"
    "TX" "TX"
    "TEXAS" "TX"
    "UT" "UT"
    "UTAH" "UT"
    "VT" "VT"
    "VERMONT" "VT"
    "VI" "VI"
    "VIRGIN ISLANDS" "VI"
    "VA" "VA"
    "VIRGINIA" "VA"
    "WA" "WA"
    "WASHINGTON" "WA"
    "WV" "WV"
    "WEST VIRGINIA" "WV"
    "WI" "WI"
    "WISCONSIN" "WI"
    "WY" "WY"
    "WYOMING" "WY"
    "AE" "AE"
    "ARMED FORCES AFRICA" "AE"
    "AA" "AA"
    "ARMED FORCES AMERICAS (EXCEPT CANADA)" "AA"
    "AE" "AE"
    "ARMED FORCES CANADA" "AE"
    "AE" "AE"
    "ARMED FORCES EUROPE" "AE"
    "AE" "AE"
    "ARMED FORCES MIDDLE EAST" "AE"
    "AP" "AP"
    "ARMED FORCES PACIFIC" "AP"
    ))
(def
 ^{:const true 
   :source "https://www.usps.com/send/official-abbreviations.htm"}
  street-suffix-lookup
  (hash-map 
    "ALLEY" "ALY"
    "ALLEE" "ALY"
    "ALY" "ALY"
    "ALLY" "ALY"
    "ANNEX" "ANX"
    "ANEX" "ANX"
    "ANX" "ANX"
    "ARCADE" "ARC"
    "ARC" "ARC"
    "AVENUE" "AVE"
    "AV" "AVE"
    "AVE" "AVE"
    "AVEN" "AVE"
    "AVENU" "AVE"
    "AVN" "AVE"
    "AVNUE" "AVE"
    "BAYOO" "BYU"
    "BYU" "BYU"
    "BAYOU" "BYU"
    "BEACH" "BCH"
    "BCH" "BCH"
    "BEND" "BND"
    "BND" "BND"
    "BLUFF" "BLF"
    "BLF" "BLF"
    "BLUF" "BLF"
    "BLUFFS" "BLFS"
    "BLFS" "BLFS"
    "BOTTOM" "BTM"
    "BOT" "BTM"
    "BTM" "BTM"
    "BOTTM" "BTM"
    "BOULEVARD" "BLVD"
    "BLVD" "BLVD"
    "BOUL" "BLVD"
    "BOULV" "BLVD"
    "BRANCH" "BR"
    "BR" "BR"
    "BRNCH" "BR"
    "BRIDGE" "BRG"
    "BRDGE" "BRG"
    "BRG" "BRG"
    "BROOK" "BRK"
    "BRK" "BRK"
    "BROOKS" "BRKS"
    "BRKS" "BRKS"
    "BURG" "BG"
    "BG" "BG"
    "BURGS" "BGS"
    "BGS" "BGS"
    "BYPASS" "BYP"
    "BYP" "BYP"
    "BYPA" "BYP"
    "BYPAS" "BYP"
    "BYPS" "BYP"
    "CAMP" "CP"
    "CP" "CP"
    "CMP" "CP"
    "CANYON" "CYN"
    "CANYN" "CYN"
    "CYN" "CYN"
    "CNYN" "CYN"
    "CAPE" "CPE"
    "CPE" "CPE"
    "CAUSEWAY" "CSWY"
    "CSWY" "CSWY"
    "CAUSWAY" "CSWY"
    "CENTER" "CTR"
    "CEN" "CTR"
    "CTR" "CTR"
    "CENT" "CTR"
    "CENTR" "CTR"
    "CENTRE" "CTR"
    "CNTER" "CTR"
    "CNTR" "CTR"
    "CENTERS" "CTRS"
    "CTRS" "CTRS"
    "CIRCLE" "CIR"
    "CIR" "CIR"
    "CIRC" "CIR"
    "CIRCL" "CIR"
    "CRCL" "CIR"
    "CRCLE" "CIR"
    "CIRCLES" "CIRS"
    "CIRS" "CIRS"
    "CLIFF" "CLF"
    "CLF" "CLF"
    "CLIFFS" "CLFS"
    "CLFS" "CLFS"
    "CLUB" "CLB"
    "CLB" "CLB"
    "COMMON" "CMN"
    "CMN" "CMN"
    "CORNER" "COR"
    "COR" "COR"
    "CORNERS" "CORS"
    "CORS" "CORS"
    "COURSE" "CRSE"
    "CRSE" "CRSE"
    "COURT" "CT"
    "CT" "CT"
    "CT" "CTS"
    "CRT" "CT"
    "COURTS" "CTS"
    "CTS" "CTS"
    "COVE" "CV"
    "CV" "CV"
    "COVES" "CVS"
    "CVS" "CVS"
    "CREEK" "CRK"
    "CK" "CRK"
    "CRK" "CRK"
    "CR" "CRK"
    "CRESCENT" "CRES"
    "CRECENT" "CRES"
    "CRES" "CRES"
    "CRESENT" "CRES"
    "CRSCNT" "CRES"
    "CRSENT" "CRES"
    "CRSNT" "CRES"
    "CREST" "CRST"
    "CRST" "CRST"
    "CROSSING" "XING"
    "XING" "XING"
    "CRSSING" "XING"
    "CRSSNG" "XING"
    "CROSSROAD" "XRD"
    "XRD" "XRD"
    "CURVE" "CURV"
    "CURV" "CURV"
    "DALE" "DL"
    "DL" "DL"
    "DAM" "DM"
    "DM" "DM"
    "DIVIDE" "DV"
    "DIV" "DV"
    "DV" "DV"
    "DVD" "DV"
    "DRIVE" "DR"
    "DR" "DR"
    "DRIV" "DR"
    "DRV" "DR"
    "DRIVES" "DRS"
    "DRS" "DRS"
    "ESTATE" "EST"
    "EST" "EST"
    "ESTATES" "ESTS"
    "ESTS" "ESTS"
    "EXPRESSWAY" "EXPY"
    "EXP" "EXPY"
    "EXPY" "EXPY"
    "EXPR" "EXPY"
    "EXPRESS" "EXPY"
    "EXPW" "EXPY"
    "EXTENSION" "EXT"
    "EXT" "EXT"
    "EXTN" "EXT"
    "EXTNSN" "EXT"
    "EXTENSIONS" "EXTS"
    "EXTS" "EXTS"
    "FALL" "FALL"
    "FALLS" "FLS"
    "FLS" "FLS"
    "FERRY" "FRY"
    "FRY" "FRY"
    "FRRY" "FRY"
    "FIELD" "FLD"
    "FLD" "FLD"
    "FIELDS" "FLDS"
    "FLDS" "FLDS"
    "FLAT" "FLT"
    "FLT" "FLT"
    "FLATS" "FLTS"
    "FLTS" "FLTS"
    "FORD" "FRD"
    "FRD" "FRD"
    "FORDS" "FRDS"
    "FRDS" "FRDS"
    "FOREST" "FRST"
    "FRST" "FRST"
    "FORESTS" "FRST"
    "FORGE" "FRG"
    "FORG" "FRG"
    "FRG" "FRG"
    "FORGES" "FRGS"
    "FRGS" "FRGS"
    "FORK" "FRK"
    "FRK" "FRK"
    "FORKS" "FRKS"
    "FRKS" "FRKS"
    "FORT" "FT"
    "FT" "FT"
    "FRT" "FT"
    "FREEWAY" "FWY"
    "FWY" "FWY"
    "FREEWY" "FWY"
    "FRWAY" "FWY"
    "FRWY" "FWY"
    "GARDEN" "GDN"
    "GDN" "GDN"
    "GARDN" "GDN"
    "GRDEN" "GDN"
    "GRDN" "GDN"
    "GARDENS" "GDNS"
    "GDNS" "GDNS"
    "GRDNS" "GDNS"
    "GATEWAY" "GTWY"
    "GTWY" "GTWY"
    "GATEWY" "GTWY"
    "GATWAY" "GTWY"
    "GTWAY" "GTWY"
    "GLEN" "GLN"
    "GLN" "GLN"
    "GLENS" "GLNS"
    "GLNS" "GLNS"
    "GREEN" "GRN"
    "GRN" "GRN"
    "GREENS" "GRNS"
    "GRNS" "GRNS"
    "GROVE" "GRV"
    "GROV" "GRV"
    "GRV" "GRV"
    "GROVES" "GRVS"
    "GRVS" "GRVS"
    "HARBOR" "HBR"
    "HARB" "HBR"
    "HBR" "HBR"
    "HARBR" "HBR"
    "HRBOR" "HBR"
    "HARBORS" "HBRS"
    "HBRS" "HBRS"
    "HAVEN" "HVN"
    "HVN" "HVN"
    "HAVN" "HVN"
    "HEIGHTS" "HTS"
    "HEIGHT" "HTS"
    "HTS" "HTS"
    "HGTS" "HTS"
    "HT" "HTS"
    "HIGHWAY" "HWY"
    "HWY" "HWY"
    "HIGHWY" "HWY"
    "HIWAY" "HWY"
    "HIWY" "HWY"
    "HWAY" "HWY"
    "HILL" "HL"
    "HL" "HL"
    "HILLS" "HLS"
    "HLS" "HLS"
    "HOLLOW" "HOLW"
    "HLLW" "HOLW"
    "HOLW" "HOLW"
    "HOLLOWS" "HOLW"
    "HOLWS" "HOLW"
    "INLET" "INLT"
    "INLT" "INLT"
    "ISLAND" "IS"
    "IS" "IS"
    "ISLND" "IS"
    "ISLANDS" "ISS"
    "ISS" "ISS"
    "ISLNDS" "ISS"
    "ISLE" "ISLE"
    "ISLES" "ISLE"
    "JUNCTION" "JCT"
    "JCT" "JCT"
    "JCTION" "JCT"
    "JCTN" "JCT"
    "JUNCTN" "JCT"
    "JUNCTON" "JCT"
    "JUNCTIONS" "JCTS"
    "JCTNS" "JCTS"
    "JCTS" "JCTS"
    "KEY" "KY"
    "KY" "KY"
    "KEYS" "KYS"
    "KYS" "KYS"
    "KNOLL" "KNL"
    "KNL" "KNL"
    "KNOL" "KNL"
    "KNOLLS" "KNLS"
    "KNLS" "KNLS"
    "LAKE" "LK"
    "LK" "LK"
    "LAKES" "LKS"
    "LKS" "LKS"
    "LAND" "LAND"
    "LANDING" "LNDG"
    "LNDG" "LNDG"
    "LNDNG" "LNDG"
    "LANE" "LN"
    "LA" "LN"
    "LN" "LN"
    "LANES" "LN"
    "LIGHT" "LGT"
    "LGT" "LGT"
    "LIGHTS" "LGTS"
    "LGTS" "LGTS"
    "LOAF" "LF"
    "LF" "LF"
    "LOCK" "LCK"
    "LCK" "LCK"
    "LOCKS" "LCKS"
    "LCKS" "LCKS"
    "LODGE" "LDG"
    "LDG" "LDG"
    "LDGE" "LDG"
    "LODG" "LDG"
    "LOOP" "LOOP"
    "LOOPS" "LOOP"
    "MALL" "MALL"
    "MANOR" "MNR"
    "MNR" "MNR"
    "MANORS" "MNRS"
    "MNRS" "MNRS"
    "MEADOW" "MDW"
    "MDW" "MDW"
    "MEADOWS" "MDWS"
    "MDWS" "MDWS"
    "MEDOWS" "MDWS"
    "MEWS" "MEWS"
    "MILL" "ML"
    "ML" "ML"
    "MILLS" "MLS"
    "MLS" "MLS"
    "MISSION" "MSN"
    "MSN" "MSN"
    "MISSN" "MSN"
    "MSSN" "MSN"
    "MOTORWAY" "MTWY"
    "MTWY" "MTWY"
    "MOUNT" "MT"
    "MNT" "MT"
    "MT" "MT"
    "MOUNTAIN" "MTN"
    "MNTAIN" "MTN"
    "MTN" "MTN"
    "MNTN" "MTN"
    "MOUNTIN" "MTN"
    "MTIN" "MTN"
    "MOUNTAINS" "MTNS"
    "MNTNS" "MTNS"
    "MTNS" "MTNS"
    "NECK" "NCK"
    "NCK" "NCK"
    "ORCHARD" "ORCH"
    "ORCH" "ORCH"
    "ORCHRD" "ORCH"
    "OVAL" "OVAL"
    "OVL" "OVAL"
    "OVERPASS" "OPAS"
    "OPAS" "OPAS"
    "PARK" "PARK"
    "PK" "PARK"
    "PRK" "PARK"
    "PARKS" "PARK"
    "PARKWAY" "PKWY"
    "PKWY" "PKWY"
    "PARKWY" "PKWY"
    "PKWAY" "PKWY"
    "PKY" "PKWY"
    "PARKWAYS" "PKWY"
    "PKWYS" "PKWY"
    "PASS" "PASS"
    "PASSAGE" "PSGE"
    "PSGE" "PSGE"
    "PATH" "PATH"
    "PATHS" "PATH"
    "PIKE" "PIKE"
    "PIKES" "PIKE"
    "PINE" "PNE"
    "PNE" "PNE"
    "PINES" "PNES"
    "PNES" "PNES"
    "PLACE" "PL"
    "PL" "PL"
    "PLAIN" "PLN"
    "PLN" "PLN"
    "PLAINS" "PLNS"
    "PLAINES" "PLNS"
    "PLNS" "PLNS"
    "PLAZA" "PLZ"
    "PLZ" "PLZ"
    "PLZA" "PLZ"
    "POINT" "PT"
    "PT" "PT"
    "POINTS" "PTS"
    "PTS" "PTS"
    "PORT" "PRT"
    "PRT" "PRT"
    "PORTS" "PRTS"
    "PRTS" "PRTS"
    "PRAIRIE" "PR"
    "PR" "PR"
    "PRARIE" "PR"
    "PRR" "PR"
    "RADIAL" "RADL"
    "RAD" "RADL"
    "RADL" "RADL"
    "RADIEL" "RADL"
    "RAMP" "RAMP"
    "RANCH" "RNCH"
    "RNCH" "RNCH"
    "RANCHES" "RNCH"
    "RNCHS" "RNCH"
    "RAPID" "RPD"
    "RPD" "RPD"
    "RAPIDS" "RPDS"
    "RPDS" "RPDS"
    "REST" "RST"
    "RST" "RST"
    "RIDGE" "RDG"
    "RDG" "RDG"
    "RDGE" "RDG"
    "RIDGES" "RDGS"
    "RDGS" "RDGS"
    "RIVER" "RIV"
    "RIV" "RIV"
    "RIVR" "RIV"
    "RVR" "RIV"
    "ROAD" "RD"
    "RD" "RD"
    "ROADS" "RDS"
    "RDS" "RDS"
    "ROUTE" "RTE"
    "RTE" "RTE"
    "ROW" "ROW"
    "RUE" "RUE"
    "RUN" "RUN"
    "SHOAL" "SHL"
    "SHL" "SHL"
    "SHOALS" "SHLS"
    "SHLS" "SHLS"
    "SHORE" "SHR"
    "SHOAR" "SHR"
    "SHR" "SHR"
    "SHORES" "SHRS"
    "SHOARS" "SHRS"
    "SHRS" "SHRS"
    "SKYWAY" "SKWY"
    "SKWY" "SKWY"
    "SPRING" "SPG"
    "SPG" "SPG"
    "SPNG" "SPG"
    "SPRNG" "SPG"
    "SPRINGS" "SPGS"
    "SPGS" "SPGS"
    "SPNGS" "SPGS"
    "SPRNGS" "SPGS"
    "SPUR" "SPUR"
    "SPURS" "SPUR"
    "SQUARE" "SQ"
    "SQ" "SQ"
    "SQR" "SQ"
    "SQRE" "SQ"
    "SQU" "SQ"
    "SQUARES" "SQS"
    "SQRS" "SQS"
    "SQS" "SQS"
    "STATION" "STA"
    "STA" "STA"
    "STATN" "STA"
    "STN" "STA"
    "STRAVENUE" "STRA"
    "STRA" "STRA"
    "STRAV" "STRA"
    "STRAVE" "STRA"
    "STRAVEN" "STRA"
    "STRAVN" "STRA"
    "STRVN" "STRA"
    "STRVNUE" "STRA"
    "STREAM" "STRM"
    "STRM" "STRM"
    "STREME" "STRM"
    "STREET" "ST"
    "ST" "ST"
    "STR" "ST"
    "STRT" "ST"
    "STREETS" "STS"
    "STS" "STS"
    "SUMMIT" "SMT"
    "SMT" "SMT"
    "SUMIT" "SMT"
    "SUMITT" "SMT"
    "TERRACE" "TER"
    "TER" "TER"
    "TERR" "TER"
    "THROUGHWAY" "TRWY"
    "TRWY" "TRWY"
    "TRACE" "TRCE"
    "TRCE" "TRCE"
    "TRACES" "TRCE"
    "TRACK" "TRAK"
    "TRAK" "TRAK"
    "TRACKS" "TRAK"
    "TRK" "TRAK"
    "TRKS" "TRAK"
    "TRAFFICWAY" "TRFY"
    "TRFY" "TRFY"
    "TRAIL" "TRL"
    "TR" "TRL"
    "TRL" "TRL"
    "TRAILS" "TRL"
    "TRLS" "TRL"
    "TUNNEL" "TUNL"
    "TUNEL" "TUNL"
    "TUNL" "TUNL"
    "TUNLS" "TUNL"
    "TUNNELS" "TUNL"
    "TUNNL" "TUNL"
    "TURNPIKE" "TPKE"
    "TPK" "TPKE"
    "TPKE" "TPKE"
    "TRNPK" "TPKE"
    "TRPK" "TPKE"
    "TURNPK" "TPKE"
    "UNDERPASS" "UPAS"
    "UPAS" "UPAS"
    "UNION" "UN"
    "UN" "UN"
    "UNIONS" "UNS"
    "UNS" "UNS"
    "VALLEY" "VLY"
    "VLY" "VLY"
    "VALLY" "VLY"
    "VLLY" "VLY"
    "VALLEYS" "VLYS"
    "VLYS" "VLYS"
    "VIADUCT" "VIA"
    "VDCT" "VIA"
    "VIA" "VIA"
    "VIADCT" "VIA"
    "VIEW" "VW"
    "VW" "VW"
    "VIEWS" "VWS"
    "VWS" "VWS"
    "VILLAGE" "VLG"
    "VILL" "VLG"
    "VLG" "VLG"
    "VILLAG" "VLG"
    "VILLG" "VLG"
    "VILLIAGE" "VLG"
    "VILLAGES" "VLGS"
    "VLGS" "VLGS"
    "VILLE" "VL"
    "VL" "VL"
    "VISTA" "VIS"
    "VIS" "VIS"
    "VIST" "VIS"
    "VST" "VIS"
    "VSTA" "VIS"
    "WALK" "WALK"
    "WALKS" "WALK"
    "WALL" "WALL"
    "WAY" "WAY"
    "WY" "WAY"
    "WAYS" "WAYS"
    "WELL" "WL"
    "WL" "WL"
    "WELLS" "WLS"
    "WLS" "WLS"
    ))


(defrecord UsAddress [street city state zipcode])

(defn- find-last-occurrence-index-of [lookup-map coll]
  (last (keep-indexed #(when (contains? lookup-map %2) %1) coll)))

(defn- split-potential-address-into-parts [s]
  (-> s (s/replace #"," "") s/upper-case (s/split #"\s+")))

(defn extract-us-address [s] 
  (let [parts (split-potential-address-into-parts s)
        state-index (find-last-occurrence-index-of state-abbreviation-lookup parts)
        street-index (find-last-occurrence-index-of street-suffix-lookup parts)
        zip-code (re-matches #"^\d{5}$" (last parts))]
    (when (and zip-code street-index state-index (< street-index state-index))
      (UsAddress. 
        (s/join " " (take (inc street-index) parts))
        (s/join " " (drop (inc street-index) (take state-index parts)))
        (state-abbreviation-lookup (nth parts state-index))
        zip-code))))


(deftype UsAddressExtractor [popular-threshold]
  AttributeExtractor
  (id [this] 
    (symbol "outlierdetection.attribute" "address-us"))
  (extract [this string]
    (extract-us-address string))
  (generate-detector [this attributes-coll]
    (let [usaddress? (partial instance? UsAddress)
          outlier-detector 
          (build-discrete-outlier-detector 
            popular-threshold
            (map usaddress? (extract-values-by-key attributes-coll (id this)))
            :format-callback (fn [popular-percentage value-popular _]
                              (format "%.2f%% of inputs %s consist of a US address" popular-percentage (if value-popular "do" "do not"))))
          value-extractor (id this)]
      (comp outlier-detector usaddress? value-extractor))))


(def ^:private parse-us-address-into-tree
  (letfn [(combine-strings-to-pattern [strings]
            (s/join " | " (map (partial format "'%s'") strings)))]
    (insta/parser
      (format 
      "<address>      = street-address <ows> city <ows> state <ows> zip
       street-address = word+ suffix
       city           = word+
       word           = #'\\w+' | #'\\w+' <ows>    
       suffix         = %s
       state          = %s
       zip            = #'\\d{5}'
       ows            = #'[\\s,]+'"
        (combine-strings-to-pattern (keys street-suffix-lookup))
        (combine-strings-to-pattern (keys state-abbreviation-lookup))))))


(defn- combine-words [parse-words]
  (s/join " " 
          (map (comp s/trim second) parse-words)))

(defn- extract-address-from-parse-tree [parse-tree]
  (let [[street-address-parts city-parts state-parts zip-code-parts] (map rest parse-tree)
        street-address-suffix (get street-suffix-lookup (second (last street-address-parts)))
        street-address-root (combine-words (butlast street-address-parts))
        street-address (s/join " " (list street-address-root street-address-suffix))
        city (combine-words city-parts)
        state (get state-abbreviation-lookup (first state-parts))
        zip-code (first zip-code-parts)]
      (UsAddress. 
        street-address
        city
        state
        zip-code)))

(defn parse-us-address [s]
  (let [parse-tree (parse-us-address-into-tree (s/upper-case s))]
    (when-not (insta/failure? parse-tree)
      (extract-address-from-parse-tree parse-tree))))


(deftype ParsingUsAddressExtractor [popular-threshold]
  AttributeExtractor
  (id [this] 
    (symbol "outlierdetection.attribute" "address-us-parsed"))
  (extract [this string]
    (parse-us-address string))
  (generate-detector [this attributes-coll]
    (let [usaddress? (partial instance? UsAddress)
          outlier-detector 
          (build-discrete-outlier-detector 
            popular-threshold
            (map usaddress? (extract-values-by-key attributes-coll (id this)))
            :format-callback (fn [popular-percentage value-popular _]
                              (format "%.2f%% of inputs %s consist of a US address" popular-percentage (if value-popular "do" "do not"))))
          value-extractor (id this)]
      (comp outlier-detector usaddress? value-extractor))))