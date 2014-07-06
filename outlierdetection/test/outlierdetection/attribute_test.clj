(ns outlierdetection.attribute-test
  (:require [clojure.test :refer :all]
            [outlierdetection.attribute :refer :all ]))

(defn- generate-test-attributes-coll [key values]
  (map #(hash-map key %) values))

(deftest LengthExtractor-test
  (testing "LengthExtractor"
    (testing "extract"
      (testing "should extract length of string"
        (let [x (outlierdetection.attribute.LengthExtractor. 80)]
          (is (= (extract x "potatoes") 8)))))

    (testing "generate-detector"
      (testing "if there is a consistent length, should return nil for consistent length and message for outliers"
        (let [x (outlierdetection.attribute.LengthExtractor. 80)
              attributes-coll (generate-test-attributes-coll (id x) [ 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5  6])
              detector (generate-detector x attributes-coll)]
          (are [i o] (= (detector (hash-map (id x) i)) o)
            5 nil
            6 "94.44% of inputs have length of 5"
            )))
      (testing "if there is a no consistent length, should return nil"
        (let [x (outlierdetection.attribute.LengthExtractor. 80)
              attributes-coll (generate-test-attributes-coll (id x) [ 5 6 5 6 5 5 6 5 6 5 6 5 5 6 5 6])
              detector (generate-detector x attributes-coll)]
          (are [i o] (= (detector (hash-map (id x) i)) o)
            5 nil
            6 nil
            ))))    

    (testing "id"
      (testing "should return correct id"
        (let [x (outlierdetection.attribute.LengthExtractor. 80)]
          (is (= (id x) 'outlierdetection.attribute/length)))))))

(deftest HasLeadingSpaceExtractor-test
  (testing "HasLeadingSpaceExtractor"
    (testing "extract"
      (testing "should determine if string has leading space"
        (let [x (outlierdetection.attribute.HasLeadingSpaceExtractor. 80)]
          (are [i o] (= (extract x i) o)
            ""      false
            "     " true
            " nose" true
            "nose"  false
            "!nose" false
            "nose!" false
            "nose " false))))

    (testing "generate-detector"
      (testing "if most have leading space, should return nil for leading space and message for no leading space"
        (let [x (outlierdetection.attribute.HasLeadingSpaceExtractor. 80)
              attributes-coll (generate-test-attributes-coll (id x) [ true true true true true true true true true true  false])
              detector (generate-detector x attributes-coll)]
          (are [i o] (= (detector (hash-map (id x) i)) o)
            true nil
            false "90.91% of inputs have leading space"
            )))
      (testing "if most do not have leading space, should return nil for no leading space and message for leading space"
        (let [x (outlierdetection.attribute.HasLeadingSpaceExtractor. 80)
              attributes-coll (generate-test-attributes-coll (id x) [ false false false false false false false false false false  true])
              detector (generate-detector x attributes-coll)]
          (are [i o] (= (detector (hash-map (id x) i)) o)
            false nil
            true "90.91% of inputs have no leading space"
            )))
      (testing "if no consistency, should return nil"
        (let [x (outlierdetection.attribute.HasLeadingSpaceExtractor. 80)
              attributes-coll (generate-test-attributes-coll (id x) [ false true false false true false true false false true true])
              detector (generate-detector x attributes-coll)]
          (are [i o] (= (detector (hash-map (id x) i)) o)
            false nil
            true nil
            )))) 

    (testing "id"
      (testing "should return correct id"
        (let [x (outlierdetection.attribute.HasLeadingSpaceExtractor. 80)]
          (is (= (id x) 'outlierdetection.attribute/has-leading-space)))))))

(deftest HasTrailingSpaceExtractor-test
  (testing "HasTrailingSpaceExtractor"
    (testing "extract"
      (testing "should determine if string has trailing space"
        (let [x (outlierdetection.attribute.HasTrailingSpaceExtractor. 80)]
          (are [i o] (= (extract x i) o)
            ""      false
            "     " true
            " nose" false
            "nose"  false
            "!nose" false
            "nose!" false
            "nose " true))))

    (testing "generate-detector"
      (testing "if most have trailing space, should return nil for trailing space and message for no trailing space"
        (let [x (outlierdetection.attribute.HasTrailingSpaceExtractor. 80)
              attributes-coll (generate-test-attributes-coll (id x) [ true true true true true true true true true true  false])
              detector (generate-detector x attributes-coll)]
          (are [i o] (= (detector (hash-map (id x) i)) o)
            true nil
            false "90.91% of inputs have trailing space"
            )))
      (testing "if most do not have trailing space, should return nil for no trailing space and message for trailing space"
        (let [x (outlierdetection.attribute.HasTrailingSpaceExtractor. 80)
              attributes-coll (generate-test-attributes-coll (id x) [ false false false false false false false false false false  true])
              detector (generate-detector x attributes-coll)]
          (are [i o] (= (detector (hash-map (id x) i)) o)
            false nil
            true "90.91% of inputs have no trailing space"
            )))
      (testing "if no consistency, should return nil"
        (let [x (outlierdetection.attribute.HasTrailingSpaceExtractor. 80)
              attributes-coll (generate-test-attributes-coll (id x) [ false true false false true false true false false true true])
              detector (generate-detector x attributes-coll)]
          (are [i o] (= (detector (hash-map (id x) i)) o)
            false nil
            true nil
            )))) 

    (testing "id"
      (testing "should return correct id"
        (let [x (outlierdetection.attribute.HasTrailingSpaceExtractor. 80)]
          (is (= (id x) 'outlierdetection.attribute/has-trailing-space)))))))

(deftest DurationExtractor-test
  (testing "Duration conversion"
    (testing "duration-total-seconds2str"
      (are [i o] (= (duration-total-seconds2str i) o)
        5020  "1:23:40"
       -5020 "-1:23:40"))
    (testing "duration-total-seconds2str"
      (are [i o] (= (duration-str2total-seconds i) o)
        "1:23:40"  5020
       "-1:23:40" -5020))
    (testing "string to total-seconds and back should be equal"
      (are [s] (= ((comp duration-total-seconds2str duration-str2total-seconds) s) s)
        "1:23:40"
        "-1:23:40"))
    (testing "total-seconds to string and back should be equal"
      (are [t] (= ((comp duration-str2total-seconds duration-total-seconds2str) t) t)
         5020
        -5020)))

  (testing "DurationExtractor"
    (testing "extract"
      (testing "should determine if string has a duration"
        (let [x (outlierdetection.attribute.DurationExtractor. 3)]
          (are [i o] (= (extract x i) o)
            ""      nil
            "eggs"  nil
            "34:24" nil
            "23"    nil
            "1:23:40"  5020
            "-1:23:40" -5020))))

    (testing "generate-detector"
      (testing "should return nil for inliers and message for outliers"
        (let [x (outlierdetection.attribute.DurationExtractor. 3)
              attributes-coll (generate-test-attributes-coll (id x) [ 34 54 23 43 42 53 21 23 42 34 23 43 23 12 43  12334])
              detector (generate-detector x attributes-coll)]
          (are [i o] (= (detector (hash-map (id x) i)) o)
            34 nil
            54 nil
            23 nil
            12334  "93.75% of inputs fall within [-2:20:21, 2:47:07]"
            ))))

    (testing "id"
      (testing "should return correct id"
        (let [x (outlierdetection.attribute.DurationExtractor. 3)]
          (is (= (id x) 'outlierdetection.attribute/time-duration)))))))

(deftest NumberExtractor-test
  (testing "NumberExtractor"
    (testing "extract"
      (testing "should extract number if of all string is a number"
        (let [x (outlierdetection.attribute.NumberExtractor. 3)]
          (are [i o] (= (extract x i) o)
            "-0"  0
            "0"   0
            "-2" -2
            "3"   3
            "0.0" 0.0
            "-3.4" -3.4
            "4.6" 4.6
            "123412342.12341234123" 123412342.12341234123
            "-12341234123.12341234123" -12341234123.12341234123
            "123411234123" 123411234123
            "-324232323242342342" -324232323242342342
            "0.0000000000000000000002" 0.0000000000000000000002
            "-0.00000000000000000000023423" -0.00000000000000000000023423
            )))  
      (testing "should return nil if all of string is not a number"
        (let [x (outlierdetection.attribute.NumberExtractor. 3)]
          (are [i] (= (extract x i) nil)
            "potatoes"
            "34m"
            "m56"
            "m3454m"
            "343x545"
            "45.5x"
            "z43.7"
            "23423.23423."
            ".23423."
            "343.23-"
            "--234.234"
            "3242..234"
            "."
            ""
            ".0"
            ".323" 
            "-.2343"
            "-"
            ))))

    (testing "generate-detector"
      (testing "should return nil for inliers and message for outliers"
        (let [x (outlierdetection.attribute.NumberExtractor. 3)
              attributes-coll (generate-test-attributes-coll (id x) [ 1 3 4 5 3 2 5 6 1 2 3 4 2 5 4 1  45])
              detector (generate-detector x attributes-coll)]
          (are [i o] (= (detector (hash-map (id x) i)) o)
            1 nil
            2 nil
            5 nil
            45 "94.12% of inputs fall within [-25.129293, 36.423411]"
            ))))

    (testing "id"
      (testing "should return correct id"
        (let [x (outlierdetection.attribute.NumberExtractor. 3)]
          (is (= (id x) 'outlierdetection.attribute/numerical-value)))))))

(deftest NaiveCharacterTypeCompositionExtractor-test
  (testing "NaiveCharacterTypeCompositionExtractor"
    (testing "extract"
      (testing "should determine composition of string"
        (let [x (outlierdetection.attribute.NaiveCharacterTypeCompositionExtractor. 80)]
          (are [i o] (= (extract x i) o)
            ""  #{}
            "0"  (hash-set :digit)
            "a"  (hash-set :lower-case)
            "A"  (hash-set :upper-case)
            "!"  (hash-set :symbol)
            " "  (hash-set :white-space)
            (str \u007f)  (hash-set :iso-control)
            "\n" (hash-set :iso-control :white-space)
            "abcd ABCD 1234 !@#$\n\r" (hash-set :digit :lower-case :upper-case :symbol :white-space :iso-control)
            )))) 

    (testing "generate-detector"
      (testing "if consistency, should return nil for expected composition and message for inconsistent composition"
        (let [x (outlierdetection.attribute.NaiveCharacterTypeCompositionExtractor. 80)
              attributes-coll (generate-test-attributes-coll (id x) [ (hash-set :digit)
                                                            (hash-set :digit) 
                                                            (hash-set :digit) 
                                                            (hash-set :digit) 
                                                            (hash-set :digit) 
                                                            (hash-set :digit) 
                                                            (hash-set :digit)
                                                            (hash-set :digit)
                                                            (hash-set :digit)
                                                            (hash-set :digit)
                                                            (hash-set :digit)
                                                            (hash-set :digit :symbol)])
              detector (generate-detector x attributes-coll)]
          (are [i o] (= (detector (hash-map (id x) i)) o)
            (hash-set :digit) nil
            (hash-set :digit :symbol) "91.67% of inputs consists of digit"
            )))
      (testing "if no consistency, should return nil"
        (let [x (outlierdetection.attribute.NaiveCharacterTypeCompositionExtractor. 80)
              attributes-coll (generate-test-attributes-coll (id x) [ (hash-set :digit)
                                                            (hash-set :lower-case) 
                                                            (hash-set :digit)
                                                            (hash-set :upper-case)
                                                            (hash-set :digit)
                                                            (hash-set :digit :upper-case)
                                                            (hash-set :upper-case :lower-case)
                                                            (hash-set :digit :symbol)])
              detector (generate-detector x attributes-coll)]
          (are [i o] (= (detector (hash-map (id x) i)) o)
            (hash-set :digit) nil
            (hash-set :digit :symbol) nil
            )))) 

    (testing "id"
      (testing "should return correct id"
        (let [x (outlierdetection.attribute.NaiveCharacterTypeCompositionExtractor. 80)]
          (is (= (id x) 'outlierdetection.attribute/naive-character-type-composition)))))))

(deftest CharacterTypeCompositionExtractor-test
  (testing "CharacterTypeCompositionExtractor"
    (testing "extract"
      (testing "should determine composition of string"
        (let [x (outlierdetection.attribute.CharacterTypeCompositionExtractor. 80)]
          (are [i o] (= (extract x i) o)
            ""  #{}
            "0"  (hash-set :decimal-digit-number)
            "a"  (hash-set :lower-case)
            "A"  (hash-set :upper-case)
            "!"  (hash-set :other-punctuation)
            " "  (hash-set :space-separator)
            (str \u007f)  (hash-set :control)
            (str \u02C0)  (hash-set :modifier-letter)
            (str \u01BB)  (hash-set :other-letter)
            (str \u0316)  (hash-set :non-spacing-mark)
            (str \u20DD)  (hash-set :enclosing-mark)
            (str \u0983)  (hash-set :combining-spacing-mark)
            (str \u2163)  (hash-set :letter-number)
            (str \u2463)  (hash-set :other-number)
            (str \u2028)  (hash-set :line-separator)
            (str \u2029)  (hash-set :paragraph-separator)
            (str \u0600)  (hash-set :format)
            (str \uF8FF)  (hash-set :private-use)
            (str \u2014)  (hash-set :dash-punctuation)
            (str \u007B)  (hash-set :start-punctuation)
            (str \u007D)  (hash-set :end-punctuation)
            (str \u005F)  (hash-set :connector-punctuation)
            (str \u00BF)  (hash-set :other-punctuation)
            (str \u00F7)  (hash-set :math-symbol)
            (str \u20A1)  (hash-set :currency-symbol)
            (str \u00A8)  (hash-set :modifier-symbol)
            (str \u00B0)  (hash-set :other-symbol)
            (str \u201C)  (hash-set :initial-quote-punctuation)
            (str \u201D)  (hash-set :final-quote-punctuation)
            "\n" (hash-set :control)
            "abcd ABCD 1234 !@#$\n\r" (hash-set :lower-case :space-separator :other-punctuation :upper-case :decimal-digit-number :control :currency-symbol)
            )))) 

    (testing "generate-detector"
      (testing "if consistency, should return nil for expected composition and message for inconsistent composition"
        (let [x (outlierdetection.attribute.CharacterTypeCompositionExtractor. 80)
              attributes-coll (generate-test-attributes-coll (id x) [ (hash-set :decimal-digit-number)
                                                            (hash-set :decimal-digit-number) 
                                                            (hash-set :decimal-digit-number)
                                                            (hash-set :decimal-digit-number)
                                                            (hash-set :decimal-digit-number)
                                                            (hash-set :decimal-digit-number)
                                                            (hash-set :decimal-digit-number)
                                                            (hash-set :decimal-digit-number)
                                                            (hash-set :decimal-digit-number)
                                                            (hash-set :decimal-digit-number)
                                                            (hash-set :decimal-digit-number)
                                                            (hash-set :decimal-digit-number :currency-symbol)])
              detector (generate-detector x attributes-coll)]
          (are [i o] (= (detector (hash-map (id x) i)) o)
            (hash-set :decimal-digit-number) nil
            (hash-set :decimal-digit-number :currency-symbol) "91.67% of inputs consists of decimal digit number"
            )))
      (testing "if no consistency, should return nil"
        (let [x (outlierdetection.attribute.CharacterTypeCompositionExtractor. 80)
              attributes-coll (generate-test-attributes-coll (id x) [ (hash-set :decimal-digit-number)
                                                            (hash-set :lower-case) 
                                                            (hash-set :decimal-digit-number)
                                                            (hash-set :upper-case)
                                                            (hash-set :decimal-digit-number)
                                                            (hash-set :decimal-digit-number :upper-case)
                                                            (hash-set :upper-case :lower-case)
                                                            (hash-set :decimal-digit-number :currency-symbol)])
              detector (generate-detector x attributes-coll)]
          (are [i o] (= (detector (hash-map (id x) i)) o)
            (hash-set :decimal-digit-number) nil
            (hash-set :decimal-digit-number :currency-symbol) nil
            )))) 

    (testing "id"
      (testing "should return correct id"
        (let [x (outlierdetection.attribute.CharacterTypeCompositionExtractor. 80)]
          (is (= (id x) 'outlierdetection.attribute/character-type-composition)))))))


(deftest UsAddressExtractor-test
  (testing "UsAddressExtractor"
    (testing "extract"
      (testing "should determine if string is a US address"
        (let [x (outlierdetection.attribute.UsAddressExtractor. 80)]
          (are [i o] (= (extract x i) o)
            "3526 HIGH ST SACRAMENTO CA 95838"  (outlierdetection.attribute.UsAddress. "3526 HIGH ST" "SACRAMENTO" "CA" "95838")
            "3526 HIGH ST SACRAMENTO, CA 95838"  (outlierdetection.attribute.UsAddress. "3526 HIGH ST" "SACRAMENTO" "CA" "95838")
            "3526 HIGH ST SACRAMENTO California 95838"  (outlierdetection.attribute.UsAddress. "3526 HIGH ST" "SACRAMENTO" "CA" "95838")
            "3526 HIGH ST SACRAMENTO, California 95838"  (outlierdetection.attribute.UsAddress. "3526 HIGH ST" "SACRAMENTO" "CA" "95838")
            "3526 high st sacramento ca 95838"  (outlierdetection.attribute.UsAddress. "3526 HIGH ST" "SACRAMENTO" "CA" "95838")
            "3526 high st sacramento, ca 95838"  (outlierdetection.attribute.UsAddress. "3526 HIGH ST" "SACRAMENTO" "CA" "95838")
            "3526 CALIFORNIA ST SACRAMENTO CA 95838"  (outlierdetection.attribute.UsAddress. "3526 CALIFORNIA ST" "SACRAMENTO" "CA" "95838")
            "3526 CALIFORNIA ST SACRAMENTO, CA 95838"  (outlierdetection.attribute.UsAddress. "3526 CALIFORNIA ST" "SACRAMENTO" "CA" "95838")
            "3526 AVENUE ST SACRAMENTO CA 95838"  (outlierdetection.attribute.UsAddress. "3526 AVENUE ST" "SACRAMENTO" "CA" "95838")
            "3526 AVENUE ST SACRAMENTO, CA 95838"  (outlierdetection.attribute.UsAddress. "3526 AVENUE ST" "SACRAMENTO" "CA" "95838")
            "California"  nil
            "CA"  nil
            ""  nil
            "SACRAMENTO"  nil
            "95838"  nil
            "3526 HIGH STREET"  nil
            "304 867 5309" nil)))) 

    (testing "generate-detector"
      (testing "if consistently addresses, should return nil for address and explaination for other"
        (let [x (outlierdetection.attribute.UsAddressExtractor. 80)
              attributes-coll (generate-test-attributes-coll (id x) [(outlierdetection.attribute.UsAddress. "3526 HIGH ST" "SACRAMENTO" "CA" "95838")
                                                                     (outlierdetection.attribute.UsAddress. "51 OMAHA CT" "SACRAMENTO" "CA" "95823")
                                                                     (outlierdetection.attribute.UsAddress. "2796 BRANCH ST" "SACRAMENTO" "CA" "95815")
                                                                     (outlierdetection.attribute.UsAddress. "2805 JANETTE WAY" "SACRAMENTO" "CA" "95815")
                                                                     (outlierdetection.attribute.UsAddress. "6001 MCMAHON DR" "SACRAMENTO" "CA" "95824")
                                                                     (outlierdetection.attribute.UsAddress. "5828 PEPPERMILL CT" "SACRAMENTO" "CA" "95841")
                                                                     (outlierdetection.attribute.UsAddress. "6048 OGDEN NASH WAY" "SACRAMENTO" "CA" "95842")
                                                                     nil])
              detector (generate-detector x attributes-coll)]
          (are [i o] (= (detector (hash-map (id x) i)) o)
            (outlierdetection.attribute.UsAddress. "3526 HIGH ST" "SACRAMENTO" "CA" "95838") nil
            nil "87.50% of inputs do consist of a US address"
            )))
      (testing "if consistently not addresses, should return nil for not address and explaination for address"
        (let [x (outlierdetection.attribute.UsAddressExtractor. 80)
              attributes-coll (generate-test-attributes-coll (id x) [nil
                                                                     nil
                                                                     nil
                                                                     nil
                                                                     nil
                                                                     (outlierdetection.attribute.UsAddress. "5828 PEPPERMILL CT" "SACRAMENTO" "CA" "95841")
                                                                     nil
                                                                     nil])
              detector (generate-detector x attributes-coll)]
          (are [i o] (= (detector (hash-map (id x) i)) o)
            (outlierdetection.attribute.UsAddress. "3526 HIGH ST" "SACRAMENTO" "CA" "95838") "87.50% of inputs do not consist of a US address"
            nil nil
            )))
      (testing "if no consistency, should return nil"
        (let [x (outlierdetection.attribute.UsAddressExtractor. 80)
              attributes-coll (generate-test-attributes-coll (id x)  [(outlierdetection.attribute.UsAddress. "3526 HIGH ST" "SACRAMENTO" "CA" "95838")
                                                                     (outlierdetection.attribute.UsAddress.  "51 OMAHA CT" "SACRAMENTO" "CA" "95823")
                                                                     nil
                                                                     (outlierdetection.attribute.UsAddress. "2805 JANETTE WAY" "SACRAMENTO" "CA" "95815")
                                                                     (outlierdetection.attribute.UsAddress. "6001 MCMAHON DR" "SACRAMENTO" "CA" "95824")
                                                                     nil
                                                                     (outlierdetection.attribute.UsAddress. "6048 OGDEN NASH WAY" "SACRAMENTO" "CA" "95842")
                                                                     nil])
              detector (generate-detector x attributes-coll)]
          (are [i o] (= (detector (hash-map (id x) i)) o)
            (outlierdetection.attribute.UsAddress. "3526 HIGH ST" "SACRAMENTO" "CA" "95838") nil
            nil nil
            )))) 

    (testing "id"
      (testing "should return correct id"
        (let [x (outlierdetection.attribute.UsAddressExtractor. 80)]
          (is (= (id x) 'outlierdetection.attribute/address-us)))))))




