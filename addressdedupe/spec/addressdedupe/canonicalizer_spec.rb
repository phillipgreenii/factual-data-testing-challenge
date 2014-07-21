require 'spec_helper'

describe Addressdedupe::Canonicalizer do
  let(:canonicalizer) { Addressdedupe::Canonicalizer.new}

  describe '#correct_ordinal' do
    it 'should ordinalize 1st' do
      expect(canonicalizer.correct_ordinal('1st')).to eq "FIRST"
    end 

    it 'should ordinalize 1er' do
      expect(canonicalizer.correct_ordinal('1er')).to eq "FIRST"
    end 


    it 'should not ordinalize 1' do
      expect(canonicalizer.correct_ordinal('1')).to eq "1"
    end 

    it 'should not ordinalize eggs' do
      expect(canonicalizer.correct_ordinal('EGGS')).to eq "EGGS"
    end 

  end

  describe '#correct_cardinal' do
    it 'should correct S' do
      expect(canonicalizer.correct_cardinal('S')).to eq "SOUTH"
    end 

    it 'should correct N' do
      expect(canonicalizer.correct_cardinal('N')).to eq "NORTH"
    end 

    it 'should correct E' do
      expect(canonicalizer.correct_cardinal('E')).to eq "EAST"
    end 

    it 'should correct W' do
      expect(canonicalizer.correct_cardinal('W')).to eq "WEST"
    end 

    it 'should strip single period' do
      expect(canonicalizer.correct_cardinal('W.')).to eq "WEST"
    end 

    it 'should correct SE' do
      expect(canonicalizer.correct_cardinal('SE')).to eq "SOUTH EAST"
    end 

    it 'should correct SW' do
      expect(canonicalizer.correct_cardinal('SW')).to eq "SOUTH WEST"
    end 

    it 'should correct NE' do
      expect(canonicalizer.correct_cardinal('NE')).to eq "NORTH EAST"
    end 

    it 'should correct NW' do
      expect(canonicalizer.correct_cardinal('NW')).to eq "NORTH WEST"
    end 

    it 'should strip multiple periods' do
      expect(canonicalizer.correct_cardinal('N.W.')).to eq "NORTH WEST"
    end 

    it 'should not correct 1' do
      expect(canonicalizer.correct_cardinal('1')).to eq "1"
    end 

    it 'should not ordinalize eggs' do
      expect(canonicalizer.correct_cardinal('EGGS')).to eq "EGGS"
    end 

  end

  describe '#canonicalize' do
    def expect_canonicalize input_parts, expected_parts
      expect(canonicalizer.canonicalize(Addressdedupe::Address.new(*input_parts)))
                                 .to eq Addressdedupe::Address.new(*expected_parts)
    end

    it 'should canonicalize to canonical address' do
      expect_canonicalize(["1234 street st","cityburg","pa","12345"],
                          ["1234 STREET ST", "CITYBURG", "PA", "12345"])
    end   

    it 'should strip out punctionation from suffix' do
      expect_canonicalize(["1234 street st.","cityburg","pennsylvania","12345"],
                          ["1234 STREET ST", "CITYBURG", "PA", "12345"])
    end  

    it 'should replace state with abbreviation' do
      expect_canonicalize(["1234 street st","cityburg","pennsylvania","12345"],
                          ["1234 STREET ST", "CITYBURG", "PA", "12345"])
    end  

    it 'should correct street abbreviation' do
      expect_canonicalize(["1234 street str","cityburg","pa","12345"],
                          ["1234 STREET ST", "CITYBURG", "PA", "12345"])
    end  

    it 'should correct city' do
      expect_canonicalize(["1234 street st","west los angeles","ca","90025"],
                          ["1234 STREET ST", "LOS ANGELES", "CA", "90025"])
    end 
   
    it 'should handle suffix in city' do
      expect_canonicalize(["1 Government Dr","St Louis","MO","63110"],
                          ["1 GOVERNMENT DR", "ST LOUIS", "MO", "63110"])
    end 
    
    it 'should correct numeric street name formats' do
      expect_canonicalize(["123 1st Street","cityburg","PA","12345"],
                          ["123 FIRST ST", "CITYBURG", "PA", "12345"])
    end 
    
    it 'should correct numeric street name formats' do
      expect_canonicalize(["123 1er Street","Paris","France","75008"],
                          ["123 FIRST ST", "PARIS", "FR", "75008"])
    end 

    it 'should not correct irregular spanning addresses (OR)' do
      # 97635 spans New Pine Creek OR and Adin, CA but zipcode db will only have one city
      expect_canonicalize(["1234 street st","New Pine Creek","OR","97635"],
                          ["1234 STREET ST", "NEW PINE CREEK", "OR", "97635"])
    end 

    it 'should not correct irregular spanning addresses (CA)' do
      # 97635 spans New Pine Creek OR and Adin, CA but zipcode db will only have one city
      expect_canonicalize(["1234 street st","Adin","CA","97635"],
                          ["1234 STREET ST", "ADIN", "CA", "97635"])
    end 

    it 'should correct single cardinal quantifiers' do
      expect_canonicalize(["1234 N. street st","cityburg","pa","12345"],
                          ["1234 NORTH STREET ST", "CITYBURG", "PA", "12345"])
    end 

    it 'should correct mutliple cardinal quantifiers' do
      expect_canonicalize(["1234 N W street st","cityburg","pa","12345"],
                          ["1234 NORTH WEST STREET ST", "CITYBURG", "PA", "12345"])
    end 

    it 'should correct combo cardinal quantifiers' do
      expect_canonicalize(["1234 NW street st","cityburg","pa","12345"],
                          ["1234 NORTH WEST STREET ST", "CITYBURG", "PA", "12345"])
    end 

  end

end