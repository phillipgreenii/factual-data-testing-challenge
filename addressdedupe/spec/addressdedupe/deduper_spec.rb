require 'spec_helper'

describe Addressdedupe::Deduper do
  let(:deduper) { Addressdedupe::Deduper.new}

  describe '#dedupe_from' do
    it 'should extract duplicates from test data' do

      duplicates = deduper.dedupe_from('spec/addressdedupe/test_addresses.txt')
    
      expect(duplicates).to eq [
        Addressdedupe::Duplicate.new(1,2),
        Addressdedupe::Duplicate.new(4,5)
      ]
    end  

    it 'should extract no duplicates from mcdonalds data' do

      duplicates = deduper.dedupe_from('spec/addressdedupe/mcdonalds_addresses_no_duplicates.txt')
    
      expect(duplicates).to eq [
      ]
    end  

    it 'should extract duplicates from mcdonalds data' do

      duplicates = deduper.dedupe_from('spec/addressdedupe/mcdonalds_addresses_duplicates.txt')
    
      expect(duplicates).to eq [
        Addressdedupe::Duplicate.new(3,1003),
        Addressdedupe::Duplicate.new(3,2003),
        Addressdedupe::Duplicate.new(6,1006),
        Addressdedupe::Duplicate.new(12,1012),
        Addressdedupe::Duplicate.new(18,1018),
        Addressdedupe::Duplicate.new(19,1019),
        Addressdedupe::Duplicate.new(35,1035),
        Addressdedupe::Duplicate.new(65,1065),
        Addressdedupe::Duplicate.new(77,1077)
      ]
    end 

  end

end