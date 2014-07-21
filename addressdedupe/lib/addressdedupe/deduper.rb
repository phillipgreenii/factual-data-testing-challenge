module Addressdedupe
  require 'csv'

  Duplicate = Struct.new(:source_id, :duplicate_id)

  class Deduper

    def dedupe_from file_name
      canonicalizer = Canonicalizer.new
      unique_address = {}
      duplicates = []
      CSV.foreach(file_name,{:headers => :first_row, :col_sep => "\t" }) do |row|
        id = row['id'].strip.to_i
        raw_address = Address.new(row['street address'].strip, 
                                  row['city'].strip, 
                                  row['region'].strip, 
                                  row['postal code'].strip)
        canonicalized_address = canonicalizer.canonicalize(raw_address)

        if unique_address.has_key? canonicalized_address
          duplicates.push(Duplicate.new(unique_address[canonicalized_address],id))
        else
          unique_address[canonicalized_address] = id
        end
      end
      duplicates
    end
  end

end
