module Addressdedupe

  require "ordinalize"

  Address = Struct.new(:street, :city, :region,:postal_code)

  class Canonicalizer

    ORDINAL_PATTERN = /^(\d+)[[:alpha:]]+$/
    def correct_ordinal word
      match = ORDINAL_PATTERN.match(word)
      if match
        match[1].to_i.ordinalize.upcase
      else
        word
      end
    end

    CARDINAL_PATTERN = /^([NS]?)([EW]?)$/
    CARDINAL_DB = {'N'=>'NORTH', 'S'=>'SOUTH', 'E'=>'EAST', 'W'=>'WEST' }
    
    def correct_cardinal(word)
      clean_word = word.gsub(/\p{Punct}/, '').upcase
      match = CARDINAL_PATTERN.match(clean_word)
      if match
        cardinals = match.captures.reject(&:empty?)
        CARDINAL_DB.values_at(*cardinals).join(" ")
      else
        word
      end
    end

    def correct_street street
      (beginning, separator, suffix ) = street.rpartition(" ")
      clean_suffix = suffix.gsub(/\p{Punct}/, '')
      clean_beginning = beginning.split(" ").map{ |word| correct_ordinal(correct_cardinal(word))}
      clean_beginning.join(" ") + separator + STREET_SUFFIX_DB.fetch(clean_suffix,suffix)
    end

    def correct_region region
      if STATE_DB.has_key? region
        STATE_DB[region]
      elsif COUNTRY_DB.has_key? region
        COUNTRY_DB[region]
      else 
        region
      end
    end

    def correct_city zip_code, city
      ZIPCODE_DB.fetch(zip_code,{}).fetch(city,city)
    end

    def canonicalize address
      postal_code = address.postal_code.upcase
      region = correct_region(address.region.upcase)
      city = correct_city(postal_code, address.city.upcase)
      street = correct_street(address.street.upcase)

      Address.new(street, city, region, postal_code)
    end
  end

end
