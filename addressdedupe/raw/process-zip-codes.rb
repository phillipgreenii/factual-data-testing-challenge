#!/usr/bin/env ruby
require 'csv'

mapping = Hash.new { |hash, key| hash[key] = Hash.new }

def cleanup_city(city)
  city.upcase.strip.gsub("'","\\\\'")
end

CSV.foreach("raw-zip-code-database.csv",{:headers => :first_row}) do |row|
    zip = row['zip']
    primary_city = cleanup_city(row['primary_city'])
    if row['acceptable_cities']
      acceptable_cities = row['acceptable_cities'].split(',').map { |item| cleanup_city(item) } 
    else
      acceptable_cities = []
    end

    mapping[zip][primary_city] = primary_city
    acceptable_cities.each { |city| mapping[zip][city] = primary_city }  
end

File.open('../lib/addressdedupe/db_zip_code_database.rb', 'w') do |output| 
  output.puts('module Addressdedupe')
  output.puts('  #Source: http://www.unitedstateszipcodes.org/zip-code-database/')
  output.puts('  ZIPCODE_DB = {')
  mapping.each do |zip,cities_lookup|
    output.puts("    '#{zip}' => {")
    cities_lookup.each { |acceptable, primary| output.puts("      '#{acceptable}' => '#{primary}',")}
    output.puts("    },")
  end
  output.puts("  }")
  output.puts('end')
end