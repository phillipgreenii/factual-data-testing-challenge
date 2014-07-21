#!/usr/bin/env ruby
require 'csv'
require 'set'

mapping = Hash.new { |hash, key| hash[key] = SortedSet.new() }

CSV.foreach("raw-country-abbreviations.csv",{:headers => :first_row}) do |row|
  if row['A2']
    mapping[row['A2'].upcase].add(row['A2'].upcase)
    mapping[row['A3'].upcase].add(row['A2'].upcase)
    mapping[row['COUNTRY'].upcase].add(row['A2'].upcase)
  end
end

File.open('../lib/addressdedupe/db_country_mapping.rb', 'w') do |output| 
  output.puts('module Addressdedupe')
  output.puts('  #source http://www.worldatlas.com/aatlas/ctycodes.htm')
  output.puts('  COUNTRY_DB = {')
  mapping.each do |key,values|
    values.each { |value| output.puts("    '#{key}' => '#{value}',")}
  end
  output.puts("  }")
  output.puts('end')
end