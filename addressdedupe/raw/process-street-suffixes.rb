#!/usr/bin/env ruby
require 'csv'
require 'set'

mapping = Hash.new { |hash, key| hash[key] = SortedSet.new() }

CSV.foreach("raw-official-usps-abbreviations-street-suffixes.csv",{:headers => :first_row}) do |row|
  if row['standard_suffix']
    mapping[row['suffix'].upcase].add(row['standard_suffix'].upcase)
    mapping[row['common_suffix'].upcase].add(row['standard_suffix'].upcase)
    mapping[row['standard_suffix'].upcase].add(row['standard_suffix'].upcase)
  end
end

File.open('../lib/addressdedupe/db_street_suffix_mapping.rb', 'w') do |output| 
  output.puts('module Addressdedupe')
  output.puts('  #source https://www.usps.com/send/official-abbreviations.htm')
  output.puts('  STREET_SUFFIX_DB = {')
  mapping.each do |key,values|
    values.each { |value| output.puts("    '#{key}' => '#{value}',")}
  end
  output.puts("  }")
  output.puts('end')
end