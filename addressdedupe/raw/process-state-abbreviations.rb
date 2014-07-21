#!/usr/bin/env ruby
require 'csv'

File.open('../lib/addressdedupe/db_state_mapping.rb', 'w') do |output| 
  output.puts('module Addressdedupe')

  output.puts('  #source https://www.usps.com/send/official-abbreviations.htm')
  output.puts('  STATE_DB = {')
  CSV.foreach("raw-official-usps-abbreviations-states.csv",{:headers => :first_row}) do |row|
    state = row['state']
    standard_abbr = row['standard_abbr']
    output.puts("    '#{standard_abbr}' => '#{standard_abbr}',")
    output.puts("    '#{state}' => '#{standard_abbr}',")
  end
  output.puts("  }")

  output.puts('end')
end
