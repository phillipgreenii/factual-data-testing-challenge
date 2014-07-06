require 'csv'

File.open('state_hashmap.clj', 'w') do |output| 
  output.puts('(def')
  output.puts(' ^{:const true ')
  output.puts('   :source "https://www.usps.com/send/official-abbreviations.htm"}')
  output.puts('  state-abbreviation-lookup')
    
  output.puts("  (hash-map ")
  CSV.foreach("official-usps-abbreviations-states.csv",{:headers => :first_row}) do |row|
    output.puts(format('    "%s" "%s"', row['standard_abbr'].upcase, row['standard_abbr'].upcase))
    output.puts(format('    "%s" "%s"', row['state'].upcase, row['standard_abbr'].upcase))
  end
  output.puts("    ))")
end
