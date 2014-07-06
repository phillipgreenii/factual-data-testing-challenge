require 'csv'
require 'set'

mapping = Hash.new { |hash, key| hash[key] = SortedSet.new() }

CSV.foreach("official-usps-abbreviations-street-suffixes.csv",{:headers => :first_row}) do |row|
  if row['standard_suffix']
    mapping[row['suffix'].upcase].add(row['standard_suffix'].upcase)
    mapping[row['common_suffix'].upcase].add(row['standard_suffix'].upcase)
    mapping[row['standard_suffix'].upcase].add(row['standard_suffix'].upcase)
  end
end

File.open('street_suffix_hashmap.clj', 'w') do |output| 
  output.puts('(def')
  output.puts(' ^{:const true ')
  output.puts('   :source "https://www.usps.com/send/official-abbreviations.htm"}')
  output.puts('  street-suffix-lookup')
  output.puts("  (hash-map ")
  mapping.each do |key,values|
    values.each { |value| output.puts(format('    "%s" "%s"',key, value))}
  end
  output.puts("    ))")
end
