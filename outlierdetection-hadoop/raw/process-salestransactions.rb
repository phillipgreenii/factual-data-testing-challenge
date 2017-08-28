require 'csv'

File.open('addresses.txt', 'w') do |output| 
  CSV.foreach("Sacramentorealestatetransactions.csv",{:headers => :first_row}) do |row|
    output.puts(format("%s %s, %s %s", row['street'], row['city'], row['state'], row['zip']))
  end
end