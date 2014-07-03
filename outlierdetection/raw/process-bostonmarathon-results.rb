require 'csv'

File.open('bostonmarathon-2012-results.txt', 'w') do |output| 
  CSV.foreach("bostonmarathon-2012-results.csv",{:headers => :first_row}) do |row|
    total_seconds = row['net'].to_f * 60

    hours, remainder = total_seconds.divmod(60**2)
    minutes, seconds = remainder.divmod(60)

    output.puts(format("%02d:%02d:%02d", hours, minutes, seconds))
  end
end