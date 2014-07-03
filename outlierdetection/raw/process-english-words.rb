File.open('english-words-partial.txt', 'w') do |output|
    File.foreach("english-words-full.txt") do |line|
        output.puts(line) if rand(1..10) == 1
    end
end