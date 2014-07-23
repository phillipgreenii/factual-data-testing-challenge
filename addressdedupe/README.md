# Addressdedupe

This is my implementation to Problem 2: Dedupe found at [Data Testing Challenge](http://devblog.factual.com/data-testing-challenge)

## Installation

Download from [https://github.com/phillipgreenii/factual-data-testing-challenge].

    $ gem build addressdedupe.gemspec 
    $ gem install addressdedupe

## Usage

    $  dedupe-addresses input_file_name

### Input file format

The input file must be a tab delimited file with the following column headers: 'id', 'street address', 'city', 'region', 'postal code'.

### Output

Each line of the output represents a duplicate address found.  The first value of the output pair is the ID of the source address and the secoond is the ID of the duplicate.

## Options

None

## License

Copyright © 2014 Phillip Green II

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
