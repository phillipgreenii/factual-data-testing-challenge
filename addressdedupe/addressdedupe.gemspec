# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'addressdedupe/version'

Gem::Specification.new do |spec|
  spec.name          = "addressdedupe"
  spec.version       = Addressdedupe::VERSION
  spec.authors       = ["Phillip Green II"]
  spec.email         = ["phillip.green.ii@gmail.com"]
  spec.summary       = "Removes duplicate address from input file"
  spec.description   = ""
  spec.homepage      = ""
  spec.license       = "MIT"

  spec.files         = `git ls-files -z`.split("\x0")
  spec.executables   = spec.files.grep(%r{^bin/}) { |f| File.basename(f) }
  spec.test_files    = spec.files.grep(%r{^(test|spec|features)/})
  spec.require_paths = ["lib"]

  spec.add_dependency "ordinalize"

  spec.add_development_dependency "bundler", "~> 1.6"
  spec.add_development_dependency "rake"
  spec.add_development_dependency "rspec"
end
