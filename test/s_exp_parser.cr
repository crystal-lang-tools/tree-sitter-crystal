#!/bin/env crystal

require "./util"

file = ARGV[0]

expected_output = `tree-sitter parse "#{file}" --no-ranges`.strip

exit 1 unless $?.success?

test = CorpusTest.new(
  File.basename(file),
  [":language(crystal)"],
  File.read(file),
  expected_output,
  true
)

if test.run
else
  exit 1
end
