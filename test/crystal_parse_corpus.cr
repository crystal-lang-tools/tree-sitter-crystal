#!/bin/env crystal

require "colorize"
require "string_scanner"

require "./util"

# This script ensures the overall structure of the tree-sitter parse tree matches the Crystal
# compiler's parse tree. This is an extra layer of verification in two ways:
#  1. The corpus tests are parsed by the compiler, making sure they are free of syntax errors.
#  2. Tree-sitter must resolve precedence conflicts and ambiguities in the same way as the compiler.

tests = Dir["test/corpus/**/*.txt"]
  .flat_map { |file| CorpusFile.new(file).tests }

results = tests.select(&.runnable?).map do |test|
  test.run
end

puts

unverified_tests = tests.select do |test|
  !test.runnable? && !test.skipped? && !test.error?
end

if unverified_tests.size > 0
  puts "#{unverified_tests.size} unverified tests".colorize.yellow
end

failures = results.count(false)
passes = results.count(true)

puts "#{passes} tests passed".colorize.green

if failures > 0
  puts "#{failures} failures".colorize.red
  exit 1
end
