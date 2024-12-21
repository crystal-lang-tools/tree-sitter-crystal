#!/bin/env crystal

require "colorize"
require "string_scanner"

require "compiler/crystal/syntax"

require "./s_exp_visitor"

class CorpusFile
  getter file_path : String | Path

  def initialize(@file_path)
  end

  def tests
    corpus_tests = [] of CorpusTest

    file_content = File.read(@file_path)
    lines = file_content.lines

    sections = lines.slice_after { |line| line.starts_with?("===") }.skip(1)

    sections.each_slice(2) do |(header, test_lines)|
      title = header[0].strip
      tags = header[1..-2]

      test_body, expected_output = test_lines.join("\n").split(/-{3,}\n/)
      expected_output = expected_output.lines[0..-2].join("\n").strip

      corpus_tests << CorpusTest.new(title, tags, test_body, expected_output)
    end

    corpus_tests
  end
end

STRIPPED_NODE_NAMES = [
  # the crystal parser represents strings & chars the same, regardless of whether they're escaped or not
  "char_escape_sequence",
  "string_escape_sequence",
  "ignored_backslash",

  # comments aren't included in the crystal parser's syntax tree
  "comment",
]

class CorpusTest
  getter name : String
  getter tags : Array(String)
  getter test_body : String
  getter expected_output : String

  def initialize(@name, @tags, @test_body, @expected_output)
    STRIPPED_NODE_NAMES.each do |node_name|
      @expected_output = strip_node(@expected_output, node_name)
    end
  end

  def strip_node(string, node_name)
    string.gsub(/\s*\(#{node_name}\)/, "")
  end

  def runnable?
    @tags.any?(":language(crystal)") && @tags.none?(":skip")
  end

  def skipped?
    @tags.any?(":skip")
  end

  def error?
    @tags.any?(":error")
  end

  def inspect(io)
    io << "<CorpusTest \"#{@name}\": "
    io << "#{@test_body.lines.size} lines>"
  end

  @crystal_tree : String?

  def crystal_tree
    @crystal_tree ||= begin
      node = Crystal::Parser.parse(@test_body)

      visitor = SExpVisitor.new
      as_expressions(node).accept(visitor)
      visitor.output.strip
    end
  end

  def as_expressions(node)
    if node.is_a?(Crystal::Expressions)
      node
    else
      Crystal::Expressions.new([node])
    end
  end

  def run
    if expected_output == crystal_tree
      puts "PASS: #{@name}".colorize.green
      true
    else
      puts "FAIL: #{@name}".colorize.red
      puts "Expected: #{expected_output.bytesize} bytes"
      puts expected_output
      puts "Got: #{crystal_tree.bytesize} bytes"
      puts crystal_tree
      false
    end
  rescue ex : Crystal::SyntaxException
    puts "FAIL: #{@name}".colorize.red
    puts ex
    false
  end
end

tests = Dir["test/corpus/**/*.txt"]
  .map { |file| CorpusFile.new(file).tests }
  .flatten

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

if failures > 0
  puts "#{failures} failures".colorize.red
  exit 1
else
  puts "All tests passed".colorize.green
end
