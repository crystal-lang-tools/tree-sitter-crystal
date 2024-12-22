#!/bin/env crystal

require "colorize"
require "string_scanner"

require "compiler/crystal/syntax"

require "./s_exp_visitor"

# This script ensures the overall structure of the tree-sitter parse tree matches the Crystal
# compiler's parse tree. This is an extra layer of verification in two ways:
#  1. The corpus tests are parsed by the compiler, making sure they are free of syntax errors.
#  2. Tree-sitter must resolve precedence conflicts and ambiguities in the same way as the compiler.

# A tree-sitter corpus test file
class CorpusFile
  getter file_path : String | Path

  def initialize(@file_path)
  end

  def tests
    corpus_tests = [] of CorpusTest

    file_content = File.read(@file_path)
    lines = file_content.lines

    sections = lines.slice_after(&.starts_with?("===")).skip(1)

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

# These nodes will be removed from the expected output
STRIPPED_NODES = [
  # string escapes are interpreted by the parser and not preserved
  "char_escape_sequence",
  "string_escape_sequence",
  "ignored_backslash",

  # regex escapes are interpreted by the parser and not preserved
  "regex_escape_sequence",
  "regex_character_class",
  "regex_special_match",
  "regex_modifier",

  # heredoc bodies are collapsed by the parser
  "heredoc_end",
  "heredoc_content",
  "heredoc_body",

  # comments aren't included in the crystal parser's syntax tree
  "comment",
]

# These nodes will be renamed in the expected output
RENAMED_NODES = {
  "heredoc_start" => "string",
}

# A single corpus test example
class CorpusTest
  getter name : String
  getter tags : Array(String)
  getter test_body : String
  getter expected_output : String

  def initialize(@name, @tags, @test_body, @expected_output)
    STRIPPED_NODES.each do |node_name|
      @expected_output = strip_node(@expected_output, node_name)
    end

    RENAMED_NODES.each do |old_name, new_name|
      @expected_output = rename_node(@expected_output, old_name, new_name)
    end
  end

  def strip_node(string, node_name)
    string.gsub(/\s*\(#{node_name}\)/, "")
  end

  def rename_node(string, old_name, new_name)
    string.gsub(/\(#{old_name}\)/, "(#{new_name})")
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

  # A very basic diff that just compares lines directly
  def format_diff
    expected_lines = expected_output.lines
    actual_lines = crystal_tree.lines

    while expected_lines.size < actual_lines.size
      expected_lines << ""
    end

    expected_vs_actual = expected_lines.zip?(actual_lines)

    while !expected_vs_actual.empty?
      expected, actual = expected_vs_actual.shift
      if expected == actual
        puts expected
      else
        present = [actual]
        absent = [expected]

        while !expected_vs_actual.empty? &&
              expected_vs_actual[0][0] != expected_vs_actual[0][1]
          expected, actual = expected_vs_actual.shift

          present << actual
          absent << expected
        end

        puts present.compact.join("\n").rstrip.colorize.red
        puts absent.compact.join("\n").rstrip.colorize.green
      end
    end
  end

  def run
    if expected_output == crystal_tree
      puts "PASS: #{@name}".colorize.green
      true
    else
      puts "FAIL: #{@name}".colorize.red
      format_diff
      false
    end
  rescue ex : Crystal::SyntaxException | NotImplementedError
    puts "FAIL: #{@name}".colorize.red
    puts ex
    false
  end
end

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
