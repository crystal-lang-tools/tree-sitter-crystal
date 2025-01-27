require "compiler/crystal/syntax"

require "./s_exp_visitor"

# A tree-sitter corpus test file
class CorpusFile
  getter file_path : String | Path

  def initialize(@file_path)
  end

  def tests
    corpus_tests = [] of CorpusTest

    lines = File.read_lines @file_path

    sections = lines.slice_before(&.starts_with?("==="))

    sections.each_slice(2) do |(header, test_lines)|
      title = header[1].strip
      tags = header[2..-1]

      test_body, expected_output = test_lines[1..-1].join("\n").split(/-{3,}\n/)
      expected_output = expected_output.lines.join("\n").strip

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
  "assign_call"   => "call",
}

# A single corpus test example
class CorpusTest
  getter name : String
  getter tags : Array(String)
  getter test_body : String
  getter expected_output : String

  def initialize(@name, @tags, @test_body, @expected_output, @print_diff = true)
    STRIPPED_NODES.each do |node_name|
      @expected_output = strip_node(@expected_output, node_name)
    end

    RENAMED_NODES.each do |old_name, new_name|
      @expected_output = rename_node(@expected_output, old_name, new_name)
    end

    @expected_output = collapse_macro_content(@expected_output)
  end

  def strip_node(string, node_name)
    string.gsub(/\s*\(#{node_name}\)/, "")
  end

  def rename_node(string, old_name, new_name)
    string.gsub(/\(#{old_name}\b/, "(#{new_name}")
  end

  def collapse_macro_content(string)
    neighboring_macro_contents = /\(macro_content\)\s*\(macro_content\)/

    while string.matches? neighboring_macro_contents
      string = string.gsub(neighboring_macro_contents, "(macro_content)")
    end

    string
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
    io << "<CorpusTest \"" << @name << "\": "
    io << @test_body.lines.size << " lines>"
  end

  @crystal_tree : String?

  def crystal_tree
    @crystal_tree ||= begin
      node = Crystal::Parser.parse(@test_body)

      visitor = SExpVisitor.new
      as_expressions(node).accept(visitor)
      visitor_output = visitor.output.strip
      collapse_macro_content(visitor_output)
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
        puts "  " + expected
      else
        present = [actual]
        absent = [expected]

        while !expected_vs_actual.empty? &&
              expected_vs_actual[0][0] != expected_vs_actual[0][1]
          expected, actual = expected_vs_actual.shift

          present << actual
          absent << expected
        end

        puts present.compact.map { |i| "- " + i }.join("\n").rstrip.colorize.red
        puts absent.compact.map { |i| "+ " + i }.join("\n").rstrip.colorize.green
      end
    end
  end

  def run
    if expected_output == crystal_tree
      puts "PASS: #{@name}".colorize.green
      true
    else
      puts "FAIL: #{@name}".colorize.red
      if @print_diff
        puts "::group::#{@name} formatted dif"
        format_diff
        puts "::endgroup::"
      end
      false
    end
  rescue ex : Crystal::SyntaxException | NotImplementedError
    puts "FAIL: #{@name}".colorize.red
    puts ex
    false
  end
end

def info(key, value)
  puts "#{key}: #{value.colorize.blue}"
end

def find_stdlib_files
  # Find stdlib path and version
  stdlib_path = Crystal::PATH.split(":").find do |path|
    File.exists?(Path[path, "array.cr"])
  end
  abort("Stdlib path not found.") if stdlib_path.nil?

  stdlib_version = File.read(Path[stdlib_path, "VERSION"]).strip

  info("stdlib version", stdlib_version)
  info("stdlib path", stdlib_path)

  # Grab all stdlib files
  {stdlib_path, Dir["#{stdlib_path}/**/*.cr"].sort!}
end
