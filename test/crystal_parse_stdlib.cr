#!/bin/env crystal

require "colorize"
require "./util"

PASS                      = "pass".colorize.green
FAIL                      = "fail".colorize.red
EXPECTED_TO_FAIL_FILEPATH = "#{__DIR__}/crystal_parse_stdlib_fail.txt"

stdlib_path, stdlib_files = find_stdlib_files

if stdlib_files.empty?
  puts "No stdlib files found"
  exit
end

expected_fail = begin
  File.read(EXPECTED_TO_FAIL_FILEPATH).split
rescue
  [] of String
end

passed = 0
failed = [] of String

stdlib_files.each do |file|
  expected_output = `tree-sitter parse "#{file}" --no-ranges`.strip

  # Only want to compare files that successfully parse
  next unless $?.success?

  test_name = file[(stdlib_path.size + 1)..]

  test = CorpusTest.new(test_name, [":language(crystal)"], File.read(file), expected_output, !test_name.in?(expected_fail))

  if test.run
    passed += 1
  else
    failed << test.name
    exit 1 if ARGV.includes?("--fail-fast")
  end
end

pass_str = (100 * (passed / stdlib_files.size)).format(decimal_places: 2) + "%"
puts <<-EOS

  Total files checked: #{stdlib_files.size.colorize.blue}
  Pass: #{passed} (#{pass_str.colorize.green})

  EOS

if failed != expected_fail
  new_fails = failed - expected_fail
  new_pass = expected_fail - failed
  should_update_failed_list = ARGV.includes?("-u")

  if should_update_failed_list
    File.write(EXPECTED_TO_FAIL_FILEPATH, failed.join("\n") + "\n")
  else
    if !new_fails.empty?
      puts "Following files failed to parse, Use -u to update the list of files expected to fail:"
      new_fails.each { |f| puts " #{f.colorize.red}" }
    end

    if !new_pass.empty?
      puts "Following files now succeed to parse, use -u to update the list of files expected to fail:"
      new_pass.each { |f| puts " #{f.colorize.green}" }
    end
  end

  exit 1 unless should_update_failed_list
end
