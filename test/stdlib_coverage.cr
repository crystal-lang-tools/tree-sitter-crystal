#!/bin/env crystal

{% skip_file unless flag?(:linux) || flag?(:darwin) %}

require "colorize"

require "./util"

PASS                      = "pass".colorize.green
FAIL                      = "fail".colorize.red
EXPECTED_TO_FAIL_FILEPATH = "#{__DIR__}/stdlib_coverage_expected_to_fail.txt"

MACRO_REGEX = /{{|{%|\bmacro\b/

class Test
  getter file_path : String
  getter label : String
  getter elapsed : Time::Span = Time::Span::ZERO

  def initialize(@file_path, @label)
  end

  def execute : Process::Status
    process = Process.new("tree-sitter parse #{@file_path}", shell: true, error: :inherit)
    start_time = Time.monotonic
    status = process.wait
    @elapsed = Time.monotonic - start_time
    status
  end
end

stdlib_path, stdlib_files = find_stdlib_files
pass = 0

expected_fail = begin
  File.read(EXPECTED_TO_FAIL_FILEPATH).split
rescue
  [] of String
end
failed = [] of String

if stdlib_files.empty?
  puts "No stdlib files found"
  exit
end

stdlib_files.each do |stdlib_file|
  test = Test.new(stdlib_file, stdlib_file[(stdlib_path.size + 1)..])
  test_status = test.execute

  if !test_status.exit_reason.normal?
    # the parser didn't exit normally. maybe a failed assertion, or a segfault
    puts "test did not exit normally: #{test_status.exit_reason} (status code #{test_status.exit_code})"
    abort("encountered a serious problem parsing #{stdlib_file}")
  end

  success = test_status.success?

  if success
    pass += 1
  else
    failed << test.label
  end

  is_macro = File.read(stdlib_file).matches?(MACRO_REGEX)

  elapsed_ms = sprintf("%8.3fms", test.elapsed.total_milliseconds).colorize.dark_gray
  # Why 63? So we match 80 columns.
  printf("%-63s %s %s %s\n", test.label, success ? PASS : FAIL, is_macro ? "macro" : "     ", elapsed_ms)
end

pass_str = (100 * (pass / stdlib_files.size)).format(decimal_places: 2) + "%"
puts <<-EOS

        Total files checked: #{stdlib_files.size.colorize.blue}
        Pass: #{pass} (#{pass_str.colorize.green})

        EOS

if failed != expected_fail
  new_fails = failed - expected_fail
  new_pass = expected_fail - failed
  should_update_failed_list = ARGV.includes?("-u")

  if should_update_failed_list
    File.write(EXPECTED_TO_FAIL_FILEPATH, failed.join("\n"))
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
