#!/bin/env crystal

{% skip_file unless flag?(:linux) || flag?(:darwin) %}

require "colorize"

PASS                      = "pass".colorize.green
FAIL                      = "fail".colorize.red
EXPECTED_TO_FAIL_FILEPATH = "#{__DIR__}/stdlib_coverage_expected_to_fail.txt"

class Test
  getter file_path : String
  getter label : String
  getter elapsed : Time::Span = Time::Span::ZERO

  def initialize(@file_path, @label)
  end

  def execute : Bool
    process = Process.new("tree-sitter parse #{@file_path}", shell: true)
    start_time = Time.monotonic
    status = process.wait
    @elapsed = Time.monotonic - start_time
    status.success?
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
  success = test.execute

  if success
    pass += 1
  else
    failed << test.label
  end
  elapsed_ms = "#{test.elapsed.total_milliseconds}ms".colorize.dark_gray
  # Why 63? So we match 80 columns.
  printf("%-63s %s %s\n", test.label, success ? PASS : FAIL, elapsed_ms)
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