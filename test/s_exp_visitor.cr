require "compiler/crystal/syntax"

class SExpVisitor < Crystal::Visitor
  include Crystal

  @io = IO::Memory.new
  @indent = 0
  @fields = [] of String

  def initialize
  end

  def output
    @io.to_s
  end

  def print_indent
    @io.puts
    @io << ("  " * @indent)
  end

  def print_node(name : String)
    print_indent
    @io << active_field << "(" << name << ")"
  end

  def enter_node(name : String)
    print_indent
    @io << active_field << "(" << name
    @indent += 1
  end

  def exit_node
    @io << ")"
    @indent -= 1
  end

  def visit(node : Expressions)
    enter_node("expressions")
    true
  end

  def end_visit(node : Expressions)
    exit_node()
  end

  def field(name : String)
    @fields << name
    yield
    @fields.pop
  end

  def active_field : String
    if @fields.empty?
      ""
    else
      "#{@fields.last}: "
    end
  end

  ##########
  # Literals
  ##########

  def visit(node : NilLiteral)
    print_node("nil")
    false
  end

  def visit(node : BoolLiteral)
    if node.value == true
      print_node("true")
    else
      print_node("false")
    end
    false
  end

  def visit(node : NumberLiteral)
    if node.kind.f32? || node.kind.f64?
      print_node("float")
    else
      print_node("integer")
    end
    false
  end

  def visit(node : CharLiteral)
    print_node("char")
    false
  end

  def visit(node : StringLiteral)
    print_node("string")
    false
  end

  ##############
  # Method calls
  ##############

  def visit(node : Call)
    named_args = node.named_args

    ident_match = /[0-9A-Za-z_\N{U+00a0}-\N{U+10ffff}]/

    is_plain_identifier_call = !node.has_parentheses? &&
                               !node.global? &&
                               node.name.ends_with?(ident_match) &&
                               node.obj.nil? &&
                               node.args.empty? &&
                               node.block_arg.nil? &&
                               node.block.nil? &&
                               (named_args.nil? || named_args.empty?)

    if is_plain_identifier_call
      # the tree-sitter parser doesn't have semantic context to identify this as a call,
      # so we output as a plain identifier
      enter_node("identifier")
      return false
    end

    enter_node("call")

    field("reciever") do
      node.obj.try &.accept(self)
    end

    field "method" do
      print_node("identifier")
    end

    field "arguments" do
      if node.args.size > 0
        print_node("<todo>")
      end

      if named_args && named_args.size > 0
        print_node("<todo>")
      end
    end

    field "block" do
      node.block_arg.try &.accept(self)
      node.block.try &.accept(self)
    end

    false
  end

  def end_visit(node : Call)
    exit_node
  end

  def visit(node)
    raise "visit_node unimplemented for #{node.class}"
  end
end
