require "compiler/crystal/syntax"

# Visits a Crystal AST and outputs a parse tree in the same s-exp format used by tree-sitter
class SExpVisitor < Crystal::Visitor
  include Crystal

  @io = IO::Memory.new
  @indent = 0
  @fields = [] of String?
  @pending_alias : String? = nil

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
    @io << active_field << "(" << with_alias(name) << ")"
  end

  def enter_node(name : String)
    print_indent
    @io << active_field << "(" << with_alias(name)
    @indent += 1
    @fields << nil
  end

  def exit_node
    @io << ")"
    @indent -= 1
    @fields.pop
  end

  def in_node(name : String, &)
    enter_node(name)
    yield
  ensure
    exit_node
  end

  # Force the next node to be printed with the given `alias_name`
  def alias_next_node!(alias_name : String)
    @pending_alias = alias_name
  end

  def with_alias(name : String)
    if @pending_alias
      name = @pending_alias
      @pending_alias = nil
    end
    name
  end

  def visit(node : Expressions)
    # collapse (expressions (nop)) into (nil)
    if node.expressions.size == 1 && node.expressions.first.is_a?(Nop)
      enter_node("nil")

      return false
    end

    enter_node("expressions")
    true
  end

  def end_visit(node : Expressions)
    exit_node()
  end

  def field(name : String, &)
    @fields << name
    yield
    @fields.pop
  end

  def active_field : String
    if @fields.empty? || @fields.last.nil?
      ""
    else
      "#{@fields.last}: "
    end
  end

  #############
  # Identifiers
  #############

  def visit(node : Crystal::Path)
    print_node("constant")
    false
  end

  def visit(node : Var)
    if node.name == "self"
      print_node("self")
      return false
    end

    print_node("identifier")
    false
  end

  def visit(node : Self)
    print_node("self")
    false
  end

  def visit(node : InstanceVar)
    print_node("instance_var")
    false
  end

  def visit(node : ClassVar)
    print_node("class_var")
    false
  end

  ##########
  # Literals
  ##########

  def visit(node : Nop)
    false
  end

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

  def visit(node : StringInterpolation)
    in_node "string" do
      node.expressions.each do |interp|
        next if interp.is_a?(StringLiteral)

        in_node("interpolation") { interp.accept self }
      end
    end

    false
  end

  def visit(node : SymbolLiteral)
    print_node("symbol")
    false
  end

  def visit(node : RegexLiteral)
    in_node("regex") do
      if (val = node.value).is_a?(StringInterpolation)
        val.expressions.each do |interp|
          next if interp.is_a?(StringLiteral)

          in_node("interpolation") { interp.accept self }
        end
      end
    end

    false
  end

  ############
  # Structures
  ############

  def visit(node : RangeLiteral)
    in_node("range") do
      field "begin" do
        node.from.accept self
      end

      field "operator" do
        print_node("operator")
      end

      field "end" do
        node.to.accept self
      end
    end

    false
  end

  def visit(node : TupleLiteral)
    enter_node("tuple")
    true
  end

  def end_visit(node : TupleLiteral)
    exit_node
  end

  def visit(node : NamedTupleLiteral)
    in_node("named_tuple") do
      node.entries.each do |entry|
        in_node("named_expr") do
          field "name" do
            print_node("identifier")
          end

          entry.value.accept self
        end
      end
    end

    false
  end

  def visit(node : HashLiteral)
    in_node("hash") do
      node.entries.each do |entry|
        in_node("hash_entry") do
          entry.key.accept(self)
          entry.value.accept(self)
        end
      end

      if (of = node.of)
        field "of_key" do
          of.key.accept(self)
        end

        field "of_value" do
          of.value.accept(self)
        end
      end
    end

    false
  end

  def visit(node : ArrayLiteral)
    in_node("array") do
      node.elements.each(&.accept(self))

      field "of" do
        if ["::Symbol", "::String"].includes? node.of.to_s
          # %i and %w arrays inject a "of" node automatically, so ignore it
          next
        end

        node.of.try(&.accept(self))
      end
    end

    false
  end

  #####################
  # Classes and methods
  #####################

  def visit(node : Def)
    in_node("method_def") do
      field "visibility" do
        case node.visibility
        in .public?
        in .protected? then print_node("protected")
        in .private?   then print_node("private")
        end
      end

      field "receiver" do
        node.receiver.try &.accept self
      end

      field "name" do
        if operator? node.name
          print_node("operator")
        else
          print_node("identifier")
        end
      end

      field "params" do
        if node.args.size > 0
          in_node("param_list") do
            node.args.each &.accept self
          end
        end
      end

      field "type" do
        node.return_type.try &.accept self
      end

      field "forall" do
        # TODO
      end

      field "body" do
        body = node.body

        case body
        when Nop
          break
        when Expressions
          # keep it
        else
          body = Expressions.new([body])
        end

        body.accept self
      end
    end

    false
  end

  def visit(node : Arg)
    in_node("param") do
      field "extern_name" do
        if node.external_name != node.name
          print_node("identifier")
        end
      end

      field "name" do
        print_node("identifier")
      end

      field "type" do
        node.restriction.try &.accept self
      end

      field "default" do
        node.default_value.try &.accept self
      end
    end

    false
  end

  def visit(node : ProcLiteral)
    in_node("proc") do
      field "params" do
        if node.def.args.size > 0
          in_node("param_list") do
            node.def.args.each &.accept self
          end
        end
      end

      field "type" do
        node.def.return_type.try(&.accept self)
      end

      field "block" do
        body = node.def.body

        case body
        when Nop
          # keep it
        when Expressions
          # keep it
        else
          body = Expressions.new([body])
        end

        in_node("block") do
          field "body" do
            body.accept self
          end
        end
      end
    end

    false
  end

  def visit(node : ProcPointer)
    in_node("method_proc") do
      field "receiver" do
        node.obj.try &.accept self
      end

      field "method" do
        if operator? node.name
          print_node("operator")
        else
          print_node("identifier")
        end
      end

      field "params" do
        if node.args.size > 0
          in_node("param_list") do
            node.args.each &.accept self
          end
        end
      end
    end
    false
  end

  def visit(node : ClassDef)
    in_node("class_def") do
      field "visibility" do
        case node.visibility
        in .public?
        in .protected? then print_node("protected")
        in .private?   then print_node("private")
        end
      end

      # TODO
    end

    false
  end

  def visit(node : Block)
    enter_node("block")
    # TODO
    true
  end

  def end_visit(node : Block)
    exit_node
  end

  ##############
  # Control flow
  ##############

  def visit(node : If)
    in_node("conditional") do
      field "cond" do
        node.cond.accept self
      end

      field "then" do
        node.then.accept self
      end

      field "else" do
        node.else.accept self
      end
    end

    false
  end

  def visit(node : Return)
    in_node("return") do
      if (exp = node.exp)
        in_node("argument_list") do
          exp.accept self
        end
      end
    end

    false
  end

  #######
  # Types
  #######

  def visit(node : Alias)
    in_node "alias" do
      field "visibility" do
        if node.visibility.private?
          print_node("private")
        end
      end

      field "name" do
        node.name.accept self
      end

      field "type" do
        node.value.accept self
      end
    end

    false
  end

  def visit(node : ProcNotation)
    in_node("proc_type") do
      node.inputs.try(&.each(&.accept self))

      field "return" do
        node.output.try(&.accept self)
      end
    end

    false
  end

  def visit(node : Union)
    enter_node("union_type")
    true
  end

  def end_visit(node : Union)
    exit_node
  end

  def visit(node : Generic)
    # `a : {Foo}` is represented in Crystal's syntax tree as `a : ::Tuple(Foo)`
    if node.name.to_s == "::Tuple"
      in_node("tuple_type") do
        node.type_vars.each(&.accept self)
      end

      return false
    end

    if node.name.to_s == "::NamedTupleType"
      in_node("named_tuple_type") do
        node.named_args.try &.each do |arg|
          in_node("named_type") do
            field "name" { print_node("identifier") }
            field "constant" { arg.value.accept self }
          end
        end
      end

      return false
    end

    if node.suffix.asterisk?
      in_node("pointer_type") do
        node.type_vars.each(&.accept self)
      end

      return false
    end

    in_node("generic_instance_type") do
      node.name.accept self

      field "params" do
        in_node("param_list") do
          node.type_vars.each(&.accept self)
          # TODO: named args
        end
      end
    end

    false
  end

  ##############
  # Method calls
  ##############
  OPERATOR_TOKENS = [
    "+",
    "-",
    "*",
    "/",
    "//",
    "%",
    "&",
    "|",
    "^",
    "**",
    ">>",
    "<<",
    "==",
    "!=",
    "<",
    "<=",
    ">",
    ">=",
    "<=>",
    "===",
    "[]",
    "[]?",
    "[]=",
    "!",
    "~",
    "!~",
    "=~",
    "&+",
    "&-",
    "&*",
    "&**",
  ]

  def operator?(name : String)
    OPERATOR_TOKENS.includes? name
  end

  def visit(node : Call)
    named_args = node.named_args

    ident_match = /[0-9A-Za-z_\x{00a0}-\x{10ffff}]/

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
      print_node("identifier")

      return false
    end

    if node.name == "`"
      # this is a `command` literal
      in_node("command") do
        node.args.each do |arg|
          case arg
          when StringLiteral
            next
          when StringInterpolation
            arg.expressions.each do |interp|
              next if interp.is_a?(StringLiteral)

              in_node("interpolation") do
                interp.accept self
              end
            end
          else
            raise "unexpected node #{arg.class} #{arg}"
          end
        end
      end

      return false
    end

    unary_tokens = [
      "~",
      "+",
      "-",
      "&+",
      "&-",
    ]

    named_args_size = named_args.try(&.size) || 0
    has_args = (node.args.size + named_args_size) > 0

    is_operator = operator? node.name

    is_unary_operator = unary_tokens.includes?(node.name) &&
                        node.obj &&
                        !has_args &&
                        named_args_size == 0 &&
                        !(node.block_arg) &&
                        !(node.block)

    if is_unary_operator && (obj = node.obj)
      in_node("call") do
        field "method" { print_node("operator") }
        field "receiver" { obj.accept self }
      end

      return false
    end

    in_node("call") do
      field "receiver" do
        node.obj.try &.accept(self)
      end

      field "method" do
        if is_operator
          print_node("operator")
        else
          print_node("identifier")
        end
      end

      field "arguments" do
        break if !has_args

        in_node("argument_list") do
          node.args.each(&.accept self)

          if named_args && named_args.size > 0
            # TODO
            print_node("<todo>")
          end
        end
      end

      field "block" do
        node.block_arg.try &.accept(self)
        node.block.try &.accept(self)
      end
    end

    false
  end

  def visit(node : BinaryOp)
    node_type = case node
                in Or       then "or"
                in And      then "and"
                in BinaryOp then raise "not possible"
                end

    in_node(node_type) do
      node.left.accept self
      print_node("operator")
      node.right.accept self
    end

    false
  end

  def visit(node : Assign)
    in_node("assign") do
      field "lhs" do
        node.target.accept(self)
      end
      field "rhs" do
        node.value.accept(self)
      end
    end

    false
  end

  def visit(node : MultiAssign)
    in_node("assign") do
      field "lhs" do
        node.targets.each &.accept(self)
      end
      field "rhs" do
        node.values.each &.accept(self)
      end
    end

    false
  end

  def visit(node : TypeDeclaration)
    in_node("type_declaration") do
      field "var" do
        node.var.accept self
      end

      field "type" do
        node.declared_type.accept self
      end

      field "value" do
        node.value.try &.accept self
      end
    end

    false
  end

  ##########
  # Fallback
  ##########

  def visit(node)
    raise NotImplementedError.new(node.class)
  end
end
