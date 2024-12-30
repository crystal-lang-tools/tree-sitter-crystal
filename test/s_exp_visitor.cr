require "compiler/crystal/syntax"

# Visits a Crystal AST and outputs a parse tree in the same s-exp format used by tree-sitter
class SExpVisitor < Crystal::Visitor
  include Crystal

  @io = IO::Memory.new
  @indent = 0
  @fields = [] of String?

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
    @fields << nil
  end

  def exit_node
    @io << ")"
    @indent -= 1
    @fields.pop
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

  def body_field(body : ASTNode)
    field "body" do
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

  def active_field : String
    if @fields.empty? || @fields.last.nil?
      ""
    else
      "#{@fields.last}: "
    end
  end

  macro visit_basic(klass)
    def visit(node : {{ klass.id }})
      enter_node({{ klass.stringify.underscore }})
    end

    def end_visit(node : {{ klass.id }})
      exit_node
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
    enter_node("string")

    node.expressions.each do |interp|
      next if interp.is_a?(StringLiteral)

      enter_node("interpolation")
      interp.accept self
      exit_node
    end

    exit_node
    false
  end

  def visit(node : SymbolLiteral)
    print_node("symbol")
    false
  end

  def visit(node : RegexLiteral)
    enter_node("regex")

    if (val = node.value).is_a?(StringInterpolation)
      val.expressions.each do |interp|
        next if interp.is_a?(StringLiteral)

        enter_node("interpolation")
        interp.accept self
        exit_node
      end
    end

    exit_node
    false
  end

  ############
  # Structures
  ############

  def visit(node : RangeLiteral)
    enter_node("range")

    field "begin" do
      node.from.accept self
    end

    field "operator" do
      print_node("operator")
    end

    field "end" do
      node.to.accept self
    end

    exit_node
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
    enter_node("named_tuple")

    node.entries.each do |entry|
      enter_node("named_expr")

      field "name" do
        print_node("identifier")
      end

      entry.value.accept self

      exit_node
    end

    exit_node
    false
  end

  def visit(node : HashLiteral)
    enter_node("hash")

    node.entries.each do |entry|
      enter_node("hash_entry")

      entry.key.accept(self)
      entry.value.accept(self)

      exit_node
    end

    if (of = node.of)
      field "of_key" do
        of.key.accept(self)
      end

      field "of_value" do
        of.value.accept(self)
      end
    end

    exit_node
    false
  end

  def visit(node : ArrayLiteral)
    enter_node("array")

    node.elements.each(&.accept(self))

    field "of" do
      if ["::Symbol", "::String"].includes? node.of.to_s
        # %i and %w arrays inject a "of" node automatically, so ignore it
        next
      end

      node.of.try(&.accept(self))
    end

    exit_node
    false
  end

  #####################
  # Classes and methods
  #####################

  def visit(node : Def)
    enter_node("method_def")

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
        enter_node("param_list")
        node.args.each &.accept self
        exit_node
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

    exit_node
    false
  end

  def visit(node : Macro)
    enter_node("macro_def")

    field "visibility" do
      case node.visibility
      in .public?
      in .protected? then print_node("protected")
      in .private?   then print_node("private")
      end
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
        enter_node("param_list")
        node.args.each &.accept self
        exit_node
      end
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

    exit_node
    false
  end

  def visit(node : Arg)
    enter_node("param")

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

    exit_node
    false
  end

  def visit(node : ProcLiteral)
    enter_node("proc")

    field "params" do
      if node.def.args.size > 0
        enter_node("param_list")

        node.def.args.each &.accept self

        exit_node
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

      enter_node("block")

      field "body" do
        body.accept self
      end

      exit_node
    end

    exit_node
    false
  end

  def visit(node : ProcPointer)
    enter_node("method_proc")

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
        enter_node("param_list")

        node.args.each &.accept self

        exit_node
      end
    end

    exit_node
    false
  end

  def visit(node : ClassDef)
    enter_node("class_def")

    field "name" do
      node.name.accept self
    end

    body_field(node.body)

    exit_node
    false
  end

  def visit(node : ModuleDef)
    enter_node("module_def")

    field "name" do
      node.name.accept self
    end

    body_field(node.body)

    exit_node
    false
  end

  def visit(node : LibDef)
    enter_node("lib_def")

    field "name" do
      node.name.accept self
    end

    body_field(node.body)

    exit_node
    false
  end

  def visit(node : EnumDef)
    enter_node("enum_def")

    field "name" do
      node.name.accept self
    end

    if (base_type = node.base_type)
      field "type" do
        base_type.accept self
      end
    end

    field "body" do
      node.members.each &.accept self
    end

    exit_node
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

  def visit(node : If | Unless)
    enter_node("conditional")

    field "cond" do
      node.cond.accept self
    end

    field "then" do
      node.then.accept self
    end

    field "else" do
      node.else.accept self
    end

    exit_node
    false
  end

  def visit(node : While)
    enter_node("while")

    field "cond" do
      node.cond.accept self
    end

    body_field(node.body)

    exit_node
    false
  end

  def visit(node : Until)
    enter_node("until")

    field "cond" do
      node.cond.accept self
    end

    body_field(node.body)

    exit_node
    false
  end

  def visit(node : Case)
    enter_node("case")

    if (cond = node.cond)
      field "cond" do
        cond.accept self
      end
    end

    node.whens.each do |when_|
      field "when" do
        when_.accept self
      end
    end

    if (else_ = node.else)
      field "else" do
        else_.accept self
      end
    end

    exit_node
    false
  end

  def visit(node : When)
    enter_node(node.exhaustive? ? "in" : "when")

    node.conds.each do |cond|
      field "cond" do
        cond.accept self
      end
    end

    body_field(node.body)

    exit_node
    false
  end

  def visit(node : Return)
    enter_node("return")

    if (exp = node.exp)
      enter_node("argument_list")
      exp.accept self
      exit_node
    end

    exit_node
    false
  end

  ########
  # Macros
  ########

  def visit(node : MacroIf)
    enter_node("macro_if")

    field "cond" do
      node.cond.accept self
    end

    field "then" do
      node.then.accept self
    end

    field "else" do
      node.else.accept self
    end

    exit_node
    false
  end

  visit_basic(MacroLiteral)
  visit_basic(MacroExpression)

  #######
  # Types
  #######

  def visit(node : ProcNotation)
    enter_node("proc_type")

    node.inputs.try(&.each(&.accept self))

    field "return" do
      node.output.try(&.accept self)
    end

    exit_node
    false
  end

  visit_basic(Union)

  def visit(node : Generic)
    # `a : {Foo}` is represented in Crystal's syntax tree as `a : ::Tuple(Foo)`
    if node.name.to_s == "::Tuple"
      enter_node("tuple_type")
      node.type_vars.each(&.accept self)
      exit_node
      return false
    end

    if node.suffix.asterisk?
      enter_node("pointer_type")
      node.type_vars.each(&.accept self)
      exit_node
      return false
    end

    enter_node("generic_instance_type")

    node.name.accept self

    field "params" do
      enter_node("param_list")
      node.type_vars.each(&.accept self)
      # TODO: named args
      exit_node
    end

    exit_node
    false
  end

  visit_basic(Metaclass)

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
      enter_node("identifier")
      exit_node
      return false
    end

    if node.name == "`"
      # this is a `command` literal
      enter_node("command")

      node.args.each do |arg|
        case arg
        when StringLiteral
          next
        when StringInterpolation
          arg.expressions.each do |interp|
            next if interp.is_a?(StringLiteral)

            enter_node("interpolation")
            interp.accept self
            exit_node
          end
        else
          raise "unexpected node #{arg.class} #{arg}"
        end
      end

      exit_node
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

    enter_node("call")

    if is_unary_operator && (obj = node.obj)
      field "method" { print_node("operator") }
      field "receiver" { obj.accept self }

      exit_node
      return false
    end

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

      enter_node("argument_list")
      node.args.each(&.accept self)

      if named_args && named_args.size > 0
        # TODO
        print_node("<todo>")
      end
      exit_node
    end

    field "block" do
      node.block_arg.try &.accept(self)
      node.block.try &.accept(self)
    end

    exit_node
    false
  end

  def visit(node : BinaryOp)
    case node
    in Or       then enter_node("or")
    in And      then enter_node("and")
    in BinaryOp then raise "not possible"
    end

    node.left.accept self
    print_node("operator")
    node.right.accept self

    exit_node
    false
  end

  def visit(node : Assign | OpAssign)
    enter_node("assign")
    field "lhs" do
      node.target.accept(self)
    end
    field "rhs" do
      node.value.accept(self)
    end
    exit_node
    false
  end

  def visit(node : MultiAssign)
    enter_node("assign")
    field "lhs" do
      node.targets.each &.accept(self)
    end
    field "rhs" do
      node.values.each &.accept(self)
    end
    exit_node
    false
  end

  def visit(node : TypeDeclaration)
    enter_node("type_declaration")

    field "var" do
      node.var.accept self
    end

    field "type" do
      node.declared_type.accept self
    end

    field "value" do
      node.value.try &.accept self
    end

    exit_node
    false
  end

  def visit(node : IsA)
    enter_node("call")

    field "receiver" do
      node.obj.accept(self)
    end

    field "method" do
      print_node("identifier")
    end

    field "arguments" do
      enter_node("argument_list")
      node.const.accept self
      exit_node
    end

    exit_node
    false
  end

  def visit(node : TypeOf)
    enter_node("call")

    field "method" do
      print_node("identifier")
    end

    field "arguments" do
      enter_node("argument_list")
      node.expressions.each &.accept self
      exit_node
    end

    exit_node
    false
  end

  def visit(node : AnnotationDef)
    enter_node("annotation_def")

    field "name" do
      node.name.accept self
    end

    exit_node
    false
  end

  def visit(node : Alias)
    enter_node("alias")

    field "name" do
      node.name.accept self
    end

    field "type" do
      node.value.accept self
    end

    exit_node
    false
  end

  def visit(node : Cast | NilableCast)
    enter_node("call")

    field "receiver" do
      node.obj.accept(self)
    end

    field "method" do
      print_node("identifier")
    end

    field "arguments" do
      enter_node("argument_list")
      node.to.accept self
      exit_node
    end

    exit_node
    false
  end

  def visit(node : RespondsTo)
    enter_node("call")

    field "receiver" do
      node.obj.accept(self)
    end

    field "method" do
      print_node("identifier")
    end

    field "arguments" do
      enter_node("argument_list")
      print_node("symbol")
      exit_node
    end

    exit_node
    false
  end

  def visit(node : ReadInstanceVar)
    enter_node("call")

    field "receiver" do
      node.obj.accept(self)
    end

    field "method" do
      print_node("instance_var")
    end

    exit_node
    false
  end

  visit_basic(Include)
  visit_basic(Yield)
  visit_basic(Require)
  visit_basic(Not)
  visit_basic(Splat)
  visit_basic(DoubleSplat)

  # This should already be handled by the requisite nodes
  def visit(node : VisibilityModifier)
    true
  end

  def visit(node : Annotation)
    enter_node("annotation")

    node.path.accept self

    field "arguments" do
      node.args.each &.accept(self)
      if (named_args = node.named_args)
        named_args.each &.accept(self)
      end
    end

    exit_node
    false
  end

  def visit(node : ExceptionHandler)
    true
  end

  ##########
  # Fallback
  ##########

  def visit(node)
    raise NotImplementedError.new(node.class)
  end
end
