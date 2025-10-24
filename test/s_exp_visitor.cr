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
      if node.keyword.begin?
        print_node("begin")
      else
        print_node("nil")
      end

      return false
    end

    if node.keyword.begin?
      in_node("begin") do
        field "body" do
          in_node("expressions") do
            node.expressions.each &.accept(self)
          end
        end
      end

      return false
    end

    in_node("expressions") do
      node.expressions.each &.accept(self)
    end

    false
  end

  def field(name : String, &)
    @fields << name
    yield
  ensure
    @fields.pop
  end

  def handle_exception_handler(node : ExceptionHandler)
    if node.suffix
      field "body" do
        in_node("expressions") do
          handle_exception_suffix(node)
        end
      end
      return
    end

    body_field(node.body)

    node.rescues.try &.each do |rescue_|
      field "rescue" do
        rescue_.accept(self)
      end
    end

    field "else" do
      if (else_ = node.else) && !else_.is_a?(Nop)
        in_node("else") do
          field "body" do
            if else_.is_a?(Expressions)
              else_.accept(self)
            else
              in_node("expressions") do
                else_.accept(self)
              end
            end
          end
        end
      elsif node.else_location
        print_node("else")
      end
    end

    field "ensure" do
      if (ensure_ = node.ensure) && !ensure_.is_a?(Nop)
        in_node("ensure") do
          field "body" do
            if ensure_.is_a?(Expressions)
              ensure_.accept(self)
            else
              in_node("expressions") do
                ensure_.accept(self)
              end
            end
          end
        end
      elsif node.ensure_location
        print_node("ensure")
      end
    end
  end

  def handle_exception_suffix(node : ExceptionHandler)
    case
    when (rescue_ = node.rescues.try(&.first))
      in_node("modifier_rescue") do
        if (body = node.body).is_a?(Expressions)
          body.expressions.each &.accept(self)
        else
          body.accept(self)
        end

        field "rescue" do
          rescue_.body.accept(self)
        end
      end
    when (ensure_ = node.ensure)
      in_node("modifier_ensure") do
        if (body = node.body).is_a?(Expressions)
          body.expressions.each &.accept(self)
        else
          body.accept(self)
        end

        field "ensure" do
          if ensure_.is_a?(Expressions)
            ensure_.expressions.each &.accept(self)
          else
            ensure_.accept(self)
          end
        end
      end
    else
      raise NotImplementedError.new(node.class)
    end
  end

  def body_field(body : ASTNode?)
    if body.is_a?(ExceptionHandler)
      handle_exception_handler(body)
    else
      field "body" do
        case body
        when Nop, nil
          break
        when Expressions
          if body.keyword.paren?
            # Make sure we output 2 levels of `expressions` with syntax like
            #   def foo; (1); end
            body = Expressions.new([body] of ASTNode)
          end
        when ASTNode
          body = Expressions.new([body] of ASTNode)
        else
          return
        end

        body.accept self
      end
    end
  end

  def with_visibility_modifier(node : ASTNode, &)
    raise "no visibility" unless node.responds_to?(:visibility)

    modifier = nil

    case node.visibility
    in .public?
    in .protected?
      modifier = "protected"
    in .private?
      modifier = "private"
    end

    if modifier
      in_node("visibility_modifier") do
        field "visibility" do
          print_node(modifier)
        end

        yield
      end
    else
      yield
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
      true
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

    if node.name == "_"
      print_node("underscore")
      return false
    end

    print_node("identifier")
    false
  end

  visit_basic(Self)
  visit_basic(ClassVar)
  visit_basic(InstanceVar)
  visit_basic(Underscore)

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
    in_node("char") do
      print_node("literal_content")
    end
    false
  end

  def visit(node : StringLiteral)
    in_node("string") do
      print_node("literal_content") unless node.value.empty?
    end

    false
  end

  def visit(node : StringInterpolation)
    in_node "string" do
      # We want to collapse adjacent StringLiteral pieces into a single node
      chunked_expressions = node.expressions.chunk_while do |piece1, piece2|
        piece1.is_a?(StringLiteral) && piece2.is_a?(StringLiteral)
      end

      chunked_expressions.each do |pieces|
        if pieces.first.is_a?(StringLiteral)
          print_node("literal_content")
          next
        end

        interp = pieces.first
        in_node("interpolation") { interp.accept self }
      end
    end

    false
  end

  def visit(node : SymbolLiteral)
    in_node("symbol") do
      print_node("literal_content") unless node.value.empty?
    end

    false
  end

  def visit(node : RegexLiteral)
    in_node("regex") do
      val = node.value
      case val
      when StringInterpolation
        # We want to collapse adjacent StringLiteral pieces into a single node
        chunked_expressions = val.expressions.chunk_while do |piece1, piece2|
          piece1.is_a?(StringLiteral) && piece2.is_a?(StringLiteral)
        end

        chunked_expressions.each do |pieces|
          if pieces.first.is_a?(StringLiteral)
            print_node("literal_content")
            next
          end

          interp = pieces.first
          in_node("interpolation") { interp.accept self }
        end
      when StringLiteral
        print_node("literal_content") unless val.value.empty?
      else
        raise "unknown regex node: #{val.class}"
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
    if node.name
      in_node("hash_like") do
        field "name" do
          node.name.try &.accept(self)
        end

        field "values" do
          in_node("hash") do
            node.entries.each do |entry|
              in_node("hash_entry") do
                entry.key.accept(self)
                entry.value.accept(self)
              end
            end
          end
        end
      end

      return false
    end

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
    if node.name
      in_node("array_like") do
        field "name" do
          node.name.try &.accept self
        end

        field "values" do
          in_node("tuple") do
            node.elements.each(&.accept(self))
          end
        end
      end

      return false
    end

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
    def_type = node.abstract? ? "abstract_method_def" : "method_def"

    with_visibility_modifier(node) do
      in_node(def_type) do
        field "class" do
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
          if node.args.size > 0 || node.double_splat
            in_node("param_list") do
              splat_index = node.splat_index || -1

              node.args.each_with_index do |arg, i|
                if i == splat_index
                  alias_next_node!("splat_param")
                end
                arg.accept self
              end

              if (double_splat = node.double_splat)
                alias_next_node!("double_splat_param")
                double_splat.accept(self)
              end
            end
          end
        end

        field "type" do
          node.return_type.try &.accept self
        end

        field "forall" do
          # TODO
        end

        body_field(node.body)
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
    node_type = if node.struct?
                  "struct_def"
                else
                  "class_def"
                end

    with_visibility_modifier(node) do
      in_node(node_type) do
        field "name" do
          if (type_vars = node.type_vars)
            in_node("generic_type") do
              node.name.accept self

              field "params" do
                in_node("param_list") do
                  splat_index = node.splat_index || -1

                  type_vars.each_with_index do |_type_var, i|
                    if i == splat_index
                      in_node("splat") { print_node("constant") }
                    else
                      print_node("constant")
                    end
                  end
                end
              end
            end
          else
            node.name.accept self
          end
        end

        field "superclass" { node.superclass.try(&.accept self) }

        body_field(node.body)
      end
    end

    false
  end

  def visit(node : ModuleDef)
    with_visibility_modifier(node) do
      in_node("module_def") do
        field "name" do
          node.name.accept self
        end

        body_field(node.body)
      end
    end

    false
  end

  def visit(node : LibDef)
    in_node("lib_def") do
      field "name" do
        node.name.accept self
      end

      body_field(node.body)
    end

    false
  end

  def visit(node : EnumDef)
    with_visibility_modifier(node) do
      in_node("enum_def") do
        field "name" do
          node.name.accept self
        end

        if (base_type = node.base_type)
          field "type" do
            base_type.accept self
          end
        end

        field "body" do
          in_node("expressions") do
            node.members.each do |member|
              case member
              when Arg
                if (default = member.default_value)
                  in_node("const_assign") do
                    field "lhs" do
                      print_node("constant")
                    end

                    field "rhs" do
                      default.accept(self)
                    end
                  end
                else
                  print_node("constant")
                end
              else
                member.accept(self)
              end
            end
          end
        end
      end
    end

    false
  end

  def visit(node : CStructOrUnionDef)
    node_type = if node.union?
                  "union_def"
                else
                  "c_struct_def"
                end

    with_visibility_modifier(node) do
      in_node(node_type) do
        field "name" do
          print_node("constant")
        end

        field "body" do
          if (body = node.body).is_a?(Expressions)
            in_node("expressions") do
              field_name = if node.union?
                             "union_fields"
                           else
                             "c_struct_fields"
                           end

              body.expressions.each do |member|
                case member
                when TypeDeclaration
                  in_node(field_name) do
                    field "name" do
                      print_node("identifier")
                    end

                    field "type" do
                      member.declared_type.accept(self)
                    end
                  end
                else
                  member.accept(self)
                end
              end
            end
          end
        end
      end
    end

    false
  end

  def unpack_block_params(unpack : Expressions)
    unpack.expressions.each do |exp|
      case exp
      when Var, Underscore
        in_node("param") do
          field "name" do
            exp.accept self
          end
        end
      when Splat
        in_node("splat_param") do
          field "name" do
            exp.exp.accept self
          end
        end
      when Expressions
        unpack_block_params(exp)
      else
        raise "unexpected unpack node #{exp.class} #{exp}"
      end
    end
  end

  def visit(node : Block)
    in_node("block") do
      field "params" do
        if node.args.size > 0
          in_node("param_list") do
            splat_index = node.splat_index || -1

            node.args.each_with_index do |arg, i|
              if arg.name.empty? && (unpack = node.unpacks.try &.[i])
                unpack_block_params(unpack)
                next
              end

              if i == splat_index
                alias_next_node!("splat_param")
              end
              in_node("param") do
                field "name" do
                  arg.accept self
                end
              end
            end
          end
        end
      end

      body_field(node.body)
    end

    false
  end

  def visit(node : FunDef)
    in_node("fun_def") do
      field "name" do
        print_node("identifier")
      end

      field "real_name" do
        next if node.real_name == node.name
        if node.real_name.starts_with?(/[A-Z]/)
          print_node("constant")
        else
          print_node("identifier")
        end
      end

      # TODO varargs

      field "params" do
        if node.args.size > 0
          in_node("param_list") do
            node.args.each_with_index do |arg, i|
              alias_next_node!("fun_param")
              arg.accept self
            end
          end
        end
      end

      field "type" do
        node.return_type.try &.accept self
      end

      body_field(node.body)
    end

    false
  end

  def visit(node : TypeDef)
    in_node("type_def") do
      field "name" do
        print_node("constant")
      end

      field "type" do
        node.type_spec.accept(self)
      end
    end

    false
  end

  ##############
  # Control flow
  ##############

  def visit(node : If | Unless)
    then_loc = node.then.location
    cond_loc = node.cond.location

    if then_loc && cond_loc && then_loc < cond_loc
      # then comes before cond, so this must be modifier form
      node_type = if node.is_a?(If)
                    "modifier_if"
                  else
                    "modifier_unless"
                  end

      in_node(node_type) do
        field "then" do
          node.then.accept self
        end

        field "cond" do
          node.cond.accept self
        end
      end

      return false
    end

    is_ternary = false
    node_type = if node.is_a?(If)
                  if node.ternary?
                    is_ternary = true
                    "conditional"
                  else
                    "if"
                  end
                else
                  "unless"
                end

    in_node(node_type) do
      field "cond" do
        node.cond.accept self
      end

      field "then" do
        then_ = node.then
        if !is_ternary && !(then_).is_a?(Nop)
          in_node("then") do
            if then_ && then_.is_a?(Expressions)
              then_.expressions.each &.accept(self)
            else
              then_.accept self
            end
          end
        else
          node.then.accept self
        end
      end

      field "else" do
        if is_ternary
          node.else.accept self
        else
          if node.else.is_a?(If) && node.else_location == node.else.location
            # `node.else_location == node.else.location` confirms this is an elsif keyword
            # If not, it must actually be `else if`
            alias_next_node!("elsif")
            node.else.accept(self)
          elsif node.else.is_a?(Nop)
            # We need to check else_location to distinguish between
            #   if foo
            #     5
            #   else
            #   end
            # and
            #   if foo
            #     5
            #   end
            # The former should have an empty `else` node.
            if node.else_location
              print_node("else")
            end
          else
            in_node("else") do
              body_field(node.else)
            end
          end
        end
      end
    end

    false
  end

  def visit(node : ControlExpression)
    in_node(node.class.to_s.split("::").last.underscore) do
      if (exp = node.exp)
        in_node("argument_list") do
          exp.accept self
        end
      end
    end

    false
  end

  def visit(node : While)
    in_node("while") do
      field "cond" do
        node.cond.accept self
      end

      body_field(node.body)
    end

    false
  end

  def visit(node : Until)
    in_node("until") do
      field "cond" do
        node.cond.accept self
      end

      body_field(node.body)
    end

    false
  end

  def visit(node : Case)
    in_node("case") do
      if (cond = node.cond)
        field "cond" do
          cond.accept self
        end
      end

      node.whens.each do |when_|
        when_.accept self
      end

      if (else_ = node.else)
        in_node("else") do
          next if else_.is_a?(Nop)

          field "body" do
            in_node("expressions") do
              else_.accept self
            end
          end
        end
      end
    end

    false
  end

  def visit(node : Select)
    in_node("select") do
      node.whens.each do |when_|
        when_.accept self
      end

      if (else_ = node.else)
        in_node("else") do
          next if else_.is_a?(Nop)

          field "body" do
            in_node("expressions") do
              else_.accept self
            end
          end
        end
      end
    end

    false
  end

  def visit(node : When)
    in_node(node.exhaustive? ? "in" : "when") do
      node.conds.each do |cond|
        field "cond" do
          cond.accept self
        end
      end

      body_field(node.body)
    end

    false
  end

  #######
  # Types
  #######

  def visit(node : Alias)
    with_visibility_modifier(node) do
      in_node "alias" do
        field "name" do
          node.name.accept self
        end

        field "type" do
          node.value.accept self
        end
      end

      false
    end
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
    # The type `Foo?` is expanded to `Union(Foo, ::Nil)`
    if node.types.size == 2 && node.types.last.to_s == "::Nil"
      in_node("nilable_type") do
        node.types.first.accept self
      end

      return false
    end

    in_node("union_type") do
      node.types.each &.accept(self)
    end

    false
  end

  def visit(node : Generic)
    # `a : {Foo}` is represented in Crystal's syntax tree as `a : ::Tuple(Foo)`
    if node.name.to_s == "::Tuple"
      in_node("tuple_type") do
        node.type_vars.each(&.accept self)
      end

      return false
    end

    if node.name.to_s == "::NamedTuple"
      in_node("named_tuple_type") do
        node.named_args.try &.each do |arg|
          in_node("named_type") do
            field "name" { print_node("identifier") }

            arg.value.accept self
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

    if node.suffix.bracket?
      in_node("static_array_type") do
        node.type_vars.each(&.accept self)
      end

      return false
    end

    # `a = Int32?` is represented as `a = ::Union(Int32, ::Nil)`
    if node.suffix.question?
      in_node("nilable_constant") do
        node.type_vars.first.accept(self)
      end

      return false
    end

    in_node("generic_instance_type") do
      node.name.accept self

      field "params" do
        break unless node.type_vars.size > 0 || node.named_args

        in_node("param_list") do
          node.type_vars.each(&.accept self)

          node.named_args.try &.each do |arg|
            in_node("named_type") do
              field "name" { print_node("identifier") }

              arg.value.accept self
            end
          end
        end
      end
    end

    false
  end

  def visit(node : Splat)
    in_node("splat_type") do
      node.exp.accept self
    end

    false
  end

  def visit(node : DoubleSplat)
    in_node("double_splat_type") do
      node.exp.accept self
    end

    false
  end

  def visit(node : TypeOf)
    enter_node("typeof")
    true
  end

  def end_visit(node : TypeOf)
    exit_node()
  end

  def visit(node : SizeOf)
    enter_node("sizeof")
    true
  end

  def end_visit(node : SizeOf)
    exit_node()
  end

  def visit(node : InstanceSizeOf)
    enter_node("instance_sizeof")
    true
  end

  def end_visit(node : InstanceSizeOf)
    exit_node()
  end

  def visit(node : OffsetOf)
    enter_node("offsetof")
    true
  end

  def end_visit(node : OffsetOf)
    exit_node()
  end

  def visit(node : Metaclass)
    enter_node("class_type")
    true
  end

  def end_visit(node : Metaclass)
    exit_node()
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

    if (obj = node.obj).is_a?(Global)
      visit(obj)
      return false
    end

    if node.name == "`"
      # this is a `command` literal
      in_node("command") do
        node.args.each do |arg|
          case arg
          when StringLiteral
            print_node("literal_content") unless arg.value.empty?
          when StringInterpolation
            # We want to collapse adjacent StringLiteral pieces into a single node
            chunked_expressions = arg.expressions.chunk_while do |piece1, piece2|
              piece1.is_a?(StringLiteral) && piece2.is_a?(StringLiteral)
            end

            chunked_expressions.each do |pieces|
              if pieces.first.is_a?(StringLiteral)
                print_node("literal_content")
                next
              end

              interp = pieces.first
              in_node("interpolation") { interp.accept self }
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

    # `foo.bar = baz` is represented as a call with method `bar=`. We should
    # output this as an assignment. (But `foo.bar <= baz` is not an assignment.)
    if node.name.ends_with?("=") && !is_operator && node.args.size == 1
      in_node("assign") do
        field "lhs" do
          in_node("call") do
            field "receiver" do
              node.obj.try &.accept(self)
            end

            field "method" do
              print_node("identifier")
            end
          end
        end

        field "rhs" do
          node.args[0].accept self
        end
      end

      return false
    end

    if node.name == "[]=" && node.args_in_brackets?
      in_node("assign") do
        field "lhs" do
          in_node("index_call") do
            field "receiver" do
              node.obj.try &.accept(self)
            end

            field "method" do
              print_node("operator")
            end

            field "arguments" do
              in_node("argument_list") do
                node.args[0..-2].each do |arg|
                  case arg
                  when Splat
                    alias_next_node!("splat")
                  when DoubleSplat
                    alias_next_node!("double_splat")
                  end

                  arg.accept(self)
                end

                if named_args && named_args.size > 0
                  named_args.each do |named_arg|
                    named_arg.accept(self)
                  end
                end
              end
            end
          end
        end

        field "rhs" do
          node.args[-1].accept self
        end
      end

      return false
    end

    call_name = if (obj = node.obj) && obj.is_a?(ImplicitObj)
                  "implicit_object_call"
                elsif (node.name == "[]" || node.name == "[]?") && (node.args_in_brackets? || node.name_size == 0)
                  "index_call"
                else
                  "call"
                end

    in_node(call_name) do
      field "receiver" do
        node.obj.try &.accept(self)
      end

      field "method" do
        if is_operator
          print_node("operator")
        elsif node.name.starts_with?(/[A-Z]/)
          print_node("constant")
        else
          print_node("identifier")
        end
      end

      field "arguments" do
        if !has_args
          print_node("argument_list") if node.has_parentheses?
          break
        end

        in_node("argument_list") do
          node.args.each do |arg|
            case arg
            when Splat
              alias_next_node!("splat")
            when DoubleSplat
              alias_next_node!("double_splat")
            end

            arg.accept(self)
          end

          if named_args && named_args.size > 0
            named_args.each do |named_arg|
              named_arg.accept(self)
            end
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

  def visit(node : NamedArgument)
    in_node("named_expr") do
      field "name" do
        print_node("identifier")
      end
      node.value.accept(self)
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
    assign_type = if node.target.is_a?(Crystal::Path)
                    "const_assign"
                  else
                    "assign"
                  end

    with_visibility_modifier(node) do
      in_node(assign_type) do
        field "lhs" do
          node.target.accept(self)
        end
        field "rhs" do
          node.value.accept(self)
        end
      end
    end

    false
  end

  def visit(node : OpAssign)
    in_node("op_assign") do
      field "lhs" do
        node.target.accept(self)
      end
      print_node("operator")
      field "rhs" do
        node.value.accept(self)
      end
    end
  end

  def visit(node : MultiAssign)
    in_node("assign") do
      field "lhs" do
        node.targets.each do |target|
          alias_next_node!("splat") if target.is_a?(Splat)
          target.accept(self)
        end
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

  def visit(node : IsA)
    in_node("call") do
      field "receiver" do
        node.obj.accept(self)
      end

      field "method" do
        print_node("identifier")
      end

      # `a.nil?` is transformed to `a.is_a?(::Nil)`
      # skip the argument list in this form
      next if node.const.to_s == "::Nil"

      field "arguments" do
        in_node("argument_list") do
          node.const.accept self
        end
      end
    end

    false
  end

  def visit(node : AnnotationDef)
    in_node("annotation_def") do
      field "name" do
        node.name.accept self
      end
    end

    false
  end

  def visit(node : Cast | NilableCast)
    in_node("call") do
      field "receiver" do
        node.obj.accept(self)
      end

      field "method" do
        print_node("identifier")
      end

      field "arguments" do
        in_node("argument_list") do
          node.to.accept self
        end
      end
    end

    false
  end

  def visit(node : RespondsTo)
    in_node("call") do
      field "receiver" do
        next if node.obj.is_a?(Self) ||
                ((var = node.obj).is_a?(Var) && var.name == "self")
        node.obj.accept(self)
      end

      field "method" do
        print_node("identifier")
      end

      field "arguments" do
        in_node("argument_list") do
          in_node("symbol") do
            print_node("literal_content") unless node.name.empty?
          end
        end
      end
    end

    false
  end

  visit_basic(Include)
  visit_basic(Extend)
  visit_basic(Yield)
  visit_basic(Out)

  def visit(node : Not)
    if node.location == node.exp.location
      # Not expression in call form, like `foo.!`
      in_node("call") do
        field "receiver" do
          node.exp.accept(self)
        end

        field "method" do
          print_node("operator")
        end
      end
    else
      # A regular not expression
      in_node("not") do
        print_node("operator")
        node.exp.accept(self)
      end
    end

    false
  end

  def visit(node : PointerOf)
    enter_node("pointerof")
    true
  end

  def end_visit(node : PointerOf)
    exit_node
  end

  def visit(node : ReadInstanceVar)
    in_node("call") do
      field "receiver" do
        node.obj.try &.accept(self)
      end

      field "method" do
        print_node("instance_var")
      end
    end

    false
  end

  def visit(node : Annotation)
    in_node("annotation") do
      node.path.accept self

      field "arguments" do
        if node.args.size == 1
          in_node("argument_list") do
            node.args.each &.accept(self)
            if (named_args = node.named_args)
              named_args.each &.accept(self)
            end
          end
        else
          node.args.each &.accept(self)
          if (named_args = node.named_args)
            named_args.each &.accept(self)
          end
        end
      end
    end

    false
  end

  def visit(node : MagicConstant)
    print_node("pseudo_constant")
    false
  end

  def visit(node : Global)
    print_node("special_variable")
    false
  end

  def visit(node : ExternalVar)
    in_node("global_var") do
      field "name" do
        print_node("identifier")
      end

      field "real_name" do
        next unless (real_name = node.real_name)
        if real_name.starts_with?(/[A-Z]/)
          print_node("constant")
        else
          print_node("identifier")
        end
      end

      field "type" do
        node.type_spec.accept(self)
      end
    end

    false
  end

  def visit(node : VisibilityModifier)
    in_node("visibility_modifier") do
      field "visibility" do
        case node.modifier
        in .public?
        in .protected? then print_node("protected")
        in .private?   then print_node("private")
        end
      end

      node.exp.accept(self)
    end

    false
  end

  def visit(node : UninitializedVar)
    in_node("assign") do
      field "lhs" do
        node.var.accept(self)
      end

      field "rhs" do
        in_node("uninitialized_var") do
          node.declared_type.accept(self)
        end
      end
    end

    false
  end

  def visit(node : ImplicitObj)
    true
  end

  def visit(node : ExceptionHandler)
    if node.suffix
      handle_exception_suffix(node)
    else
      in_node("begin") do
        handle_exception_handler(node)
      end
    end

    false
  end

  def visit(node : Rescue)
    in_node("rescue") do
      field "variable" do
        if node.name
          print_node("identifier")
        end
      end

      if (types = node.types)
        if types.size == 1
          field "type" do
            types.first.accept(self)
          end
        else
          field "type" do
            in_node("union_type") do
              types.each &.accept(self)
            end
          end
        end
      end

      body_field(node.body)
    end

    false
  end

  def visit(node : Require)
    in_node("require") do
      in_node("string") do
        print_node("literal_content")
      end
    end

    false
  end

  ########
  # Macros
  ########

  def visit(node : Macro)
    in_node("macro_def") do
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
          in_node("param_list") do
            splat_index = node.splat_index || -1

            node.args.each_with_index do |arg, i|
              if i == splat_index
                alias_next_node!("splat_param")
              end
              arg.accept self
            end
          end
        end
      end

      body_field(node.body)
    end

    false
  end

  def visit(node : MacroVar)
    in_node("macro_var") do
      field "name" do
        print_node "identifier"
      end

      node.exps.try &.each &.accept(self)
    end
    false
  end

  def visit(node : MacroLiteral)
    print_node("macro_content")
    false
  end

  def visit(node : MacroExpression)
    if node.output?
      in_node("macro_expression") do
        case node.exp
        when Splat
          alias_next_node!("splat")
        when DoubleSplat
          alias_next_node!("double_splat")
        end

        node.exp.accept self
      end

      return false
    end

    in_node("macro_statement") do
      body = node.exp

      if body.is_a?(Expressions)
        body.accept(self) unless body.empty?
      elsif body.is_a?(Nop)
        # no output
      else
        in_node("expressions") do
          body.accept(self)
        end
      end
    end

    false
  end

  def visit(node : MacroVerbatim)
    in_node("macro_verbatim") do
      field "body" do
        body = node.exp

        if body.is_a?(Expressions)
          body.accept(self) unless body.empty?
        else
          in_node("expressions") do
            body.accept(self)
          end
        end
      end
    end
  end

  def visit(node : MacroIf)
    if (bool = node.cond).is_a?(BoolLiteral) && bool.value == true && bool.location.nil?
      # this is a {% begin %} block, which the compiler represents as {% if true %}
      in_node("macro_begin") do
        field "body" do
          body = node.then

          if body.is_a?(Expressions)
            body.accept(self) unless body.empty?
          else
            in_node("expressions") do
              body.accept(self)
            end
          end
        end
      end

      return false
    end

    node_type = if node.is_unless?
                  "macro_unless"
                else
                  "macro_if"
                end

    in_node(node_type) do
      field "cond" do
        node.cond.accept(self)
      end

      field "then" do
        then_node = node.is_unless? ? node.else : node.then

        if then_node.is_a?(Expressions)
          then_node.accept(self) unless then_node.empty?
        elsif then_node.is_a?(Nop)
          # no output
        else
          in_node("expressions") do
            then_node.accept(self)
          end
        end
      end

      field "else" do
        else_node = node.is_unless? ? node.then : node.else

        if else_node.is_a?(MacroIf)
          alias_next_node!("macro_elsif")
          else_node.accept(self)
        elsif else_node.is_a?(Nop)
          # no output
        else
          in_node("macro_else") do
            field "body" do
              if else_node.is_a?(Expressions)
                else_node.accept(self) unless else_node.empty?
              else
                in_node("expressions") do
                  else_node.accept(self)
                end
              end
            end
          end
        end
      end
    end
  end

  def visit(node : MacroFor)
    in_node("macro_for") do
      field "var" do
        node.vars.each &.accept(self)
      end

      field "cond" do
        node.exp.accept(self)
      end

      field "body" do
        body = node.body

        if body.is_a?(Expressions)
          body.accept(self) unless body.empty?
        else
          in_node("expressions") do
            body.accept(self)
          end
        end
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
