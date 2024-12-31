const
  const_start = /[A-Z]/,
  ident_start = /[a-z_\u{00a0}-\u{10ffff}]/u,
  ident_part = /[0-9A-Za-z_\u{00a0}-\u{10ffff}]/u

// All the operators that can used as unquoted symbols or in dot calls.
// A few operators like '&&', '||', and '..' are not included here.
const operator_tokens = [
  '+',
  '-',
  '*',
  '/',
  '//',
  '%',
  '&',
  '|',
  '^',
  '**',
  '>>',
  '<<',
  '==',
  '!=',
  '<',
  '<=',
  '>',
  '>=',
  '<=>',
  '===',
  '[]',
  '[]?',
  '[]=',
  '!',
  '~',
  '!~',
  '=~',
  '&+',
  '&-',
  '&*',
  '&**',
]

module.exports = grammar({
  name: 'crystal',

  externals: $ => [
    $._line_break,
    $._line_continuation,

    $._start_of_brace_block,
    $._start_of_hash_or_tuple,
    $._start_of_named_tuple,
    $._start_of_tuple_type,
    $._start_of_named_tuple_type,

    $._start_of_index_operator,

    // To help the parser deal with a state like
    //   with foo yield yield
    // we insert an invisible token just before the first yield, which signals
    // the end of the "with" expression.
    $._end_of_with_expression,

    $.unary_plus,
    $.unary_minus,
    $.binary_plus,
    $.binary_minus,

    $.unary_wrapping_plus,
    $.unary_wrapping_minus,
    $.binary_wrapping_plus,
    $.binary_wrapping_minus,

    $._unary_star,
    $._binary_star,
    $._unary_double_star,
    $._binary_double_star,

    $._block_ampersand,
    $.binary_ampersand,

    $._beginless_range_operator,

    $._regex_start,
    $._binary_slash,
    $._binary_double_slash,

    $._regular_if_keyword,
    $._modifier_if_keyword,

    $._regular_unless_keyword,
    $._modifier_unless_keyword,

    $._regular_rescue_keyword,
    $._modifier_rescue_keyword,

    $._regular_ensure_keyword,
    $._modifier_ensure_keyword,

    $._modulo_operator,

    $._string_literal_start,
    $._delimited_string_contents,
    $._string_literal_end,

    $._string_percent_literal_start,
    $._command_percent_literal_start,
    $._string_array_percent_literal_start,
    $._symbol_array_percent_literal_start,
    $._regex_percent_literal_start,
    $._percent_literal_end,
    $._delimited_array_element_start,
    $._delimited_array_element_end,

    $.heredoc_start,
    $._heredoc_body_start,
    $.heredoc_content,
    $.heredoc_end,

    $.regex_modifier,

    $._macro_control_start,
    $._macro_control_end,
    $._macro_expression_start,
    $._macro_expression_end,

    // These symbols are never actually returned. They signal the current scope
    // to the scanner.
    $._start_of_parenless_args,
    $._end_of_range,

    // This symbol is not used in the grammar. It signals to the scanner when
    // error recovery mode is active.
    $._error_recovery,
  ],

  extras: $ => [
    /\s/,
    $._line_continuation,
    $.comment,
    $.heredoc_body,
  ],

  word: $ => $.identifier,

  precedences: $ => [
    // Operator precedence, as defined by
    // https://crystal-lang.org/reference/1.4/syntax_and_semantics/operators.html#operator-precedence
    [
      'index_operator',
      'dot_operator',
      'unary_operator',
      'exponential_operator',
      'multiplicative_operator',
      'additive_operator',
      'shift_operator',
      'binary_and_operator',
      'binary_or_operator',
      'equality_operator',
      'comparison_operator',
      'logical_and_operator',
      'logical_or_operator',
      'range_operator',
      'ternary_operator',
      'block_ampersand',
      'assignment_operator',
      'splat_operator',
      $.named_expr,
      'comma',
    ],

    // Operator precedence for the type grammar
    // Most type operators bind tighter than splat or proc types
    [
      'atomic_type',
      'splat_type',
    ],
    [
      'atomic_type',
      'proc_type',
    ],
    // Ensure most type parsing happens before $._type is upgraded to $._splattable_type
    [
      'atomic_type',
      $._splattable_type,
    ],

    // Ensure `*a = b` parses as `(*a) = b`, when encountered as a standalone statement
    [
      $.lhs_splat,
      'assignment_operator',
    ],

    // Ensure `a b { 1 }` parses as `a(b { 1 })`
    [
      'brace_block_call',
      $._expression,
    ],

    // Ensure `a b do 1 end` parses as `a(b) do 1 end`
    [
      $._expression,
      'do_end_block_call',
    ],

    // Ensure `a b() { 1 }` parses as `a(b() { 1 })`
    [
      'brace_block_call',
      'no_block_call',
    ],

    // Ensure `a b() do 1 end` parses as `a(b()) do 1 end`
    [
      'no_block_call',
      'do_end_block_call',
    ],

    // Ensure `a b &c` parses as `a b(&c)`
    [
      'ampersand_block_call',
      $._expression,
    ],

    // Ensure `a b! &c` parses as `a b!(&c)`
    [
      'ampersand_block_call',
      'no_block_call',
    ],

    // Ensure `[] of A | B` parses as `[] of (A | B)`
    [
      $.union_type,
      $.array,
    ],

    // Ensure `[ [] of Int32, Int32 -> String ]` resolves to ` [ [] of (Int32, Int32 -> String) ]`
    [
      $.proc_type,
      $.array,
    ],

    // Ensure `{} of K => A | B` parses as `{} of K => (A | B)`
    [
      $.union_type,
      $.hash,
    ],

    // Ensure `foo ? a : Int32` parses as `foo ? (a) : Int32` instead of a type declaration
    [
      $._expression,
      $.type_declaration,
    ],
    // Ensure `fun foo() end` parses `()` as empty params instead of a nil body.
    [
      $.top_level_fun_def,
      $.empty_parens,
    ],

    // Ensure `private macro(args).method` parses with `private` applied to the macro.
    // Note: this precedence is only needed to satisfy tree-sitter. In real Crystal code, there's
    // no ambiguity. A visibility keyword may only precede a single macro call, not a call chain.
    [
      'call_visibility',
      $._expression,
    ],

    // Ensure `a &.b.c` is parsed as `a(&.b.c)` instead of `a(&.b).c`
    [
      $.implicit_object_call_chainable,
      $.implicit_object_call_unchainable,
      'block_ampersand',
    ],

    // Ensure implicit object calls chain before the wrapper rule is applied
    [
      $.implicit_object_call_chainable,
      $._implicit_object_call,
    ],
  ],

  conflicts: $ => [
    // When the parser is in this state:
    //   { {} of A => B,
    //                 ^
    // we need to consider both of these interpretations as legitimate:
    //   { {} of A => B, 77 }
    // and
    //   { {} of A => B, C -> D }
    // (which is equivalent to)
    //   { {} of A => Proc(B, C, D) }
    [
      $._bare_type, $.proc_type,
    ],

    // When the parser is in this state:
    //   -> : ( A ,
    //            ^
    // we need to consider each of these interpretations as legitimate:
    //   -> : ( A ,) -> do ... end
    //   -> : ( A , B -> ) do ... end
    //   -> : ( A , B ) -> do ... end
    [
      $._bare_type, $.proc_type, $.parenthesized_proc_type,
    ],

    // When the parser is in this state:
    //   alias A = Foo(Bar,
    //                     ^
    // we need to consider each of these interpretations as legitimate:
    //   alias A = Foo(Bar,)
    //   alias A = Foo(Bar, Baz -> Qux)
    //   alias A = Foo(Bar, Baz)
    [
      $._bare_type, $.proc_type, $.type_instance_param_list,
    ],

    // Determining when to end a proc type is very complex. For instance, when
    // the parser is in this state:
    //   def foo : Int32 -> (
    //                      ^
    // we need to consider both of these interpretations as legitimate:
    //   def foo : Int32 -> ( Int32 )
    //     Proc(Int32, Int32).new{7}
    //   end
    // and:
    //   def foo : Int32 -> ( "a"; Proc(Int32, Nil).new{}) end
    // The external scanner contains some logic to make this decision (see
    // `lookahead_start_of_type`) but some parsing decisions don't invoke the
    // scanner. The way tree-sitter resolves this conflict may not exactly
    // match the behavior of Crystal's parser, but hopefully it's good enough.
    [
      $.proc_type,
    ],

    // Similar conflicts for the other proc type variations
    [
      $.no_args_proc_type,
    ],
    [
      $.parenthesized_proc_type,
    ],
  ],

  reserved: $ => [
    'abstract',
    'alias',
    'annotation',
    'asm',
    'begin',
    'break',
    'case',
    'class',
    'def',
    'end',
    'enum',
    'extend',
    'false',
    'fun',
    'include',
    'instance_sizeof',
    'lib',
    'macro',
    'module',
    'next',
    'nil',
    'offsetof',
    'private',
    'protected',
    'require',
    'return',
    'select',
    'sizeof',
    'struct',
    'true',
    'typeof',
    'until',
    'while',
    'with',
    'yield',

  ],

  rules: {
    expressions: $ => seq(
      optional($._statements),
    ),

    _terminator: $ => choice($._line_break, ';'),

    _statements: $ => choice(
      seq(
        repeat1(
          choice(
            seq($._statement, $._terminator),
            prec(-1, ';'),
          ),
        ),
        optional($._statement),
      ),
      $._statement,
    ),

    _lib_statements: $ => choice(
      seq(
        repeat1(
          choice(
            seq($._lib_statement, $._terminator),
            prec(-1, ';'),
          ),
        ),
        optional($._lib_statement),
      ),
      $._lib_statement,
    ),

    _enum_statements: $ => choice(
      seq(
        repeat1(
          choice(
            seq($._enum_statement, $._terminator),
            prec(-1, ';'),
          ),
        ),
        optional($._enum_statement),
      ),
      $._enum_statement,
    ),

    _statement: $ => choice(
      $._expression,
      $.const_assign,
      alias($.multi_assign, $.assign),
      $.annotation,
      $.annotation_def,
      $.module_def,
      $.class_def,
      $.struct_def,
      $.enum_def,
      $.lib_def,
      $.alias,
      $.method_def,
      $.abstract_method_def,
      $.macro_def,
      alias($.top_level_fun_def, $.fun_def),
      $.require,
      $.modifier_if,
      $.modifier_unless,
      $.modifier_rescue,
      $.modifier_ensure,
      $.include,
      $.extend,

      $.return,
      $.next,
      $.break,
    ),

    _lib_statement: $ => choice(
      $.macro_expression,
      $._macro_statements,
      $.alias,
      $.fun_def,
      $.type_def,
      $.c_struct_def,
      $.union_def,
      $.enum_def,
      $.global_var,
      $.const_assign,
      $.annotation,
    ),

    _enum_statement: $ => choice(
      $.macro_expression,
      $._macro_statements,
      $.constant,
      $.const_assign,
      $.method_def,
      $.macro_def,
      $.class_var,
      $.annotation,
    ),

    // Wrap multiple expressions/statements into a single node, if necessary
    parenthesized_expressions: $ => seq(
      '(', $._statements, ')',
    ),

    _expression: $ => choice(
      $.macro_expression,
      $._macro_statements,

      // Literals
      $.nil,
      $.true,
      $.false,
      $.integer,
      $.float,
      $.char,
      $.array,
      $.hash,
      $.string,
      $.chained_string,
      alias($.string_percent_literal, $.string),
      alias($.string_array_percent_literal, $.array),
      alias($.operator_symbol, $.symbol),
      alias($.unquoted_symbol, $.symbol),
      alias($.quoted_symbol, $.symbol),
      alias($.symbol_array_percent_literal, $.array),
      $.heredoc_start,
      $.range,
      alias($.beginless_range, $.range),
      $.tuple,
      $.named_tuple,
      $.proc,
      $.method_proc,
      $.command,
      alias($.command_percent_literal, $.command),
      $.regex,
      alias($.regex_percent_literal, $.regex),

      // Groupings
      alias($.empty_parens, $.nil),
      alias($.parenthesized_expressions, $.expressions),
      $.begin,

      // Symbols
      $.self,
      $.constant,
      $.generic_instance_type,
      $.pseudo_constant,
      $.special_variable,
      alias($.global_match_data_index, $.special_variable),
      $.identifier,
      $.instance_var,
      $.class_var,
      $.macro_var,
      $.type_declaration,

      // Control structures
      $.while,
      $.until,
      $.if,
      $.unless,
      $.conditional,
      $.case,
      alias($.exhaustive_case, $.case),
      $.select,
      // TODO
      // macro interpolation
      // macro if
      // macro for
      // macro verbatim
      // macro fresh variables

      // Methods
      $.call,
      alias($.call_with_visibility, $.call),

      alias($.additive_operator, $.call),
      alias($.unary_additive_operator, $.call),
      alias($.multiplicative_operator, $.call),
      alias($.exponential_operator, $.call),
      alias($.shift_operator, $.call),
      alias($.complement_operator, $.call),
      alias($.binary_and_operator, $.call),
      alias($.binary_or_operator, $.call),
      alias($.equality_operator, $.call),
      alias($.comparison_operator, $.call),
      alias($.index_operator, $.index_call),
      $.index_call,
      $.assign,
      alias($.operator_assign, $.op_assign),

      // Logical operators
      $.not,
      $.and,
      $.or,

      $.asm,

      // Keywords and special methods
      $.yield,
      $.typeof,
      $.sizeof,
      $.instance_sizeof,
      $.alignof,
      $.instance_alignof,
      $.offsetof,
      // TODO
      // super
      // previous_def
      // is_a?
      // nil?
      // responds_to?
      // as
      // as?
      // pointerof
      // uninitialized
    ),

    comment: $ => /#.*/,

    empty_parens: $ => seq('(', ')'),

    nil: $ => 'nil',

    true: $ => 'true',
    false: $ => 'false',

    integer: $ => {
      const binary_literal = seq('0b', repeat(/[01_]/))
      const octal_literal = seq('0o', repeat(/[0-7_]/))
      const hex_literal = seq('0x', repeat(/[0-9a-fA-F_]/))

      const decimal = seq(/[1-9]/, repeat(/[0-9_]/))
      const leading_zero_decimal = choice(
        '0',
        seq('0_', repeat(/[0-9_]/)),
      )

      const type_suffix = choice(
        'u8', 'u16', 'u32', 'u64', 'u128',
        'i8', 'i16', 'i32', 'i64', 'i128',
      )

      const numeric_component = seq(
        choice(
          binary_literal,
          octal_literal,
          hex_literal,
          leading_zero_decimal,
          decimal,
        ),
        optional(type_suffix),
      )

      const sign = choice(alias($.unary_minus, '-'), alias($.unary_plus, '+'))

      return choice(
        seq(sign, token.immediate(numeric_component)),
        token(numeric_component),
      )
    },

    float: $ => {
      const digit_or_underscore = /[0-9_]/

      const leading_non_zero_value = seq(/[1-9]/, repeat(digit_or_underscore))
      const leading_zero_value = choice(
        '0',
        seq('0_', repeat(/[0-9_]/)),
      )
      const leading_number = choice(
        leading_non_zero_value,
        leading_zero_value,
      )

      const decimal_and_trailing_value = seq(/\.[0-9]/, repeat(digit_or_underscore))

      const exponent = seq(/[eE]/, optional(seq(optional(/[+-]/), repeat1(digit_or_underscore))))

      const float_suffix = choice('f32', 'f64')

      // A float must contain at least one of {decimal point, exponent, suffix}
      const numeric_component = choice(
        seq(leading_number, decimal_and_trailing_value, optional(exponent), optional(float_suffix)),
        seq(leading_number, optional(decimal_and_trailing_value), exponent, optional(float_suffix)),
        seq(leading_number, optional(decimal_and_trailing_value), optional(exponent), float_suffix),
      )

      const sign = choice(alias($.unary_minus, '-'), alias($.unary_plus, '+'))

      return choice(
        seq(sign, token.immediate(numeric_component)),
        token(numeric_component),
      )
    },

    char: $ => seq(
      '\'',
      choice(
        token.immediate(prec(1, /[^\\]/)),
        $.char_escape_sequence,
      ),
      token.immediate('\''),
    ),

    char_escape_sequence: $ => {
      const char_unicode_escape = seq('u', choice(
        /[0-9a-fA-F]{4}/,
        /\{[0-9a-fA-F]{1,6}\}/,
      ))

      return token.immediate(seq('\\', choice(
        '0', '\\', '\'', 'a', 'b', 'e', 'f', 'n', 'r', 't', 'v', char_unicode_escape,
      )))
    },

    string: $ => seq(
      $._string_literal_start,
      repeat(choice(
        $._delimited_string_contents,
        $.string_escape_sequence,
        $.ignored_backslash,
        $.interpolation,
      )),
      $._string_literal_end,
    ),

    // Represents multiple strings joined by line continuations. $._line_continuation is
    // not actually included here because it doesn't play with `extras`, see
    // https://github.com/tree-sitter/tree-sitter/issues/1950
    chained_string: $ => seq($.string, repeat1($.string)),

    ignored_backslash: $ => {
      return token.immediate(seq('\\',
        /[^\n\\"abefnrtuvx0-7]/,
      ))
    },

    string_escape_sequence: $ => {
      const octal_escape = /[0-7]{1,3}/
      const hex_escape = seq('x', /[0-9a-fA-F]{2}/)

      const long_unicode_character = /[0-9a-fA-F]{1,6}/
      const unicode_escape = seq('u', choice(
        /[0-9a-fA-F]{4}/,
        seq('{', long_unicode_character, repeat(seq(' ', long_unicode_character)), '}'),
      ))

      return token.immediate(seq('\\', choice(
        '\n', '\\', '"', 'a', 'b', 'e', 'f', 'n', 'r', 't', 'v',
        octal_escape, hex_escape, unicode_escape,
      )))
    },

    interpolation: $ => seq(
      token(prec(1, '#{')), $._expression, '}',
    ),

    string_percent_literal: $ => seq(
      $._string_percent_literal_start,
      repeat(choice(
        $._delimited_string_contents,
        $.interpolation,
        $.string_escape_sequence,
        $.ignored_backslash,
      )),
      $._percent_literal_end,
    ),

    string_array_percent_literal: $ => seq(
      $._string_array_percent_literal_start,
      repeat(
        alias($.percent_literal_array_word, $.string),
      ),
      $._percent_literal_end,
    ),

    symbol_array_percent_literal: $ => seq(
      $._symbol_array_percent_literal_start,
      repeat(
        alias($.percent_literal_array_word, $.symbol),
      ),
      $._percent_literal_end,
    ),

    percent_literal_array_word: $ => seq(
      $._delimited_array_element_start,
      repeat(choice(
        $._delimited_string_contents,
        $.ignored_backslash,
        // Normally backslash + newline is considered a string escape sequence, but
        // it's valid here and represents a newline that doesn't break the word.
        alias(/\\\n/, $.ignored_backslash),
      )),
      $._delimited_array_element_end,
    ),

    heredoc_body: $ => seq(
      $._heredoc_body_start,
      repeat(choice(
        $.heredoc_content,
        $.interpolation,
        $.string_escape_sequence,
        $.ignored_backslash,
      )),
      $.heredoc_end,
    ),

    operator_symbol: $ => token(seq(
      ':',
      token.immediate(
        choice(...operator_tokens),
      ),
    )),

    unquoted_symbol: $ => token(seq(
      ':',
      token.immediate(
        seq(
          choice(ident_start, const_start),
          repeat(ident_part),
          optional(/[?!=]/),
        ),
      ),
    )),

    quoted_symbol: $ => seq(
      ':"',
      seq(
        repeat(choice(
          token.immediate(prec(1, /[^\\"]/)),
          $.string_escape_sequence,
          $.ignored_backslash,
        )),
        token.immediate('"'),
      ),
    ),

    command: $ => seq(
      '`',
      repeat(choice(
        token.immediate(prec(1, /[^\\`]/)),
        $.string_escape_sequence,
        $.ignored_backslash,
        $.interpolation,
      )),
      token.immediate('`'),
    ),

    command_percent_literal: $ => seq(
      $._command_percent_literal_start,
      repeat(choice(
        $._delimited_string_contents,
        $.interpolation,
        $.string_escape_sequence,
        $.ignored_backslash,
      )),
      $._percent_literal_end,
    ),

    regex: $ => seq(
      alias($._regex_start, '/'),
      repeat(choice(
        token.immediate(prec(1, /[^\\/]/)),
        $.regex_escape_sequence,
        $.regex_character_class,
        $.regex_special_match,
        $.interpolation,
      )),
      token.immediate('/'),
      optional($.regex_modifier),
    ),

    // TODO: handle rest of PCRE2 escape syntax:
    // https://www.pcre.org/current/doc/html/pcre2syntax.html
    // - other special characters
    // - full support for back-reference syntax
    // - groups
    // - ...

    regex_escape_sequence: $ => {
      // These are PCRE escape sequences
      const octal_escape = /[0-7]{1,3}/
      const long_octal_escape = seq('o{', repeat1(/[0-7]/), '}')

      const hex_escape = seq('x', /[0-9a-fA-F]{1,2}/)
      const long_hex_escape = seq('x{', repeat1(/[0-9a-fA-F]/), '}')

      const ctrl_escape = /c[\x20-\x7e]/

      const unicode_escape = seq('N{U+', repeat1(/[0-9a-fA-F]/), '}')

      return token.immediate(
        seq(
          '\\',
          choice(
            /[^a-zA-Z0-9]/, // non-alphanumeric characters can always be escaped
            /[aefnrt]/,
            octal_escape,
            long_octal_escape,
            hex_escape,
            long_hex_escape,
            ctrl_escape,
            unicode_escape,
          ),
        ),
      )
    },

    regex_character_class: $ => {
      const property_class = seq('p{', /[a-zA-Z _-]+/, '}')
      const not_property_class = seq('P{', /[a-zA-Z _-]+/, '}')
      return token.immediate(
        seq(
          '\\',
          choice(
            /[dDhHNRsSvVwWX]/,
            property_class,
            not_property_class,
          ),
        ),
      )
    },

    regex_special_match: $ => {
      return token.immediate(
        seq(
          '\\',
          /[AbBEgGkKQzZ]/,
        ),
      )
    },

    regex_percent_literal: $ => seq(
      $._regex_percent_literal_start,
      repeat(choice(
        $._delimited_string_contents,
        $.regex_escape_sequence,
        $.ignored_backslash,
        $.interpolation,
      )),
      $._percent_literal_end,
      optional($.regex_modifier),
    ),

    array: $ => {
      const of_type = field('of', seq('of', $._bare_type))

      return choice(
        seq(
          '[',
          $._expression,
          repeat(seq(',', $._expression)),
          optional(','),
          ']',
          optional(of_type),
        ),
        seq(
          '[',
          ']',
          of_type,
        ),
      )
    },

    hash: $ => {
      const of_type = seq(
        'of',
        field('of_key', $._bare_type),
        '=>',
        field('of_value', $._bare_type),
      )

      return choice(
        seq(
          alias($._start_of_hash_or_tuple, '{'),
          $.hash_entry,
          repeat(seq(',', $.hash_entry)),
          optional(','),
          '}',
          optional(of_type),
        ),
        seq(
          alias($._start_of_hash_or_tuple, '{'),
          '}',
          of_type,
        ),
      )
    },

    hash_entry: $ => seq($._expression, '=>', $._expression),

    tuple: $ => seq(
      alias($._start_of_hash_or_tuple, '{'),
      $._expression,
      repeat(seq(',', $._expression)),
      optional(','),
      '}',
    ),

    named_tuple: $ => seq(
      alias($._start_of_named_tuple, '{'),
      $.named_expr,
      repeat(seq(',', $.named_expr)),
      optional(','),
      '}',
    ),

    range: $ => {
      const begin = field('begin', $._expression)
      const range_op = field('operator', alias(choice('..', '...'), $.operator))
      const end = field('end', $._expression)

      return prec.left('range_operator', seq(
        begin,
        range_op,
        optional($._end_of_range),
        optional(end),
      ))
    },

    beginless_range: $ => {
      const range_op = field('operator', alias($._beginless_range_operator, $.operator))
      const end = field('end', $._expression)

      return prec.left('range_operator', seq(
        range_op,
        optional($._end_of_range),
        optional(end),
      ))
    },

    proc: $ => {
      const params = seq(
        '(',
        field('params', optional(alias($.proc_param_list, $.param_list))),
        ')',
      )
      const return_type = field('type', seq(/:\s/, $._bare_type))
      const block = field('block', choice(
        alias($.do_end_block, $.block),
        alias($.brace_block, $.block),
      ))

      // TODO: include syntax for capturing methods as procs

      return seq(
        '->',
        optional(params),
        optional(return_type),
        block,
      )
    },

    proc_param_list: $ => seq(
      $.param,
      repeat(seq(',', $.param)),
      optional(','),
    ),

    method_proc: $ => {
      const receiver = field('receiver', choice(
        $.identifier,
        $.instance_var,
        $.class_var,
        $.self,
        $.constant,
      ))

      const method = field('method', choice(
        $.identifier,
        alias($.identifier_method_call, $.identifier),
        alias($.identifier_assign, $.identifier),
        alias($._operator_token, $.operator),
      ))

      const plain_method = choice(
        $._global_method,
        field('method', choice(
          $.identifier,
          alias($.identifier_method_call, $.identifier),
          alias($.identifier_assign, $.identifier),
        )),
      )

      const param_types = seq(
        '(',
        field('params', alias($.type_instance_param_list, $.param_list)),
        ')',
      )

      return prec.right(seq(
        '->',
        choice(
          seq(receiver, '.', method),
          plain_method,
        ),
        optional(param_types),
      ))
    },

    annotation_def: $ => seq(
      'annotation',
      field('name', $.constant),
      repeat($._terminator),
      $._end,
    ),

    annotation: $ => {
      return seq(
        '@[',
        $.constant,

        optional(
          field('arguments',
            alias($.annotation_argument_list, $.argument_list),
          ),
        ),
        ']',
      )
    },

    // Same as argument_list_with_parens, except without `token.immediate`
    annotation_argument_list: $ => {
      const args = choice($._expression, $.splat, $.double_splat, $.named_expr)

      return seq(
        '(',
        optional(seq(
          args,
          repeat(seq(',', args)),
          optional(','),
        )),
        ')',
      )
    },

    module_def: $ => seq(
      optional(field('visibility', $.private)),
      'module',
      field('name', choice($.constant, $.generic_type)),
      field('body', seq(optional(alias($._statements, $.expressions)))),
      $._end,
    ),

    class_def: $ => seq(
      optional(field('visibility', $.private)),
      optional('abstract'),
      'class',
      field('name', choice($.constant, $.generic_type)),
      optional(seq(
        '<', field('superclass', choice($.constant, $.generic_instance_type)),
      )),
      field('body', seq(optional(alias($._statements, $.expressions)))),
      $._end,
    ),

    struct_def: $ => seq(
      optional(field('visibility', $.private)),
      optional('abstract'),
      'struct',
      field('name', choice($.constant, $.generic_type)),
      optional(seq(
        '<', field('superclass', choice($.constant, $.generic_instance_type)),
      )),
      field('body', seq(optional(alias($._statements, $.expressions)))),
      $._end,
    ),

    enum_def: $ => seq(
      optional(field('visibility', $.private)),
      'enum',
      field('name', alias($._constant_segment, $.constant)),
      optional(field('type', seq(/:\s/, $._bare_type))),
      field('body', seq(optional(alias($._enum_statements, $.expressions)))),
      $._end,
    ),

    lib_def: $ => seq(
      optional(field('visibility', $.private)),
      'lib',
      field('name', $.constant, $.generic_type),
      field('body', seq(optional(alias($._lib_statements, $.expressions)))),
      $._end,
    ),

    top_level_fun_def: $ => {
      const name = field('name', choice(
        $.identifier,
        alias($.identifier_method_call, $.identifier),
      ))
      const real_name = seq('=',
        field('real_name', choice($.identifier, $.constant, $.string)),
      )
      const params = seq(
        '(', optional(field('params', alias($.fun_param_list, $.param_list))), ')',
      )
      const return_type = field('type', seq(/[ \t]:\s/, $._bare_type))

      return seq(
        prec.right(seq(
          'fun',
          name,
          optional(real_name),
          optional(params),
          optional(return_type),
        )),
        field('body', seq(optional(alias($._statements, $.expressions)))),
        $._end,
      )
    },

    fun_def: $ => {
      const name = field('name', choice(
        $.identifier,
        alias($.identifier_method_call, $.identifier),
        $.constant,
      ))
      const real_name = seq('=',
        field('real_name', choice($.identifier, $.constant, $.string)),
      )
      const params = seq(
        '(',
        optional(field('params',
          alias(choice($.fun_param_list, $.fun_type_param_list), $.param_list),
        )),
        ')',
      )
      const return_type = field('type', seq(/[ \t]:\s/, $._bare_type))

      return seq(
        'fun',
        name,
        optional(real_name),
        optional(params),
        optional(return_type),
      )
    },

    fun_param_list: $ => {
      return seq(
        $.fun_param,
        repeat(seq(',', $.fun_param)),
        optional(seq(
          ',', optional('...'),
        )),
      )
    },

    fun_type_param_list: $ => {
      return seq(
        $._type,
        repeat(seq(',', $._type)),
        optional(seq(
          ',', optional('...'),
        )),
      )
    },

    fun_param: $ => {
      const name = field('name', choice($.identifier, $.constant))
      const type = field('type', seq(/[ \t]:\s/, $._bare_type))

      return seq(
        name,
        type,
      )
    },

    type_def: $ => seq(
      'type',
      $.constant,
      '=',
      $._bare_type,
    ),

    c_struct_def: $ => {
      const name = field('name', $.constant)

      return seq(
        'struct',
        name,
        $._c_struct_expressions,
        $._end,
      )
    },

    _c_struct_expressions: $ => choice(
      seq(
        repeat1(
          choice(
            seq($._c_struct_expression, $._terminator),
            prec(-1, ';'),
          ),
        ),
        optional($._c_struct_expression),
      ),
      $._c_struct_expression,
    ),

    _c_struct_expression: $ => choice(
      $.macro_expression,
      $._macro_statements,
      $.include,
      $.c_struct_fields,
    ),

    c_struct_fields: $ => {
      const names = seq($.identifier, repeat(seq(',', $.identifier)))

      return seq(
        names,
        /[ \t]:\s/,
        field('type', $._bare_type),
      )
    },

    union_def: $ => {
      const name = field('name', $.constant)

      return seq(
        'union',
        name,
        $._union_expressions,
        $._end,
      )
    },

    _union_expressions: $ => choice(
      seq(
        repeat1(
          choice(
            seq($._union_expression, $._terminator),
            prec(-1, ';'),
          ),
        ),
        optional($._union_expression),
      ),
      $._union_expression,
    ),

    _union_expression: $ => choice(
      $.include,
      $.union_fields,
    ),

    union_fields: $ => {
      const names = seq($.identifier, repeat(seq(',', $.identifier)))

      return seq(
        names,
        /[ \t]:\s/,
        field('type', $._bare_type),
      )
    },

    global_var: $ => {
      const name = seq('$', $.identifier)
      const real_name = field('real_name', choice($.identifier, $.constant))
      const return_type = field('type', seq(/[ \t]:\s/, $._bare_type))

      return seq(
        name,
        optional(seq('=', real_name)),
        return_type,
      )
    },

    _operator_token: $ => choice(...operator_tokens),

    _base_method_def: $ => {
      const klass = field('class', seq(
        choice($.constant, $.self),
        '.',
      ))
      const name = field('name', choice(
        $.identifier,
        alias($.identifier_method_call, $.identifier),
        alias($.identifier_assign, $.identifier),
        alias($._operator_token, $.operator),
      ))
      const params = seq('(', field('params', optional($.param_list)), ')')
      const return_type = field('type', seq(/[ \t]:\s/, $._bare_type))
      const forall = field('forall', $.forall)

      return prec.right(seq(
        'def',
        optional(klass),
        name,
        optional(params),
        optional(return_type),
        optional(forall),
      ))
    },

    method_def: $ => {
      const visibility = optional(
        field('visibility', choice($.private, $.protected)),
      )

      return seq(
        visibility,
        $._base_method_def,
        optional($._terminator),
        field('body', seq(optional(alias($._statements, $.expressions)))),
        optional($._rescue_else_ensure),
        $._end,
      )
    },

    abstract_method_def: $ => {
      const visibility = optional(
        field('visibility', choice($.private, $.protected)),
      )

      return prec.left(seq(
        visibility,
        'abstract',
        $._base_method_def,
        optional($._terminator),
      ))
    },

    macro_def: $ => {
      const visibility = optional(
        field('visibility', choice($.private, $.protected)),
      )
      const name = field('name', choice(
        $.identifier,
        alias($.identifier_method_call, $.identifier),
        alias($._operator_token, $.operator),
      ))
      const params = seq(
        '(',
        field('params', optional($.param_list)),
        ')',
      )

      return seq(
        visibility,
        prec.right(seq(
          'macro',
          name,
          optional(params),
        )),
        $._terminator,
        field('body', seq(optional(alias($._statements, $.expressions)))),
        $._end,
      )
    },

    include: $ => {
      return seq(
        'include',
        choice(
          $.constant,
          $.self,
          $.generic_instance_type,
        ),
      )
    },

    extend: $ => {
      return seq(
        'extend',
        choice(
          $.constant,
          $.self,
          $.generic_instance_type,
        ),
      )
    },

    forall: $ => {
      const constant = alias($._constant_segment, $.constant)

      return seq(
        'forall',
        constant,
        repeat(seq(',', constant)),
      )
    },

    param_list: $ => {
      // Splat and double splat params have restrictions on where they can appear in the parameter
      // list: https://crystal-lang.org/reference/1.4/syntax_and_semantics/default_values_named_arguments_splats_tuples_and_overloading.html
      // However, it's much simpler for the grammar to treat them interchangably. (Otherwise the
      // repeats and comma placement would be ugly.) We'll leave the rest up to the compiler.
      const param = choice($.param, $.splat_param, $.double_splat_param)

      return choice(
        seq(
          param,
          repeat(seq(',', param)),
          optional(seq(',', optional($.block_param))),
        ),
        $.block_param,
      )
    },

    param: $ => {
      const extern_name = field('extern_name', $.identifier)
      const name = field('name', choice($.identifier, $.instance_var, $.class_var))
      const type = field('type', seq(/[ \t]:\s/, $._bare_type))
      const default_value = field('default', seq('=', $._expression))

      return seq(
        repeat($.annotation),
        optional(extern_name),
        name,
        optional(type),
        optional(default_value),
      )
    },

    splat_param: $ => {
      const name = field('name', choice($.identifier, $.instance_var, $.class_var))
      const type = field('type', seq(/[ \t]:\s/, $._bare_type))

      return seq(
        repeat($.annotation),
        '*',
        optional(name),
        optional(type),
      )
    },

    double_splat_param: $ => {
      const name = field('name', choice($.identifier, $.instance_var, $.class_var))
      const type = field('type', seq(/[ \t]:\s/, $._bare_type))

      return seq(
        repeat($.annotation),
        '**',
        name,
        optional(type),
      )
    },

    block_param: $ => {
      const name = field('name', choice($.identifier, $.instance_var, $.class_var))
      const type = field('type', seq(/:\s/, $._bare_type))

      return seq(
        repeat($.annotation),
        '&',
        optional(name),
        optional(type),
      )
    },

    _control_expressions: $ => choice(
      alias($.argument_list_with_parens, $.argument_list),
      alias($.argument_list_no_parens, $.argument_list),
    ),

    return: $ => seq(
      'return',
      optional($._control_expressions),
    ),

    next: $ => seq(
      'next',
      optional($._control_expressions),
    ),

    break: $ => seq(
      'break',
      optional($._control_expressions),
    ),

    yield: $ => {
      const with_expr = field('with', $._expression)

      return seq(
        optional(seq(
          'with',
          with_expr,
          $._end_of_with_expression,
        )),
        'yield',
        optional($._control_expressions),
      )
    },

    typeof: $ => seq(
      'typeof',
      '(',
      $._expression,
      repeat(seq(',', $._expression)),
      optional(','),
      ')',
    ),

    sizeof: $ => seq(
      'sizeof',
      '(',
      $._bare_type,
      ')',
    ),

    instance_sizeof: $ => seq(
      'instance_sizeof',
      '(',
      $._bare_type,
      ')',
    ),

    alignof: $ => seq(
      'alignof',
      '(',
      $._bare_type,
      ')',
    ),

    instance_alignof: $ => seq(
      'instance_alignof',
      '(',
      $._bare_type,
      ')',
    ),

    offsetof: $ => seq(
      'offsetof',
      '(',
      $._bare_type,
      ',',
      choice(
        $.instance_var,
        $.integer,
      ),
      ')',
    ),

    _constant_segment: $ => token(seq(const_start, repeat(ident_part))),

    constant: $ => {
      return prec.right(seq(
        optional('::'),
        $._constant_segment,
        repeat(
          seq('::', $._constant_segment),
        ),
      ))
    },

    pseudo_constant: $ => choice(
      '__LINE__',
      '__END_LINE__',
      '__FILE__',
      '__DIR__',
    ),

    special_variable: $ => token(choice(
      '$?',
      '$~',
    )),

    global_match_data_index: $ => token(choice(
      /\$[0-9]+/,
      /\$[0-9]+\?/,
    )),

    identifier: $ => token(seq(ident_start, repeat(ident_part))),
    identifier_method_call: $ => token(seq(ident_start, repeat(ident_part), /[?!]/)),
    identifier_assign: $ => token(seq(ident_start, repeat(ident_part), /[=]/)),

    instance_var: $ => token(seq('@', ident_start, repeat(ident_part))),
    class_var: $ => token(seq('@@', ident_start, repeat(ident_part))),

    macro_var: $ => {
      const name = token(seq('%', ident_part, repeat(ident_part)))
      const exp = seq(
        token.immediate('{'),
        $._expression, repeat(seq(',', $._expression)),
        optional(','),
        '}',
      )

      return seq(
        field('name', alias(name, $.identifier)),
        optional(exp),
      )
    },

    self: $ => 'self',

    // Here is a rough definition of how the Crystal parser handles types, in
    // EBNF form. This is extracted from
    // https://github.com/crystal-lang/crystal/blob/master/src/compiler/crystal/syntax/parser.cr

    // bare_proc_type = proc_type | splat_type ;
    // proc_type = splat_type, { ",", splat_type }, "->", [ union_type ] ;
    // splat_type = "*", union_type | union_type ;
    // splat_arg_type = "*", type_arg | type_arg ;
    // union_type = atomic_suffix_type, { "|", atomic_suffix_type } ;
    // atomic_suffix_type = metaclass_type
    //                    | nilable_type
    //                    | pointer_type
    //                    | double_pointer_type
    //                    | static_array_type
    //                    | atomic_type ;
    // metaclass_type = atomic_type, ".class" ;
    // nilable_type = atomic_type, "?" ;
    // pointer_type = atomic_type, "*" ;
    // double_pointer_type = atomic_type, "**" ;
    // static_array_type = atomic_type, "[", type_arg, "]" ;
    // type_arg = number | sizeof | instance_sizeof | offsetof | union_type ;
    // atomic_type = "self"
    //             | "self?"
    //             | typeof
    //             | "_"
    //             | generic_type
    //             | tuple_type
    //             | named_tuple_type
    //             | proc_output_type
    //             | paren_type ;
    // proc_output_type = "->", union_type ;
    // paren_type = "(", bare_proc_type, ")" | paren_proc_type ;
    // paren_proc_type = "(", splat_type, { ",", splat_type },  [ "," ], ")", "->", [ union_type ] ;
    // generic_type = constant
    //              | constant, "(", ")"
    //              | constant, "(", bare_proc_type, ")"
    //              | constant, "(", splat_arg_type, { ",", splat_arg_type }, [ "," ], ")"
    //              | constant, "(", named_type_arg, { ",", named_type_arg }, [ "," ], ")" ;
    // tuple_type = "{", splat_type, { ",", splat_type }, [ "," ], "}" ;
    // named_type_arg = name, ":", bare_proc_type ;
    // named_tuple_type = "{", named_type_arg, { ",", named_type_arg }, [ "," ], "}" ;
    // fun_pointer = "->", identifier, "(", union_type, { ",", union_type }, [ "," ], ")" ;
    // typeof = "typeof", "(", expression, { "," expression }, ")" ;
    // sizeof = "sizeof", "(", bare_proc_type, ")" ;
    // instance_sizeof = "instance_sizeof", "(", bare_proc_type, ")" ;
    // offsetof = "offsetof", "(", bare_proc_type, ",", instance_var, ")"
    //          | "offsetof", "(", bare_proc_type, ",", number, ")" ;
    // is_a = "is_a?", "(", bare_proc_type, ")" |
    //      | "is_a?", union_type ;
    //
    // as = "as", "(", bare_proc_type, ")" |
    //    | "as", union_type ;
    // fun = "fun", name, "(", { union_type }, ")" ;
    // uninitialized = "uninitialized", bare_proc_type ;
    //
    // type_decl = name, ":", bare_proc_type


    // _bare_type is the main entry point for type parsing. The full type grammar is valid here.
    _bare_type: $ => choice(
      $.proc_type,
      $._splattable_type,
    ),

    // _splattable_type contains all types except the full proc type. It is
    // used in places where type splats are valid, but the full proc notation
    // is not. For example, inside tuple types this is valid:
    //   alias Foo = {*SomeAlias}
    // but this is not:
    //   alias Foo = {A, B -> C}
    _splattable_type: $ => choice(
      $.splat_type,
      $._type,
    ),

    // _type contains the majority of the type grammar.
    _type: $ => choice(
      $._parenthesized_type,
      $.constant,
      $.generic_instance_type,
      $.union_type,
      $.tuple_type,
      $.named_tuple_type,
      alias($.no_args_proc_type, $.proc_type),
      alias($.parenthesized_proc_type, $.proc_type),
      $.class_type,
      $.underscore,
      $.nilable_type,
      $.pointer_type,
      $.self,
      $.typeof,
      $.static_array_type,
    ),

    // _numeric_type represents an expression in the type grammar that resolves
    // to a number. Mostly used for StaticArrays, e.g. UInt8[sizeof(String)].
    _numeric_type: $ => choice(
      $.integer,
      $.constant,
      $.sizeof,
      $.instance_sizeof,
      $.alignof,
      $.instance_alignof,
      $.offsetof,
    ),

    class_type: $ => prec('atomic_type', seq(
      $._type,
      '.',
      'class',
    )),

    union_type: $ => prec.right('atomic_type', seq(
      $._type,
      repeat1(prec.left(seq('|', $._type))),
    )),

    _parenthesized_type: $ => seq('(', $._bare_type, ')'),

    proc_type: $ => prec('proc_type', seq(
      $._splattable_type,
      repeat(seq(',', $._splattable_type)),
      '->',
      // use dynamic precedence so a valid return type is prefered
      optional(prec.dynamic(10, field('return', $._type))),
    )),

    no_args_proc_type: $ => prec('proc_type', seq(
      '->',
      optional(prec.dynamic(10, field('return', $._type))),
    )),

    parenthesized_proc_type: $ => prec('proc_type', seq(
      '(',
      choice(
        seq(
          $._splattable_type,
          repeat1(seq(',', $._splattable_type)),
          optional(','),
        ),
        seq(
          $._bare_type,
          ',',
        ),
      ),
      ')',
      '->',
      optional(prec.dynamic(10, field('return', $._type))),
    )),

    splat_type: $ => prec('splat_type', seq(
      '*', $._type,
    )),

    tuple_type: $ => seq(
      alias($._start_of_tuple_type, '{'),
      $._splattable_type,
      repeat(seq(',', $._splattable_type)),
      optional(','),
      '}',
    ),

    named_type: $ => {
      const name = field('name', choice(
        $.identifier,
        alias($._constant_segment, $.identifier),
        alias($.identifier_method_call, $.identifier),
        $.string,
        alias($.string_percent_literal, $.string),
      ))

      return seq(
        name,
        token.immediate(':'),
        $._bare_type,
      )
    },

    named_tuple_type: $ => seq(
      alias($._start_of_named_tuple_type, '{'),
      $.named_type,
      repeat(seq(',', $.named_type)),
      optional(','),
      '}',
    ),

    // `generic_type` is used for generic class/module/struct definitions, e.g.
    //   class Foo(T, U); end
    // `generic_instance_type` is used for instances of generic types, where
    // the parameters are filled in with concrete types, e.g.
    //   Foo(Int32, String).new
    generic_type: $ => seq(
      $.constant,
      token.immediate('('),
      field('params', optional(alias($.type_param_list, $.param_list))),
      ')',
    ),

    type_param_splat: $ => seq('*', $.constant),

    type_param_list: $ => {
      const type_param = choice(
        $.constant,
        alias($.type_param_splat, $.splat),
      )

      return seq(
        type_param,
        repeat(seq(',', type_param)),
        optional(','),
      )
    },

    generic_instance_type: $ => seq(
      $.constant,
      token.immediate('('),
      field('params', optional(alias($.type_instance_param_list, $.param_list))),
      ')',
    ),

    type_instance_param_list: $ => seq(
      choice(
        $._bare_type,
        seq(
          $._splattable_type,
          repeat1(seq(',', $._splattable_type)),
        ),
        seq(
          $.named_type,
          repeat(seq(',', $.named_type)),
        ),
      ),
      optional(','),
    ),

    underscore: $ => '_',

    nilable_type: $ => prec('atomic_type', seq($._type, '?')),

    pointer_type: $ => prec('atomic_type', seq($._type, '*')),

    static_array_type: $ => prec('atomic_type', seq($._type, '[', $._numeric_type, ']')),

    _dot_call: $ => {
      const receiver = field('receiver', $._expression)

      const method = field('method', choice(
        $.identifier,
        alias($.identifier_method_call, $.identifier),
        alias($._operator_token, $.operator),
        $.instance_var,
      ))

      return prec('dot_operator', seq(receiver, '.', method))
    },

    bracket_argument_list: $ => {
      const args = choice($._expression, $.splat, $.double_splat, $.named_expr)

      return seq(
        args,
        repeat(seq(',', args)),
        optional(','),
      )
    },

    private: $ => 'private',
    protected: $ => 'protected',

    macro_expression: $ => seq(
      alias($._macro_expression_start, '{{'),
      choice($.splat, $.double_splat, $._expression),
      alias($._macro_expression_end, '}}'),
    ),

    _macro_statements: $ => seq(
      alias($._macro_control_start, '{%'),
      choice(
        $._macro_statement,
        alias($._statements, $.macro_expressions),
      ),
      alias($._macro_control_end, '%}'),
    ),

    _macro_statement: $ => seq(choice(
      $.macro_for,
      $.macro_if,
      $.macro_unless,
      $.macro_begin,
      $.macro_verbatim,
      $.macro_elsif,
      $.macro_else,
      $.macro_end,
    )),

    macro_for: $ => {
      const var_name = field('var', choice($.underscore, $.identifier))

      return seq(
        'for',
        var_name,
        repeat(seq(',', var_name)),
        'in',
        field('cond', choice($._expression, $.splat, $.double_splat)),
      )
    },


    macro_if: $ => {
      const cond = field('cond', $._expression)

      return seq(
        alias($._regular_if_keyword, 'if'),
        cond,
      )
    },

    macro_unless: $ => {
      const cond = field('cond', $._expression)

      return seq(
        alias($._regular_unless_keyword, 'unless'),
        cond,
      )
    },

    macro_elsif: $ => {
      const cond = field('cond', $._expression)

      return seq(
        'elsif',
        cond,
      )
    },

    macro_else: $ => 'else',

    macro_end: $ => 'end',

    macro_begin: $ => 'begin',

    macro_verbatim: $ => seq('verbatim', 'do'),

    // Represents a macro call prefixed with private/protected, e.g.
    //   private getter foo : String
    // This rule is separated from `call` so the `call_visibility` precedence can be
    // applied in a targeted way.
    call_with_visibility: $ => prec('call_visibility',
      seq(
        field('visibility', choice($.private, $.protected)),
        alias($.call, 'call without visibility'),
      ),
    ),

    // how do we distingush a method call from a variable?
    // at least one of these is required:
    // - receiver
    // - ends in ? or !
    // - parentheses
    // - arguments
    // - block arg
    call: $ => {
      const receiver_call = choice(
        $._dot_call,
        field('method', alias($.identifier_method_call, $.identifier)),
        $._global_method,
      )
      const ambiguous_call = field('method', $.identifier)

      const argument_list = field('arguments', choice(
        alias($.argument_list_with_parens, $.argument_list),
        alias($.argument_list_no_parens, $.argument_list),
      ))

      const argument_list_with_block = field('arguments', choice(
        alias($.argument_list_with_parens_and_block, $.argument_list),
        alias($.argument_list_no_parens_with_block, $.argument_list),
      ))

      const brace_block = field('block', alias($.brace_block, $.block))
      const do_end_block = field('block', alias($.do_end_block, $.block))

      return choice(
        prec('no_block_call', seq(receiver_call, optional(argument_list))),
        prec('no_block_call', seq(ambiguous_call, argument_list)),

        prec('brace_block_call',
          seq(receiver_call, optional(argument_list), brace_block),
        ),
        prec('brace_block_call',
          seq(ambiguous_call, optional(argument_list), brace_block),
        ),

        prec('do_end_block_call',
          seq(receiver_call, optional(argument_list), do_end_block),
        ),
        prec('do_end_block_call',
          seq(ambiguous_call, optional(argument_list), do_end_block),
        ),

        prec('ampersand_block_call',
          seq(receiver_call, argument_list_with_block),
        ),
        prec('ampersand_block_call',
          seq(ambiguous_call, argument_list_with_block),
        ),
      )
    },

    _global_method: $ => {
      const method = field('method', choice(
        $.identifier,
        alias($.identifier_method_call, $.identifier),
        alias($.identifier_assign, $.identifier),
      ))

      return seq('::', method)
    },

    implicit_object_method_identifier: $ => token(seq(
      '.',
      repeat(/\s/),
      ident_start,
      repeat(ident_part),
      optional(/[?!]/),
    )),

    implicit_object_method_operator: $ => token(seq(
      '.',
      repeat(/\s/),
      choice(...operator_tokens),
    )),

    implicit_object_ivar: $ => token(seq(
      '.',
      repeat(/\s/),
      '@',
      ident_start,
      repeat(ident_part),
    )),

    implicit_object_index_operator: $ => {
      const args = field('arguments', alias($.bracket_argument_list, $.argument_list))

      return seq(
        '.',
        alias($._start_of_index_operator, '['),
        args,
        choice(']', ']?'),
      )
    },

    // The primary rule for implicit objects
    _implicit_object_call: $ => alias(
      choice(
        $.implicit_object_call_chainable,
        $.implicit_object_call_unchainable,
      ),
      $.implicit_object_call,
    ),

    // An implicit object call that may not be chained
    // E.g. `&.foo 5`
    implicit_object_call_unchainable: $ => {
      const chained_receiver = field('receiver',
        alias($.implicit_object_call_chainable, $.implicit_object_call),
      )

      const method_name = field('method', choice(
        alias($.implicit_object_method_identifier, $.identifier),
        alias($.implicit_object_method_operator, $.operator),
      ))

      const argument_list = field('arguments',
        alias($.argument_list_no_parens, $.argument_list),
      )

      const argument_list_with_block = field('arguments',
        alias($.argument_list_no_parens_with_block, $.argument_list),
      )

      return seq(
        optional(chained_receiver),
        choice(
          seq(method_name, argument_list),
          seq(method_name, argument_list_with_block),
        ),
      )
    },

    // An implicit object call that could be immediately followed by another implicit object call
    // E.g. `&.foo(5)`
    implicit_object_call_chainable: $ => {
      const chained_receiver = field('receiver',
        alias($.implicit_object_call_chainable, $.implicit_object_call),
      )

      const method_name = field('method', choice(
        alias($.implicit_object_method_identifier, $.identifier),
        alias($.implicit_object_method_operator, $.operator),
      ))

      const argument_list = field('arguments', choice(
        alias($.argument_list_with_parens, $.argument_list),
        alias($.argument_list_no_parens, $.argument_list),
      ))

      const argument_list_with_block = field('arguments',
        alias($.argument_list_with_parens_and_block, $.argument_list),
      )

      const brace_block = field('block', alias($.brace_block, $.block))

      const do_end_block = field('block', alias($.do_end_block, $.block))

      return seq(
        optional(chained_receiver),
        choice(
          prec.right(seq(method_name, optional(
            field('arguments', alias($.argument_list_with_parens, $.argument_list)),
          ))),
          seq(method_name, optional(argument_list), brace_block),
          seq(method_name, optional(argument_list), do_end_block),
          seq(method_name, argument_list_with_block),
          alias($.implicit_object_ivar, $.instance_var),
          alias($.implicit_object_index_operator, $.index_call),
        ),
      )
    },

    implicit_object_tuple: $ => seq(
      alias($._start_of_hash_or_tuple, '{'),
      optional(seq(
        $._expression,
        repeat(seq(',', $._expression)),
        ',',
      )),
      choice($._implicit_object_call, $.underscore),
      repeat(seq(',', choice($._expression, $._implicit_object_call, $.underscore))),
      optional(','),
      '}',
    ),

    // A subset of method calls that can be the LHS of an assignment
    assign_call: $ => {
      const receiver = field('receiver', $._expression)
      const method_identifier = field('method', $.identifier)

      return prec('dot_operator', seq(receiver, '.', method_identifier))
    },

    index_operator: $ => {
      const receiver = field('receiver', $._expression)
      const args = field('arguments', alias($.bracket_argument_list, $.argument_list))

      return prec('index_operator', seq(
        receiver,
        alias($._start_of_index_operator, '['),
        args,
        choice(']', ']?'),
      ))
    },

    index_call: $ => {
      return seq(
        field('receiver', $._expression),
        '.',
        field('method', alias('[', $.operator)),
        alias($.bracket_argument_list, $.argument_list),
        choice(']', ']?'),
      )
    },

    not: $ => prec('unary_operator', seq(
      alias('!', $.operator),
      $._expression,
    )),
    and: $ => prec.left('logical_and_operator', seq(
      $._expression,
      alias('&&', $.operator),
      $._expression,
    )),
    or: $ => prec.left('logical_or_operator', seq(
      $._expression,
      alias('||', $.operator),
      $._expression,
    )),

    additive_operator: $ => {
      const operator = choice(
        $.binary_plus,
        $.binary_minus,
        $.binary_wrapping_plus,
        $.binary_wrapping_minus,
      )

      const receiver = field('receiver', $._expression)
      const method = field('method', alias(operator, $.operator))
      const arg = field('arguments', alias($._expression, $.argument_list))

      return prec.left('additive_operator',
        seq(receiver, method, arg),
      )
    },

    unary_additive_operator: $ => {
      const operator = choice(
        $.unary_plus,
        $.unary_minus,
        $.unary_wrapping_plus,
        $.unary_wrapping_minus,
      )

      return prec('unary_operator', seq(
        field('method', alias(operator, $.operator)),
        field('receiver', $._expression),
      ))
    },

    multiplicative_operator: $ => {
      const operator = choice(
        $._binary_star,
        '&*',
        $._binary_slash,
        $._binary_double_slash,
        $._modulo_operator,
      )

      const receiver = field('receiver', $._expression)
      const method = field('method', alias(operator, $.operator))
      const arg = field('arguments', alias($._expression, $.argument_list))

      return prec.left('multiplicative_operator', seq(
        receiver, method, arg,
      ))
    },

    exponential_operator: $ => {
      const operator = choice($._binary_double_star, '&**')

      const receiver = field('receiver', $._expression)
      const method = field('method', alias(operator, $.operator))
      const arg = field('arguments', alias($._expression, $.argument_list))

      return prec.right('exponential_operator', seq(
        receiver, method, arg,
      ))
    },

    shift_operator: $ => {
      const operator = choice('<<', '>>')

      const receiver = field('receiver', $._expression)
      const method = field('method', alias(operator, $.operator))
      const arg = field('arguments', alias($._expression, $.argument_list))

      return prec.left('shift_operator', seq(
        receiver, method, arg,
      ))
    },

    complement_operator: $ => {
      const operator = '~'

      return prec('unary_operator', seq(
        field('method', alias(operator, $.operator)),
        field('receiver', $._expression),
      ))
    },

    binary_and_operator: $ => {
      const operator = $.binary_ampersand
      const receiver = field('receiver', $._expression)
      const method = field('method', alias(operator, $.operator))
      const arg = field('arguments', alias($._expression, $.argument_list))

      return prec.left('binary_and_operator', seq(
        receiver, method, arg,
      ))
    },

    binary_or_operator: $ => {
      const operator = choice('|', '^')
      const receiver = field('receiver', $._expression)
      const method = field('method', alias(operator, $.operator))
      const arg = field('arguments', alias($._expression, $.argument_list))

      return prec.left('binary_or_operator', seq(
        receiver, method, arg,
      ))
    },

    equality_operator: $ => {
      const operator = choice('==', '!=', '=~', '!~', '===')
      const receiver = field('receiver', $._expression)
      const method = field('method', alias(operator, $.operator))
      const arg = field('arguments', alias($._expression, $.argument_list))

      return prec.left('equality_operator', seq(
        receiver, method, arg,
      ))
    },

    comparison_operator: $ => {
      const operator = choice('<', '<=', '>', '>=', '<=>')
      const receiver = field('receiver', $._expression)
      const method = field('method', alias(operator, $.operator))
      const arg = field('arguments', alias($._expression, $.argument_list))

      return prec.left('comparison_operator', seq(
        receiver, method, arg,
      ))
    },

    splat: $ => prec('splat_operator', seq(alias($._unary_star, '*'), $._expression)),

    double_splat: $ => prec('splat_operator', seq(
      alias($._unary_double_star, '**'),
      $._expression,
    )),

    named_expr: $ => {
      const name = field('name', choice(
        $.identifier,
        alias($._constant_segment, $.identifier),
        alias($.identifier_method_call, $.identifier),
        $.string,
        alias($.string_percent_literal, $.string),
      ))

      return seq(
        name,
        token.immediate(':'),
        $._expression,
      )
    },

    argument_list_no_parens: $ => {
      const args = choice($._expression, $.splat, $.double_splat, $.named_expr)

      return prec.right(seq(
        optional($._start_of_parenless_args),
        args,
        repeat(prec('comma', seq(',', args))),
      ))
    },

    argument_list_no_parens_with_block: $ => {
      const args = choice($._expression, $.splat, $.double_splat, $.named_expr)

      return prec.right(seq(
        optional($._start_of_parenless_args),
        optional(seq(
          args,
          repeat(prec('comma', seq(',', args))),
          ',',
        )),
        $.block_argument,
      ))
    },

    argument_list_with_parens: $ => {
      const args = choice($._expression, $.splat, $.double_splat, $.named_expr)

      return prec.right(seq(
        token.immediate('('),
        optional(seq(
          args,
          repeat(seq(',', args)),
          optional(','),
        )),
        ')',
      ))
    },

    argument_list_with_parens_and_block: $ => {
      const args = choice($._expression, $.splat, $.double_splat, $.named_expr)

      return prec.right(seq(
        token.immediate('('),
        optional(seq(
          args,
          repeat(seq(',', args)),
          ',',
        )),
        $.block_argument,
        ')',
      ))
    },

    assign: $ => {
      const lhs = field('lhs', choice(
        $.underscore,
        $.identifier,
        $.instance_var,
        $.class_var,
        $.macro_var,
        $.assign_call,
        $.index_call,
        alias($.index_operator, $.index_call),
        $.special_variable,
      ))
      const rhs = field('rhs', $._expression)

      return prec('assignment_operator', seq(
        lhs, '=', rhs,
      ))
    },

    const_assign: $ => {
      const visibility = optional(
        field('visibility', $.private),
      )

      const lhs = field('lhs', $.constant)
      const rhs = field('rhs', $._statement)

      return prec.right('assignment_operator', seq(
        visibility, lhs, '=', rhs,
      ))
    },

    operator_assign: $ => {
      // https://crystal-lang.org/reference/1.5/syntax_and_semantics/operators.html#combined-assignments
      const combined_operators = [
        '+=',
        '&+=',
        '-=',
        '&-=',
        '*=',
        '&*=',
        '/=',
        '//=',
        '%=',
        '|=',
        '&=',
        '^=',
        '**=',
        '<<=',
        '>>=',
        '||=',
        '&&=',
      ]

      const lhs = field('lhs', choice(
        $.identifier,
        $.instance_var,
        $.class_var,
        $.macro_var,
        $.assign_call,
        $.index_call,
        alias($.index_operator, $.index_call),
      ))
      const rhs = field('rhs', $._expression)

      return prec('assignment_operator', seq(
        lhs, choice(...combined_operators), rhs,
      ))
    },

    lhs_splat: $ => seq('*', choice(
      $.underscore,
      $.identifier,
      $.instance_var,
      $.class_var,
      $.macro_var,
      $.assign_call,
      $.index_call,
      alias($.index_operator, $.index_call),
    )),

    multi_assign: $ => {
      const lhs_basic = choice(
        $.underscore,
        $.identifier,
        $.instance_var,
        $.class_var,
        $.macro_var,
        $.assign_call,
        $.index_call,
        alias($.index_operator, $.index_call),
      )
      const lhs_splat = field('lhs', alias($.lhs_splat, $.splat))
      const lhs = field('lhs', choice(lhs_basic, alias($.lhs_splat, $.splat)))
      const multi_lhs = seq(repeat1(seq(lhs, ',')), lhs)

      const rhs = field('rhs', $._expression)
      const multi_rhs = seq(repeat1(seq(rhs, ',')), rhs)

      return choice(
        seq(lhs_splat, '=', rhs),
        seq(multi_lhs, '=', rhs),
        seq(lhs_splat, '=', multi_rhs),
        seq(multi_lhs, '=', multi_rhs),
      )
    },

    type_declaration: $ => {
      const variable = field('var', choice($.identifier, $.instance_var, $.class_var, $.macro_var))
      const type = field('type', $._bare_type)
      const value = field('value', $._expression)

      return prec('assignment_operator', seq(
        variable,
        ':',
        token.immediate(/\s/),
        type,
        optional(seq(
          '=',
          value,
        )),
      ))
    },

    alias: $ => seq(
      optional(field('visibility', $.private)),
      'alias',
      field('name', $.constant),
      '=',
      field('type', $._bare_type),
    ),

    block_body_param: $ => field('name', $.identifier),

    block_body_splat_param: $ => seq(
      '*',
      field('name', $.identifier),
    ),

    _block_body_nested_param: $ => {
      const param = alias($.block_body_param, $.param)

      return seq(
        '(',
        param,
        repeat(seq(',', param)),
        optional(','),
        ')',
      )
    },

    block_param_list: $ => {
      const param = choice(
        alias($.block_body_param, $.param),
        alias($.block_body_splat_param, $.splat_param),
        $._block_body_nested_param,
      )

      return seq(
        param,
        repeat(seq(',', param)),
        optional(','),
      )
    },

    do_end_block: $ => {
      const params = seq('|', field('params', alias($.block_param_list, $.param_list)), '|')

      return seq(
        'do',
        optional(params),
        field('body', seq(optional(alias($._statements, $.expressions)))),
        optional($._rescue_else_ensure),
        $._end,
      )
    },

    brace_block: $ => {
      const params = seq('|', field('params', alias($.block_param_list, $.param_list)), '|')

      return seq(
        alias($._start_of_brace_block, '{'),
        optional(params),
        field('body', seq(optional(alias($._statements, $.expressions)))),
        '}',
      )
    },

    block_argument: $ => {
      return prec('block_ampersand', seq(
        alias($._block_ampersand, '&'),
        choice(
          $._expression,
          $._implicit_object_call,
        ),
      ))
    },

    begin: $ => seq(
      'begin',
      optional($._terminator),
      field('body', seq(optional(alias($._statements, $.expressions)))),
      optional($._rescue_else_ensure),
      $._end,
    ),

    _end: $ => 'end',

    rescue: $ => {
      const rescue_variable = field('variable', $.identifier)
      const rescue_type = field('type', $._bare_type)
      const rescue_body = field('body', seq(optional(alias($._statements, $.expressions))))

      return seq(
        alias($._regular_rescue_keyword, 'rescue'),
        optional(choice(
          seq(rescue_variable, ': ', rescue_type),
          rescue_variable,
          rescue_type,
        )),
        $._terminator,
        rescue_body,
      )
    },

    ensure: $ => seq(alias($._regular_ensure_keyword, 'ensure'), optional($._statements)),

    modifier_rescue: $ => seq(
      $._statement,
      alias($._modifier_rescue_keyword, 'rescue'),
      field('rescue', $._expression),
    ),

    modifier_ensure: $ => seq(
      $._statement,
      alias($._modifier_ensure_keyword, 'ensure'),
      field('ensure', $._expression),
    ),

    // A block modifier clause containing least one of `rescue`, `else`, or `ensure`.
    // Split to its own rule as a performance improvement.
    _rescue_else_ensure: $ => {
      return choice(
        seq(
          field('rescue', repeat1($.rescue)),
          field('else', optional($.else)),
          field('ensure', optional($.ensure)),
        ),
        seq(
          field('rescue', repeat($.rescue)),
          field('else', $.else),
          field('ensure', optional($.ensure)),
        ),
        seq(
          field('rescue', repeat($.rescue)),
          field('else', optional($.else)),
          field('ensure', $.ensure),
        ),
      )
    },


    while: $ => seq(
      'while',
      field('cond', $._expression),
      $._terminator,
      field('body', seq(optional(alias($._statements, $.expressions)))),
      $._end,
    ),

    until: $ => seq(
      'until',
      field('cond', $._expression),
      $._terminator,
      field('body', seq(optional(alias($._statements, $.expressions)))),
      $._end,
    ),

    if: $ => {
      const cond = field('cond', $._expression)
      const then = field('then', $.then)
      const else_ = field('else', choice($.elsif, $.else))

      return seq(
        alias($._regular_if_keyword, 'if'),
        cond,
        $._terminator,
        optional(then),
        optional(else_),
        $._end,
      )
    },

    unless: $ => {
      const cond = field('cond', $._expression)
      const then = field('then', $.then)
      const else_ = field('else', $.else)

      return seq(
        alias($._regular_unless_keyword, 'unless'),
        cond,
        $._terminator,
        optional(then),
        optional(else_),
        $._end,
      )
    },

    then: $ => seq($._statements),

    elsif: $ => {
      const cond = field('cond', $._expression)
      const then = field('then', $.then)
      const else_ = field('else', choice($.elsif, $.else))

      return seq(
        'elsif',
        cond,
        $._terminator,
        optional(then),
        optional(else_),
      )
    },

    else: $ => seq(
      'else',
      field('body', seq(optional(alias($._statements, $.expressions)))),
    ),

    conditional: $ => prec.right('ternary_operator', seq(
      field('cond', $._expression),
      '?',
      field('then', $._expression),
      ':',
      field('else', $._expression),
    )),

    modifier_if: $ => seq(
      field('then', $._statement),
      alias($._modifier_if_keyword, 'if'),
      field('cond', $._expression),
    ),

    modifier_unless: $ => seq(
      field('then', $._statement),
      alias($._modifier_unless_keyword, 'unless'),
      field('cond', $._expression),
    ),

    require: $ => seq('require', $.string),

    when: $ => {
      const cond = field('cond', choice(
        $._expression,
        $._implicit_object_call,
        alias($.implicit_object_tuple, $.tuple),
      ))

      return seq(
        'when',
        cond,
        repeat(seq(',', cond)),
        choice('then', $._terminator),
        field('body', seq(optional(alias($._statements, $.expressions)))),
      )
    },

    case: $ => {
      const cond = field('cond', $._expression)

      return seq(
        'case',
        optional(cond),
        repeat($.when),
        optional($.else),
        $._end,
      )
    },

    select: $ => {
      return seq(
        'select',
        repeat($.when),
        optional($.else),
        $._end,
      )
    },

    in: $ => {
      const cond = field('cond', choice(
        $.generic_instance_type,
        $.constant,
        $.true,
        $.false,
        $.nil,
        $._implicit_object_call,
      ))

      return seq(
        'in',
        cond,
        repeat(seq(',', cond)),
        choice('then', $._terminator),
        field('body', seq(optional(alias($._statements, $.expressions)))),
      )
    },

    exhaustive_case: $ => {
      const cond = field('cond', $._expression)

      return seq(
        'case',
        cond,
        repeat1($.in),
        $._end,
      )
    },

    asm: $ => seq(
      seq('asm', /\s*/, '('),
      field('text', $.string),
      optional($._asm_outputs),
      ')',
    ),

    // Each field is split to a separate rule to optimize tree-sitter parser size
    _asm_outputs: $ => seq(
      ':',
      optional(field('outputs', $.asm_operands)),
      optional($._asm_inputs),
    ),

    _asm_inputs: $ => seq(
      ':',
      optional(field('inputs', $.asm_operands)),
      optional($._asm_clobbers),
    ),

    _asm_clobbers: $ => seq(
      ':',
      optional(field('clobbers', $.asm_clobbers)),
      optional($._asm_options),
    ),

    _asm_options: $ => seq(
      ':',
      optional(field('options', $.asm_options)),
    ),

    asm_operands: $ => seq(
      $.asm_operand,
      repeat(seq(',', $.asm_operand)),
    ),

    asm_operand: $ => seq(
      field('constraint', $.string),
      '(',
      field('expression', $._expression),
      ')',
    ),

    asm_clobbers: $ => seq(
      $.string,
      repeat(seq(',', $.string)),
    ),

    asm_options: $ => seq(
      $.string,
      repeat(seq(',', $.string)),
    ),
  },
})
