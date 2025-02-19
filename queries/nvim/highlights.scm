[
  "alias"
  "annotation"
  "asm"
  "begin"
  "break"
  "case"
  "do"
  "end"
  "ensure"
  "extend"
  "in"
  "include"
  "next"
  "select"
  "then"
  "verbatim"
  "when"
] @keyword

[
  "def"
  "fun"
  "macro"
] @keyword.function

[
  "class"
  "enum"
  "lib"
  "module"
  "struct"
  "type"
  "union"
] @keyword.type

"require" @keyword.import

[
  "return"
  "yield"
] @keyword.return

[
  "if"
  "else"
  "elsif"
  "unless"
] @keyword.conditional

(conditional
  [
    "?"
    ":"
  ] @keyword.conditional.ternary)

[
  "for"
  "until"
  "while"
] @keyword.repeat

"rescue" @keyword.exception

[
  (private)
  (protected)
  "abstract"
] @keyword.modifier

(pseudo_constant) @constant.builtin

; literals
(string
  "\"" @string)

(string
  (literal_content) @string)

(string
  (escape_sequence) @string.escape)

(symbol
  [
   ":"
   ":\""
   "\""
  ] @string.special.symbol)

(symbol
  (literal_content) @string.special.symbol)

(symbol
  (escape_sequence) @character)

(command
  "`" @string.special)

(command
  (literal_content) @string.special)

(command
  (escape_sequence) @character)

(regex
  "/" @punctuation.bracket)

(regex
  (literal_content) @string.regexp)

(regex_modifier) @character.special

(heredoc_body
  (literal_content) @string)

(heredoc_body
  (escape_sequence) @string.escape)

[
  (heredoc_start)
  (heredoc_end)
] @label

(char
  "'" @character)

(char
  (literal_content) @character)

(char
  (escape_sequence) @string.escape)

(integer) @number

(float) @number.float

[
  (true)
  (false)
] @boolean

(nil) @constant.builtin

(comment) @comment

; Operators and punctuation
[
  "="
  "=>"
  "->"
] @operator

[
  ","
  ";"
  "."
] @punctuation.delimiter

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

([
  "{{"
  "}}"
] @punctuation.bracket
  ; Set priority so "a{{b}}" is highlighted as brackets, not string content
  ;                   ^^
  (#set! priority 105))

(index_call
  method: (operator) @punctuation.bracket
  [
    "]"
    "]?"
  ] @punctuation.bracket)

[
  "{%"
  "%}"
] @tag.delimiter

(interpolation
  "#{" @punctuation.special
  "}" @punctuation.special)

; Types
[
  (constant)
  (generic_instance_type)
  (generic_type)
] @type

(nilable_constant
  "?" @type.builtin)

(nilable_type
  "?" @type.builtin)

(annotation
  (constant) @attribute)

(method_def
  name: [
    (identifier)
    (constant)
  ] @function.method)

(macro_def
  name: [
    (identifier)
    (constant)
  ] @function.method)

(param
  name: (identifier) @variable.parameter)

(macro_var
  name: (identifier) @variable)

[
  (class_var)
  (instance_var)
] @variable.member

(underscore) @variable.parameter.builtin

(pointer_type
  "*" @operator)

; function calls
(call
  method: (identifier) @function.call)
