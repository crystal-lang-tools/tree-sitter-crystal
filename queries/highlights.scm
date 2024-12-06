(identifier) @variable

((constant) @constant
 (#match? @constant "^[A-Z\\d_]+$"))

[
  "alias"
  "begin"
  "break"
  "case"
  "class"
  "def"
  "do"
  "end"
  "ensure"
  "in"
  "module"
  "next"
  "rescue"
  "then"
  "unless"
  "until"
  "when"
  "while"
] @keyword
["return" "yield"] @keyword.return
["if" "else" "elsif"] @keyword.conditional
["rescue"] @keyword.exception

(pseudo_constant) @constant.builtin

; literals
(string) @string

(symbol) @string.special.symbol

(regex) @string.regex

(string_escape_sequence) @escape

[(integer) (float)] @number

[(true) (false)] @boolean

(nil) @constant.builtin

(interpolation
  "#{" @punctuation.special
  "}" @punctuation.special) @embedded

(comment) @comment

; Operators

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

(class_def name: [(constant) (generic_type)] @type)

(module_def name: [(constant) (generic_type)] @module)

(struct_def name: [(constant) (generic_type)] @type)

(enum_def name: [(constant)] @type)

(method_def name: [(identifier) (constant)] @function.method)

(param name: [(identifier)] @variable.parameter)

[
  (class_var)
  (instance_var)
] @property
